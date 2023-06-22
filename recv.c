/* Copyright (C) 2023 hyperfunny88
 * SPDX-License-Identifier: GPL-3.0-or-later */

#include "util.h"
#ifdef SP_SIO
#	include <soundio/soundio.h>
#endif
#ifdef SP_PW
#	include <pipewire/pipewire.h>
#	include <spa/param/audio/format-utils.h>
#	include <spa/utils/result.h>
#endif
#ifdef SP_WAS
#	include <audiopolicy.h>
#endif
#if defined(SP_SIO) || defined(SP_WAS)
#	include <speex/speex_resampler.h>
#	define USE_RESAMPLE
#endif

typedef struct S S;
typedef struct Inst Inst;

typedef struct {
	u32 rsqual;
} Pcfg;

struct Inst {
	Pcfg pcfg;
	void (*make)(Inst *inst, S *s);
	void (*dstr)(Inst *inst, S *s);
	void (*update)(Inst *inst, S *s);
};

typedef enum {
	PLAY_WAIT,
	PLAY_PLAY,
	PLAY_PAUSE,
	PLAY_TMP_PAUSE
} PlayState;

typedef struct {
	S *s;
	pthread_t thrd;
	pthread_mutex_t mtx;
	pthread_cond_t cnd;
#ifdef USE_RESAMPLE
	SpeexResamplerState *src;
#endif
	RingBuf rb;
	Cfg orig;
	bool stop;
} Resample;

struct S {
	void *p;
	char name[MAX_NAME];
	RingBuf rb;
	pthread_mutex_t mtx;
	Cfg cfg;
	u64 t;
	u32 idx, namesz, ctr, underrun, overrun, pauseheur, clipped, lastthres,
	    slack;
	f32 vols[MAX_CHANNELS];
	PlayState ps;
	struct sockaddr sa;
	bool fullymade, logged;
	Resample *rs;
};

static u32 toms(u32 bsz, Cfg cfg)
{
	return bsz / framesz(cfg) * 1000 / cfg.rate;
}

static u32 play(S *s, u32 nframes, u32 avgframes, void *out, bool fill)
{
	u8 buf[BUF_SZ];
	u32 fsz = framesz(s->cfg), /* TODO: put in lock */
	    need = nframes * fsz,
	    avgneed = avgframes * fsz;
	if (s->ps != PLAY_PLAY) {
		if (fill) {
			memset(out, 0, need);
			return need;
		} else
			return 0;
	}
	pthread_mutex_lock(&s->mtx);
	u32 readable = ringbuf_readable(&s->rb), read = 0,
	    base = s->cfg.bufsz * fsz,
	    slack = s->cfg.rate / 1000 * s->slack * fsz,
	    thres = base + MAX(slack, 2 * avgneed);
#ifdef SP_DEBUG
	i32 thresms = (i32)toms(thres, s->cfg),
	    lastthresms = (i32)toms(s->lastthres, s->cfg);
	if (abs(thresms -  lastthresms) >= 2)
		DEBUG("[%s]: drop thres: %u ms", s->name, toms(thres, s->cfg));
#endif
	s->lastthres = thres;
	if (readable > thres) {
		u32 leave = base + avgneed;
		read = ringbuf_read(
			&s->rb, buf, MIN(readable - leave, readable));
		WARN("[%s]: drop: %u ms - %u ms = %u ms (thres: %u ms)",
		     s->name, toms(readable, s->cfg),
		     toms(readable - leave, s->cfg), toms(leave, s->cfg),
		     toms(thres, s->cfg));
	} else
		read = ringbuf_read(&s->rb, buf, need);
	pthread_mutex_unlock(&s->mtx);
	if (read < need && fill) {
		if (s->pauseheur < 2000)
			WARN("[%s]: underrun (%u)", s->name, ++s->underrun);
		else
			s->ps = PLAY_TMP_PAUSE;
		s->pauseheur += 100;
	} else if (read > need) {
		read = need;
	} else
		s->pauseheur = (u32)MAX((i32)s->pauseheur - 1, 0);
	memcpy(out, buf, read);
	if (fill)
		memset((u8*)out + read, 0, need - read);
	return read;
}

static f32 kcvt(u32 bits)
{
	return 1.0f / (f32)(1u << (bits - 1));
}

/* maybe support big-endian properly ? */
static f32 loadf32(void *p) { return *(f32*)p; }
static f32 s16tof32(void *p) { return (f32)*(i16*)p * kcvt(16); }
static f32 s32tof32(void *p) { return (f32)*(i32*)p * kcvt(32); }
static f32 s24tof32(void *p)
{
	u8 *b = p;
	i32 v = (((i32)(b[2] & 0x7F) << 16) | (i32)b[1] << 8 | (i32)b[0])
	      - ((i32)(b[2] >> 7) << 23);
	return (f32)v * kcvt(24);
}

static void bitcvt(f32 *out, u8 *in, u32 bsz, u32 bytedepth)
{
	f32 (*fnt[])(void*) = {s16tof32, s24tof32, s32tof32},
	    (*load)(void*) = fnt[bytedepth - 2];
	for (u32 i = 0, j = 0; i < bsz; i += bytedepth, ++j)
		out[j] = load(in + i);
}

#ifdef USE_RESAMPLE
static void dstrrsreal(Resample *rs)
{
	speex_resampler_destroy(rs->src);
	free(rs);
}

static void dstrrs(Resample *rs)
{
	pthread_mutex_lock(&rs->mtx);
	rs->stop = true;
	pthread_mutex_unlock(&rs->mtx);
	pthread_cond_signal(&rs->cnd);
}

static void *rsproc(void *p)
{
	Resample *rs = p;
	while (!rs->stop) {
		u8 b[BUF_SZ];
		f32 tmp[BUF_SZ / sizeof(f32)], *f = tmp,
		    out[BUF_SZ / sizeof(f32)];
		pthread_mutex_lock(&rs->mtx);
		while (!rs->stop && !ringbuf_readable(&rs->rb))
			pthread_cond_wait(&rs->cnd, &rs->mtx);
		if (rs->stop)
			break;
		u32 read = ringbuf_read(&rs->rb, b, ~0u),
		    ifsz = framesz(rs->orig),
		    nframes = read / ifsz;
		pthread_mutex_unlock(&rs->mtx);
		if (!rs->orig.isfloat)
			bitcvt(f, b, read, rs->orig.bytes);
		else 
			f = (f32*)b;
		u32 nin = nframes,
		    ofsz = framesz(rs->s->cfg),
		    nout = sizeof(out) / ofsz;
		speex_resampler_process_interleaved_float(
			rs->src, f, &nin, out, &nout);
		if (nin != nframes)
			WARN("[%s]: resampled frames not equal", rs->s->name);
		pthread_mutex_lock(&rs->s->mtx);
		ringbuf_write(&rs->s->rb, out, nout * ofsz);
		if (rs->s->fullymade)
			rs->s->ps = PLAY_PLAY;
		pthread_mutex_unlock(&rs->s->mtx);
	}
	dstrrsreal(rs);
	return NULL;
}

static void makers(S *s, Inst *inst, u32 outrate)
{
	INFO("[%s]: input sample rate (%u) not equal to device sample rate "
	     "(%u), using resampler", s->name, s->cfg.rate, outrate);
	Resample *rs = s->rs = malloc(sizeof(*s->rs));
	*rs = (Resample){
		.s = s,
		.stop = false
	};
	pthread_mutex_init(&rs->mtx, NULL);
	pthread_cond_init(&rs->cnd, NULL);
	rs->orig = s->cfg;
	int err;
	rs->src = speex_resampler_init(s->cfg.channels, s->cfg.rate, outrate,
				       (int)inst->pcfg.rsqual, &err);
	CHK(!err, "[%s]: make resampler", s->name);
	s->cfg.rate = outrate;
	s->cfg.isfloat = true;
	s->cfg.bytes = 4;
	pthread_create(&rs->thrd, NULL, rsproc, rs);
}
#endif

static u32 strhash(const char *s)
{
	/* adler32 */
	u32 a = 1, b = 0;
	for (; *s; ++s) {
		a = (a + (u32)*s) % 65521;
		b = (b + a) % 65521;
	}
	return b << 16 | a;
}

/* libsoundio { */
#ifdef SP_SIO
typedef struct {
	struct SoundIoOutStream *os;
	u8 buf[BUF_SZ]; /* jack backend stack overflows so put it on heap */
	f32 avg;
	u32 n;
} SioParam;

typedef struct {
	Inst b;
	struct SoundIo *sio;
	struct SoundIoDevice *dev;
} SioInst;

static void siowrite(S *s, struct SoundIoChannelArea *ar, u8 *buf, u32 nfr,
		     u32 mch, u32 nch, u32 bytes, f32 (*cvt)(void*))
{
	bool hasclipped = false;
	f32 k = 1.0f / (f32)mch;
	for (u32 i = 0; i < nfr; ++i) {
		f32 avg = 0.0f;
		for (u32 ch = 0; ch < mch; ar[ch].ptr += ar[ch].step, ++ch,
		     buf += bytes) {
			f32 raw = cvt(buf) * s->vols[ch],
			    clamped = fmaxf(fminf(raw, 1.0f), -1.0f);
			avg += *(f32*)(void*)ar[ch].ptr = clamped;
			hasclipped |= raw != clamped;
		}
		avg *= k;
		for (u32 ch = mch; ch < nch; ar[ch].ptr += ar[ch].step, ++ch)
			*(f32*)(void*)ar[ch].ptr = avg;
	}
	if (hasclipped)
		WARN("[%s]: clipping (%u)", s->name, ++s->clipped);
}

static u8 zeros[BUF_SZ] = {0};

static bool sioplayonce(S *s, SioParam *p, struct SoundIoOutStream *os, i32 rem,
			int *played)
{
	int nframes = rem;
	struct SoundIoChannelArea *ar;
	int err = soundio_outstream_begin_write(os, &ar, &nframes);
	if (err)
		DIE("[%s]: stream error: %s", s->name, soundio_strerror(err));
	if (!nframes)
		return false;
	*played = nframes;
	const struct SoundIoChannelLayout *lo = &os->layout;
	u8 *buf;
	if (s->ps == PLAY_PLAY) {
		buf = p->buf;
		play(s, (u32)nframes, (u32)p->avg, buf, true);
	} else
		buf = zeros;
	u32 nch = (u32)lo->channel_count, mch = MIN(s->cfg.channels, nch),
	    nfr = (u32)nframes;
	if (s->cfg.isfloat)
		siowrite(s, ar, buf, nfr, mch, nch, 4, loadf32);
	else {
		switch (s->cfg.bytes) {
		case 4: siowrite(s, ar, buf, nfr, mch, nch, 4, s32tof32); break;
		case 3: siowrite(s, ar, buf, nfr, mch, nch, 3, s24tof32); break;
		case 2: siowrite(s, ar, buf, nfr, mch, nch, 2, s16tof32); break;
		default: ASSERT(false); /* shouldn't ever happen */
		}
	}
	err = soundio_outstream_end_write(os);
	if (err == SoundIoErrorUnderflow)
		return false;
	else if (err)
		DIE("[%s]: stream error", s->name);
	return true;
}

static void sioplay(struct SoundIoOutStream *os, int framesmin, int framesmax)
{
	(void)framesmin;
	S *s = os->userdata;
	SioParam *p = s->p;
	p->avg += ((f32)framesmax - p->avg) / (f32)++p->n;
	for (i32 rem = framesmax, played; rem > 0; rem -= played) {
		if (!sioplayonce(s, p, os, rem, &played))
			break;
	}
}

static void siomake(Inst *inst, S *s)
{
	SioInst *g = (void*)inst;
	SioParam *p = s->p = malloc(sizeof(*p));
	*p = (SioParam){
		.avg = 0.0f,
		.n = 0
	};
	p->os = soundio_outstream_create(g->dev);
	CHK(p->os, "[%s]: make outstream", s->name);
	p->os->format = SoundIoFormatFloat32LE;
	p->os->software_latency = g->dev->software_latency_min;
	/* need to remove colons */
	for (char *c = s->name, *e = s->name + s->namesz; c != e; ++c)
		*c = *c == ':' ? ' ' : *c;
	p->os->name = s->name;
	/* for some reason pipewire seems to like to give a low latency that it
	 * can't actually play on alsa, so manually clamp the value */
	if (strcmp(g->dev->name, "PipeWire Sound Server") == 0)
		p->os->software_latency =  fmax(p->os->software_latency, 0.002);
	p->os->write_callback = sioplay;
	p->os->sample_rate = (int)s->cfg.rate;
	p->os->userdata = s;
	int err;
	err = soundio_outstream_open(p->os);
	CHK(!err, "[%s]: open outstream", s->name);
	if ((u32)p->os->sample_rate != s->cfg.rate)
		makers(s, inst, (u32)p->os->sample_rate);
	if (p->os->layout_error)
		DIE("[%s]: layout error", s->name);
	err = soundio_outstream_start(p->os);
	CHK(!err, "[%s]: start outstream", s->name);
}

static void siodstr(Inst *inst, S *s)
{
	(void)inst;
	SioParam *p = s->p;
	if (s->rs)
		dstrrs(s->rs);
	soundio_outstream_destroy(p->os);
	free(p);
}

static void sioupdate(Inst *inst, S *s)
{
	(void)inst, (void)s;
}

static void siofinddev(SioInst *g, u32 devhash, bool listdev)
{
	g->dev = NULL;
	if (devhash == 0 && !listdev) {
		int devidx = soundio_default_output_device_index(g->sio);
		CHK(devidx >= 0, "get output device index");
		g->dev = soundio_get_output_device(g->sio, devidx);
		CHK(g->dev, "get output device");
		goto sel;
	}
	for  (int i = 0; i < soundio_output_device_count(g->sio); ++i) {
		struct SoundIoDevice *dev =
			soundio_get_output_device(g->sio, i);
		if (!dev)
			DIE("get output device");
		u32 hash = mix(strhash(dev->id) + mix(dev->is_raw + 'R')) >> 48;
		if (listdev) {
			INFO("%04X: %s (%s)%s", hash, dev->name, dev->id,
			     dev->is_raw ? " (raw)" : "");
		}
		if (hash == devhash) {
			g->dev = dev;
			break;
		}
		soundio_device_unref(dev);
	}
	if (listdev)
		return;
	if (!g->dev && devhash)
		DIE("specified device hash not found");
sel:
	INFO("selected device: %s", g->dev->name);
}

static Inst *sioinst(Pcfg pcfg, enum SoundIoBackend backend, u32 devhash,
		     bool listdev)
{
	SioInst *g = malloc(sizeof(*g));
	g->b = (Inst){
		.pcfg = pcfg,
		.make = siomake,
		.dstr = siodstr,
		.update = sioupdate
	};
	g->sio = soundio_create();
	CHK(g->sio, "make libsoundio");
	int r;
	if (backend == SoundIoBackendNone)
		r = soundio_connect(g->sio);
	else
		r = soundio_connect_backend(g->sio, backend);
	if (r == 0) {
		const char *name =
			soundio_backend_name(g->sio->current_backend);
		INFO("libsoundio: selected backend: %s", name);
	}
	CHK(r == 0, "connect to backend");
	soundio_flush_events(g->sio);
	siofinddev(g, devhash, listdev);
	return &g->b;
}
#endif
/* } libsoundio */

/* pipewire { */
#ifdef SP_PW
typedef struct {
	Inst b;
	struct pw_thread_loop *loop;
	struct pw_context *ctx;
	struct pw_core *core;
	struct pw_registry *reg;
	struct spa_hook corelstn;
	const char *dev;
} PwInst;

typedef struct {
	S *s;
	struct pw_stream *stream;
	struct spa_hook streamlstn;
} PwParam;

/* interleaved -> f32 planar */
static void pwcvt(S *s, f32 **outs, u8 *in, u32 bsz, Cfg cfg)
{
	f32 (*fnt[])(void*) = {s16tof32, s24tof32, s32tof32, loadf32},
	    (*load)(void*) = fnt[cfg.bytes - 2 + cfg.isfloat];
	bool hasclipped = false;
	u32 fsz = framesz(cfg);
	for (u32 i = 0; i < cfg.channels; ++i) {
		for (u8 *b = in + i * cfg.bytes, *e = b + bsz; b < e;
		     b += fsz, ++outs[i]) {
			f32 raw = load(b) * s->vols[i],
			    clamped = fmaxf(fminf(raw, 1.0f), -1.0f);
			hasclipped |= raw != clamped;
			*outs[i] = clamped;
		}
	}
	if (hasclipped)
		WARN("[%s]: clipping (%u)", s->name, ++s->clipped);
}

static void pwproc(void *userdata)
{
	PwParam *p = userdata;
	struct pw_buffer *pb = pw_stream_dequeue_buffer(p->stream);
	if (pb == NULL) {
		WARN("[%s]: out of buffers", p->s->name);
		return;
	}
	struct spa_buffer *sb = pb->buffer;
	f32 *dsts[MAX_CHANNELS];
	u32 ofsz = sizeof(f32) * p->s->cfg.channels,
	    nframes = sb->datas->maxsize / ofsz;
	nframes = MIN(nframes, (u32)pb->requested);
	for (u32 i = 0; i < sb->n_datas; ++i) {
		if ((dsts[i] = sb->datas[i].data) == NULL)
			return;
		sb->datas[i].chunk->offset = 0;
		sb->datas[i].chunk->stride = sizeof(f32);
		sb->datas[i].chunk->size = nframes * sizeof(f32);
	}
	u8 buf[BUF_SZ];
	play(p->s, nframes, nframes, buf, true);
	pwcvt(p->s, dsts, buf, nframes * ofsz, p->s->cfg);
	pw_stream_queue_buffer(p->stream, pb);
}

static const struct pw_stream_events streamevts = {
        PW_VERSION_STREAM_EVENTS,
	.process = pwproc,
};

static enum spa_audio_format pwfmt(Cfg cfg)
{
	(void)cfg;
	return SPA_AUDIO_FORMAT_F32P;
}

static void pwmake(Inst *inst, S *s)
{
	PwInst *g = (void*)inst;
	PwParam *p = s->p = malloc(sizeof(*p));
	*p = (PwParam){
		.s = s,
		.streamlstn = {0}
	};
	struct pw_properties *props = pw_properties_new(
		PW_KEY_NODE_NAME, s->name,
		PW_KEY_MEDIA_TYPE, "Audio",
		PW_KEY_MEDIA_CATEGORY, "Playback",
		PW_KEY_TARGET_OBJECT, g->dev,
		NULL);
	u8 pbbuf[1024];
	struct spa_pod_builder pb = SPA_POD_BUILDER_INIT(pbbuf, sizeof(pbbuf));
	const struct spa_pod *params[1];
	params[0] = spa_format_audio_raw_build(
		&pb, SPA_PARAM_EnumFormat, &SPA_AUDIO_INFO_RAW_INIT(
			.format = pwfmt(s->cfg),
			.channels = s->cfg.channels,
			.rate = s->cfg.rate
		));;
	pw_thread_loop_lock(g->loop);
	p->stream = pw_stream_new(g->core, "audio-src", props);
	CHK(p->stream, "[%s]: make stream", s->name);
	pw_stream_add_listener(p->stream, &p->streamlstn, &streamevts, p);
	CHK(pw_stream_connect(
		p->stream, PW_DIRECTION_OUTPUT, PW_ID_ANY,
		(PW_STREAM_FLAG_AUTOCONNECT | PW_STREAM_FLAG_MAP_BUFFERS |
		 PW_STREAM_FLAG_RT_PROCESS),
		params, 1) >= 0,
	    "[%s]: connect to stream", s->name);
	pw_thread_loop_unlock(g->loop);
}

static void pwdstr(Inst *inst, S *s)
{
	PwInst *g = (void*)inst;
	PwParam *p = s->p;
	pw_thread_loop_lock(g->loop);
	pw_stream_destroy(p->stream);
	pw_thread_loop_unlock(g->loop);
	free(p);
}

static void pwupdate(Inst *inst, S *s)
{
	(void)inst, (void)s;
}

static void pwdone(void *data, uint32_t it, int seq)
{
	(void)it, (void)seq;
	pw_thread_loop_signal(data, false);
}

static const struct pw_core_events pwfinddevcoreevts = {
	.version = PW_VERSION_CORE_EVENTS,
	.done = pwdone
};

typedef struct {
	PwInst *g;
	const char *dev;
	u32 devhash;
	bool listdev;
} PwFindDev;

static void pwfinddevglobal(void *data, uint32_t id, uint32_t permissions,
			    const char *type, uint32_t version,
			    const struct spa_dict *props)

{
	(void)id, (void)permissions, (void)version;
	PwFindDev *p = data;
	if (p->g->dev && !p->listdev)
		return;
	if (!props || strcmp(type, PW_TYPE_INTERFACE_Node) != 0)
		return;
	const char *class = spa_dict_lookup(props, PW_KEY_MEDIA_CLASS);
	if (!class || strcmp(class, "Audio/Sink") != 0)
		return;
	const char *name = spa_dict_lookup(props, PW_KEY_NODE_NAME);
	if (!name)
		return;
	const char *desc = spa_dict_lookup(props, PW_KEY_NODE_DESCRIPTION);
	u32 hash = mix(strhash(name)) >> 48;
	if (p->listdev) {
		INFO("%04X: %s (%s)", hash, desc ? desc : name,
		     desc ? name : "");
		return;
	}
	if ((hash == p->devhash || p->devhash == 0)) {
		p->g->dev = name;
		INFO("selected device: %s (%s)", desc ? desc : name,
		     desc ? name : "");
	}
}

static const struct pw_registry_events finddevregevts = {
	.version = PW_VERSION_REGISTRY_EVENTS,
	.global = pwfinddevglobal
};

static void pwfinddev(PwInst *g, u32 devhash, bool listdev)
{
	g->dev = NULL;
	struct spa_hook corelstn = {0};
	CHK(pw_core_add_listener(
		g->core, &corelstn, &pwfinddevcoreevts, g->loop) >= 0,
	    "add finddev core listener");
	struct pw_registry *reg = pw_core_get_registry(
		g->core, PW_VERSION_REGISTRY, 0);
	CHK(reg, "get registry");
	pw_core_sync(g->core, 0, 0);
	struct spa_hook reglstn = {0};
	PwFindDev p = {
		.g = g,
		.devhash = devhash,
		.listdev = listdev,
	};
	int r = pw_registry_add_listener(
		reg, &reglstn, &finddevregevts, &p);
	CHK(r >= 0, "add finddev registry listener");
	pw_thread_loop_wait(g->loop);
	if (!g->dev && !listdev)
		DIE("no devices found");
	spa_hook_remove(&reglstn);
	pw_proxy_destroy((struct pw_proxy*)reg);
	spa_hook_remove(&corelstn);
}

static void pwcoreerror(void *data, uint32_t id, int seq, int res,
			const char *message)
{
	(void)data, (void)id, (void)seq;
	DIE("pipewire error: %s, %s", spa_strerror(res), message);
}

static const struct pw_core_events pwcoreevts = {
	.version = PW_VERSION_CORE_EVENTS,
	.error = pwcoreerror
};

static Inst *pwinst(Pcfg pcfg, u32 devhash, bool listdev)
{
	PwInst *g = malloc(sizeof(*g));
	*g = (PwInst){
		.b = {
			.pcfg = pcfg,
			.make = pwmake,
			.dstr = pwdstr,
			.update = pwupdate
		},
		.corelstn = {0}
	};
        pw_init(NULL, NULL);
	g->loop = pw_thread_loop_new("sp-recv-pipewire", NULL);
	pw_thread_loop_lock(g->loop);
	CHK(pw_thread_loop_start(g->loop) >= 0, "start thread loop");
	g->ctx = pw_context_new(pw_thread_loop_get_loop(g->loop),
				pw_properties_new(PW_KEY_CONFIG_NAME,
						  "client-rt.conf", NULL), 0);
	CHK(g->ctx, "make context");
	g->core = pw_context_connect(g->ctx, NULL, 0);
	CHK(g->ctx, "connect to pipewire");
	int r = pw_core_add_listener(
		g->core, &g->corelstn, &pwcoreevts, NULL);
	CHK(r >= 0, "add core listener");
	pwfinddev(g, devhash, listdev);
	pw_thread_loop_unlock(g->loop);
        return &g->b;
}
#endif
/* } pipewire */

/* I think libsoundio's WASAPI backend is really poor. I get 100% CPU on the
 * 'sio_sine' example from them. Also doesn't have support for IAudioClient3.
 * So use our own implementation. */

/* wasapi { */
#ifdef SP_WAS
typedef struct {
	Inst b;
	IMMDevice *dev;
} WasInst;

typedef struct {
	IAudioClient3 *c;
	IAudioSessionControl *sc;
	IAudioRenderClient *rc;
	SRWLOCK srw;
	HANDLE evt, thrd;
	u32 evtto, bufsz, n;
	f32 avg;
	bool stop;
} WasParam;

static WAVEFORMATEXTENSIBLE makewavefmt(Cfg cfg)
{
	WAVEFORMATEXTENSIBLE wf = {
		.Format = {
			.wFormatTag = WAVE_FORMAT_EXTENSIBLE,
			.nChannels = (WORD)cfg.channels,
			.nSamplesPerSec = cfg.rate,
			.nAvgBytesPerSec = cfg.rate * cfg.channels * 4,
			.nBlockAlign = (WORD)cfg.channels * 4,
			.wBitsPerSample = 32,
			.cbSize = sizeof(wf)
		},
		.Samples = {
			.wValidBitsPerSample = 32
		},
		/* TODO: handle special speakers */
		.dwChannelMask = SPEAKER_FRONT_LEFT | SPEAKER_FRONT_RIGHT,
		.SubFormat = KSDATAFORMAT_SUBTYPE_IEEE_FLOAT
	};
	return wf;
}

static bool wasplayonce(S *s, WasParam *p)
{
	DWORD r = WaitForSingleObjectEx(p->evt, p->evtto, FALSE);
	if (r == WAIT_FAILED) {
		WARN("[%s]: failed to wait for buffer", s->name);
		return true;
	}
	AcquireSRWLockExclusive(&p->srw);
	if (p->stop)
		return false;
	u32 padding;
	HRESULT hr = F(p->c, GetCurrentPadding, &padding);
	if (FAILED(hr))
		DIE("[%s]: GetCurrentPadding failed", s->name);
	u32 nframes = p->bufsz - padding;
	u8 *frames;
	hr = F(p->rc, GetBuffer, nframes, &frames);
	if (FAILED(hr))
		DIE("[%s]: GetBuffer failed", s->name);
	u8 buf[BUF_SZ];
	p->avg += ((f32)nframes - p->avg) / (f32)++p->n;
	play(s, nframes, (u32)p->avg, buf, true);
	f32 out[BUF_SZ / sizeof(f32)], *f = out;
	u32 ifsz = framesz(s->cfg),
	    ofsz = sizeof(f32) * s->cfg.channels;
	if (!s->cfg.isfloat)
		bitcvt(f, buf, nframes * ifsz, s->cfg.bytes);
	else 
		f = (f32*)buf;
	bool hasclipped = false;
	for (u32 i = 0; i < nframes * s->cfg.channels;) {
		for (u32 j = 0; j < s->cfg.channels; ++i, ++j) {
			f32 raw = f[i] * s->vols[j],
			    clamped = fmaxf(fminf(raw, 1.0f), -1.0f);
			hasclipped |= raw != clamped;
		}
	}
	if (hasclipped)
		WARN("[%s]: clipping (%u)", s->name, ++s->clipped);
	memcpy(frames, f, ofsz * nframes);
	F(p->rc, ReleaseBuffer, nframes, 0);
	ReleaseSRWLockExclusive(&p->srw);
	(void)loadf32; /* unused */
	return true;
}

static DWORD WINAPI wasplay(LPVOID p_)
{
	S *s = p_;
	WasParam *p = s->p;
	while (wasplayonce(s, p));
	return 0;
}

static u32 waswcstomb(char dst[MAX_NAME], const wchar_t *src)
{
	return WideCharToMultiByte(CP_UTF8, 0, src, -1, dst, MAX_NAME - 1,
				   NULL, NULL);
}

static u32 wasmbtowcs(wchar_t dst[MAX_NAME], const char *src)
{
	return MultiByteToWideChar(CP_UTF8, 0, src, -1, dst, MAX_NAME - 1);
}

static void wasmake(Inst *inst, S *s)
{
	WasInst *g = (void*)inst;
	WasParam *p = s->p = malloc(sizeof(*p));
	*p = (WasParam){
		.srw = SRWLOCK_INIT,
		.stop = false
	};
	HRESULT hr;
	hr = F(g->dev, Activate, &IID_IAudioClient3, CLSCTX_ALL, NULL,
	       (void**)&p->c);
	WAVEFORMATEXTENSIBLE wf = makewavefmt(s->cfg);
	WAVEFORMATEX *mwf;
	F(p->c, GetMixFormat, &mwf);
	bool sup = (mwf->wFormatTag == WAVE_FORMAT_IEEE_FLOAT ||
		    (mwf->wFormatTag == WAVE_FORMAT_EXTENSIBLE &&
		     IsEqualGUID(&((WAVEFORMATEXTENSIBLE*)mwf)->SubFormat,
				 &KSDATAFORMAT_SUBTYPE_IEEE_FLOAT)));
	if (!sup)
		DIE("unsupported mix format");
	if (s->cfg.rate != mwf->nSamplesPerSec) {
		makers(s, inst, mwf->nSamplesPerSec);
		wf = makewavefmt(s->cfg);
	}
	UINT32 def, fund, min, max;
	hr = F(p->c, GetSharedModeEnginePeriod, (WAVEFORMATEX*)&wf, &def, &fund,
	       &min, &max);
	CHKH(hr, "[%s]: get engine period", s->name);
	hr = F(p->c, InitializeSharedAudioStream,
	       AUDCLNT_STREAMFLAGS_EVENTCALLBACK, min, (WAVEFORMATEX*)&wf,
	       NULL);
	CHKH(hr, "[%s]: make client", s->name);
	REFERENCE_TIME defperiod, minperiod;
	hr = F(p->c, GetDevicePeriod, &defperiod, &minperiod);
	CHKH(hr, "[%s]: get device period", s->name);
	p->evtto = (u32)(minperiod / 1000);
	UINT32 bufsz;
	hr = F(p->c, GetBufferSize, &bufsz);
	CHKH(hr, "[%s]: get buffer size", s->name);
	p->bufsz = (u32)bufsz;
	hr = F(p->c, GetService, &IID_IAudioSessionControl, (void**)&p->sc);
	CHKH(hr, "[%s]: get session control", s->name);
	wchar_t name[MAX_NAME];
	wasmbtowcs(name, s->name);
	hr = F(p->sc, SetDisplayName, name, NULL);
	CHKH(hr, "[%s]: set session name", s->name);
	hr = F(p->c, GetService, &IID_IAudioRenderClient, (void**)&p->rc);
	CHKH(hr, "[%s]: get render client", s->name);
	p->evt = CreateEventW(NULL, FALSE, FALSE, NULL);
	CHK(p->evt, "[%s]: make event", s->name);
	hr = F(p->c, SetEventHandle, p->evt);
	CHKH(hr, "[%s]: set event handle", s->name);
	p->thrd = CreateThread(NULL, 0, wasplay, s, 0, NULL);
	F(p->c, Start);
}

static void wasdstr(Inst *inst, S *s)
{
	(void)inst;
	WasParam *p = s->p;
	AcquireSRWLockExclusive(&p->srw);
	p->stop = true;
	SetEvent(&p->evt);
	ReleaseSRWLockExclusive(&p->srw);
	WaitForSingleObject(p->thrd, INFINITE);
	CloseHandle(p->evt);
	F(p->rc, Release);
	F(p->sc, Release);
	F(p->c, Release);
	if (s->rs)
		dstrrs(s->rs);
	free(p);
}

static void wasupdate(Inst *inst, S *s)
{
	(void)inst, (void)s;
}

static u32 wasdevname(IMMDevice *dev, char out[MAX_NAME])
{
	IPropertyStore *ps;
	HRESULT hr = F(dev, OpenPropertyStore, STGM_READ, &ps);
	if (FAILED(hr))
		return 0;
	PROPVARIANT name;
	PropVariantInit(&name);
	F(ps, GetValue, &PKEY_DeviceInterface_FriendlyName, &name);
	u32 sz = waswcstomb(out, name.pwszVal);
	PropVariantClear(&name);
	F(ps, Release);
	return sz;
}

static void wasfinddev(WasInst *g, IMMDeviceEnumerator *devenum, u32 devhash,
		       bool listdev)
{
	g->dev = NULL;
	HRESULT hr;
	if (devhash == 0 && !listdev) {
		hr = F(devenum, GetDefaultAudioEndpoint, eRender, eConsole,
		       &g->dev);
		CHKH(hr, "get default audio endpoint");
		goto sel;
	}
	IMMDeviceCollection *devs;
	F(devenum, EnumAudioEndpoints, eRender, DEVICE_STATE_ACTIVE, &devs);
	UINT n;
	F(devs, GetCount, &n);
	for (u32 i = 0; i < n; ++i) {
		IMMDevice *dev;
		hr = F(devs, Item, i, &dev);
		if (FAILED(hr))
			goto next;
		wchar_t *wid;
		hr = F(dev, GetId, &wid);
		if (FAILED(hr))
			goto next;
		char id[MAX_NAME];
		u32 idsz = waswcstomb(id, wid),
		    hash = mix(strhash(id)) >> 48;
		CoTaskMemFree(wid);
		char name[MAX_NAME];
		u32 sz = wasdevname(dev, name);
		if (listdev)
			INFO("%04X: %.*s (%.*s)", hash, sz, name, idsz, id);
		if (hash == devhash) {
			g->dev = dev;
			break;
		}
	next:
		F(dev, Release);
	}
	if (listdev)
		return;
	if (!g->dev && devhash)
		DIE("specified device hash not found");
sel:;
	char name[MAX_NAME];
	u32 sz = wasdevname(g->dev, name);
	INFO("selected device: %.*s", sz, name);
}

static Inst *wasinst(Pcfg pcfg, u32 devhash, bool listdev)
{
	WasInst *g = malloc(sizeof(*g));
	g->b = (Inst){
		.pcfg = pcfg,
		.make = wasmake,
		.dstr = wasdstr,
		.update = wasupdate
	};
	HRESULT hr = CoInitializeEx(NULL, COINIT_MULTITHREADED);
	CHKH(hr, "make com instance");
	IMMDeviceEnumerator *devenum;
	hr = CoCreateInstance(&CLSID_MMDeviceEnumerator, NULL, CLSCTX_ALL,
			      &IID_IMMDeviceEnumerator, (void**)&devenum);
	CHKH(hr, "make device enumerator");
	wasfinddev(g, devenum, devhash, listdev);
	F(devenum, Release);
	return &g->b;
}
#endif
/* } wasapi */

static void make(S *s, u32 idx, const char *name, u32 namesz, u32 slack,
		 Cfg cfg, struct sockaddr *sa)
{
	*s = (S){
		.name = {0},
		.namesz = namesz,
		.rb = {0},
		.cfg = cfg,
		.t = 0,
		.idx = idx,
		.ctr = 0,
		.underrun = 0,
		.overrun = 0,
		.pauseheur = 0,
		.clipped = 0,
		.lastthres = 0,
		.slack = slack,
		.ps = PLAY_WAIT,
		.sa = *sa,
		.fullymade = false,
		.logged = false,
		.rs = NULL,
	};
	memcpy(s->name, name, namesz);
	INFO("[%u]: making new client: %.*s", idx, namesz, name);
	for (u32 i = 0; i < MAX_CHANNELS; ++i)
		s->vols[i] = 1.0f;
	CHK(pthread_mutex_init(&s->mtx, NULL) == 0, "[%s]: make mutex",
	    s->name);
}

static void dstr(S *s)
{
	pthread_mutex_destroy(&s->mtx);
}

static Socket makesock(u16 port)
{
#ifdef _WIN32
	WSADATA ws;
	CHK(WSAStartup(MAKEWORD(2, 2), &ws) == 0, "make winsock2");
#endif
	struct sockaddr_in sinv = {
		.sin_family = AF_INET,
		.sin_port = htons(port),
		.sin_addr.s_addr = htonl(INADDR_ANY)
	};
	Socket sfd = socket(PF_INET, SOCK_DGRAM, 0);
	CHK(sfd > 0, "make socket");
        CHK(bind(sfd, (struct sockaddr*)&sinv, sizeof(sinv)) != -1, "bind");
	return sfd;
}

static fd_set fds(Socket sfd)
{
	fd_set fds;
	FD_ZERO(&fds);
	FD_SET(sfd, &fds);
	return fds;
}

enum {MAX_CLIENT = 32};

typedef enum {
	SLOW_MAKE,
	SLOW_DSTR,
	SLOW_REMAKE
} SlowCmd;

typedef struct {
	S *s;
	Cmd recvcmd;
	SlowCmd slowcmd;
} SlowEntry;

enum {MAX_SLOW = MAX_CLIENT * 4};
typedef struct {
	Inst *inst;
	Socket sfd;
	pthread_mutex_t mtx;
	pthread_cond_t cnd;
	SlowEntry entries[MAX_SLOW], *r, *w;
} SlowProc;

static void handleslow(SlowProc *p, SlowEntry e)
{
	switch (e.slowcmd) {
	case SLOW_REMAKE:
	case SLOW_DSTR:
		p->inst->dstr(p->inst, e.s);
		e.s->fullymade = false;
		if (e.slowcmd != SLOW_REMAKE)
			break;
		/* FALLTHROUGH */
	case SLOW_MAKE:
		p->inst->make(p->inst, e.s);
		e.s->fullymade = true;
		break;
	}
	if (e.recvcmd == CMD_PREP) {
		u8 v = 1;
		int r = (int)sendto(p->sfd, (const void*)&v, sizeof(v), 0,
				    &e.s->sa, sizeof(struct sockaddr_in));
		if (r == -1)
			WARN("[%s]: error sending response to prep", e.s->name);
	}
}

static void enqslow(SlowProc *p, SlowEntry e)
{
	pthread_mutex_lock(&p->mtx);
	*p->w++ = e;
	p->w = p->w == p->entries + MAX_SLOW ? p->entries : p->w;
	pthread_mutex_unlock(&p->mtx);
	pthread_cond_signal(&p->cnd);
}

static SlowEntry deqslow(SlowProc *p)
{
	pthread_mutex_lock(&p->mtx);
	while (p->r == p->w)
		pthread_cond_wait(&p->cnd, &p->mtx);
	SlowEntry e = *p->r++;
	p->r = p->r == p->entries + MAX_SLOW ? p->entries : p->r;
	pthread_mutex_unlock(&p->mtx);
	return e;
}

static void *slowproc(void *p_)
{
	SlowProc *p = p_;
	while (true) {
		SlowEntry e = deqslow(p);
		handleslow(p, e);
	}
	return NULL;
}

static void rm(S *s, SlowProc *sl, u32 *taken, u32 *recycle, u32 *n)
{
	dstr(s);
	enqslow(sl, (SlowEntry){
		.s = s,
		.slowcmd = SLOW_DSTR,
		.recvcmd = 0
	});
	u32 bit = 1u << s->idx;
	*taken ^= bit;
	*recycle ^= bit;
	/* it might still be in the callback */
	s->ps = PLAY_PAUSE;
	ringbuf_reset(&s->rb);
	--*n;
}

static void handle(Inst *inst, Socket sfd, S *ss, u64 *lastt, u32 *n,
		   u32 *taken, u32 *avail, u32 *recycle, SlowProc *sl,
		   u32 slack, bool quiet)
{
	struct sockaddr sa;
	socklen_t sasz = sizeof(sa);
	u8 buf[BUF_SZ], *b = buf;
	int read = (int)recvfrom(sfd, (char*)buf, BUF_SZ, 0, &sa, &sasz);
	if (read < HDR_SZ + 1) {
		WARN("read less than header size (%u bytes)", read);
		return;
	}
	u32 bsz = (u32)read;
	Hdr hdr;
	dechdr(b, &hdr);
	b += HDR_SZ;
	bsz -= HDR_SZ;
	char name[MAX_NAME];
	u32 namesz = 0;
	for (char *c = (char*)b; *c; ++c, ++b)
		name[namesz++] = *c;
	bsz -= namesz + 1;
	++b;
	Cfg cfg = hdr.cfg;
	if (cfg.isfloat)
		cfg.bytes = 4;
	if (cfg.bytes < 2 || cfg.bytes > 4) {
		WARN("unsupported bit depth");
		return;
	}
	u32 idx = ~0u;
	bool makenew = false;
	SlowCmd slowcmd;
	for (u32 j = 0; j < MAX_CLIENT; ++j) {
		S *s = &ss[j];
		if (!(*taken >> j & 1) || s->cfg.id != cfg.id)
			continue;
		idx = j;
		if (!s->fullymade)
			return;
		s->cfg.bufsz = cfg.bufsz; /* ignore changes in buffer size */
		Cfg cmpcfg = s->cfg;
		if (s->rs)
			cmpcfg = s->rs->orig;
		if (memcmp(&cmpcfg, &cfg, sizeof(cfg)) != 0) {
			INFO("[%s]: client cfg has changed, remaking", s->name);
			dstr(&ss[idx]);
			slowcmd = SLOW_REMAKE;
			makenew = true;
		}
		break;
	}
	if (idx == ~0u) {
		if (*n == MAX_CLIENT) {
			WARN("connection requested but at max number of "
			     "clients");
			return;
		}
		if (!*avail) {
			*avail = *recycle;
			*recycle = 0;
		}
		idx = (u32)__builtin_ctz(*avail);
		u32 bit = 1u << idx;
		*avail ^= bit;
		*taken ^= bit;
		++*n;
		slowcmd = SLOW_MAKE;
		makenew = true;
	}
	S *s = &ss[idx];
	lastt[idx] = ms();
	if (makenew) {
		if (hdr.cfg.rate == 0) {
			WARN("invalid config from connection, skipping...");
			return;
		}
		make(s, idx, name, namesz, slack, cfg, &sa);
		enqslow(sl, (SlowEntry){
			.s = s,
			.slowcmd = slowcmd,
			.recvcmd = hdr.cmd
		});
	}
	if (s->fullymade) {
		if (!s->logged) {
			INFO("[%s]: client created: id = %08X, rate = %u, "
			     "bits = %u, channels = %u, type = %s, "
			     "buffer size: %u frames (%u ms)", s->name, cfg.id,
			     cfg.rate, cfg.bytes * 8, cfg.channels,
			     cfg.isfloat ? "f32" : "pcm",
			     cfg.bufsz / framesz(cfg),
			     cfg.bufsz * 1000 / cfg.rate);
			s->logged = true;
		}
		inst->update(inst, s);
	}
	pthread_mutex_lock(&s->mtx);
	RingBuf *rb = !s->rs ? &s->rb : &s->rs->rb;
	bool send = false;
	switch (hdr.cmd) {
	case CMD_RESET:
		INFO("[%s]: reset", s->name);
		ringbuf_reset(rb);
		goto unlock;
	case CMD_PAUSE:
		INFO("[%s]: pause", s->name);
		s->ps = PLAY_PAUSE;
		goto unlock;
	case CMD_START:
		INFO("[%s]: start", s->name);
		s->ps = PLAY_PLAY;
		goto unlock;
	case CMD_PREP:
		INFO("[%s]: prep", s->name);
		s->ps = PLAY_WAIT;
		goto unlock;
	case CMD_RM:
		if (s->fullymade) {
			INFO("[%s]: remove", s->name);
			rm(s, sl, taken, recycle, n);
		}
		goto unlock;
	case CMD_VOL:
		if (bsz < sizeof(f32)) {
			WARN("[%s]: read not enough bytes for volume", s->name);
			goto unlock;
		}
		for (u32 i = 0, ch = 0; i < bsz; i += sizeof(f32), ++ch) {
			f32 vol;
			memcpy(&vol, b, sizeof(vol));
			if (vol >= 0.0f) {
				s->vols[ch] = vol;
				INFO("[%s]: set volume (channel %u) to %u%%",
				     s->name, i, (u32)(s->vols[ch] * 100.0f));
			}
		}
		goto unlock;
	case CMD_SILENT:
		if (bsz < sizeof(u32)) {
			WARN("[%s]: read not enough bytes for silence",
			     s->name);
		}
		else {
			u32 sz;
			memcpy(&sz, b, sizeof(sz));
			ringbuf_set(rb, 0, sz);
		}
		goto unlock;
	default:
		send = true;
		break;
	}
	if (send && bsz && s->fullymade) {
		u32 writable = BUF_SZ - ringbuf_readable(rb);
		if (writable < bsz)
			ringbuf_reset(rb);
		ringbuf_write(rb, b, bsz);
		if (!s->rs && (s->ps == PLAY_WAIT || s->ps == PLAY_TMP_PAUSE))
			s->ps = PLAY_PLAY;
	}
unlock:
	pthread_mutex_unlock(&s->mtx);
	if (s->rs)
		pthread_cond_signal(&s->rs->cnd);
	u64 t = ns();
	if (!quiet && s->t && (0 || s->ctr % 100 == 0)) {
		INFO("[%s]: recv (%u): %u bytes - %u us", s->name,
		     s->ctr, read, (u32)(t - s->t) / 1000);
	}
	++s->ctr;
	s->t = t;
}

static void run(u16 port, Inst *inst, u32 slack, bool quiet)
{
	Socket sfd = makesock(port);
	SlowProc sl = {
		.sfd = sfd,
		.inst = inst,
		.entries = {0},
		.r = sl.entries,
		.w = sl.entries
	};
	pthread_mutex_init(&sl.mtx, NULL);
	pthread_cond_init(&sl.cnd, NULL);
	enum {SLOW_THREADS = 2};
	pthread_t slowthrds[SLOW_THREADS];
	for (u32 i = 0; i < SLOW_THREADS; ++i)
		pthread_create(&slowthrds[i], NULL, slowproc, &sl);
	/* not removing dead clients yet.. */
	S *ss = malloc(sizeof(*ss) * MAX_CLIENT);
	u64 lastt[MAX_CLIENT];
	u32 n = 0, taken = 0u, avail = ~0u, recycle = 0u;
	_Static_assert(MAX_CLIENT <= 32, "");
	fd_set fdsv;
	int r;
	const u32 timeouts = 60, timeoutms = timeouts * 1000;
	u64 nextto = ms() + timeoutms, t;
	struct timeval tv;
	for (; true; nextto = t + timeoutms) {
		t = ms();
		tv.tv_sec = (i64)((nextto - MIN(t, nextto)) / 1000);
		tv.tv_usec = 0;
		fdsv = fds(sfd);
		r = select((int)sfd + 1, &fdsv, NULL, NULL, &tv);
		if (r < 0)
			break;
		else if (r > 0) {
			handle(inst, sfd, ss, lastt, &n, &taken, &avail,
			       &recycle, &sl, slack, quiet);
		}
		t = ms();
		if (t < nextto)
			continue;
		u32 idxs = taken;
		for (u32 idx; idxs; idxs ^= 1 << idx) {
			idx = (u32)__builtin_ctz(idxs);
			if (t < lastt[idx] + timeoutms) 
				continue;
			INFO("[%s]: timeout", ss[idx].name);
			rm(&ss[idx], &sl, &taken, &recycle, &n);
		}
	}
	if (r < 0)
		DIE("select");
}

static void help(const char *argv)
{
	const char *s =
		"options:\n\t-h\t(help)\n\t-q\t(no recv logging)\n\t"
	     	"-d\tdevice hash\n\t-L\t(list devices)\n\t-o\t"
#ifdef SP_SIO
		"[alsa | jack | pulse] (libsoundio) "
#ifdef SP_PW
		"| "
#endif
#endif
#ifdef SP_PW
		"pipewire"
#endif
#ifdef SP_WAS
		"wasapi"
#endif
		" = auto\n\t"
	     	"-s\tmin drop slack (ms) = 0 (auto)\n\t"
#ifdef USE_RESAMPLE
	     	"-r\tresampler quality (1 (lowest) - 10 (highest), only for "
		"libsoundio and WASAPI) = 9\n\t"
#else
#endif
		"-R\t(automatically respawn process on death)";
	INFO("usage: %s <port> [options...]\n%s", argv, s);
}

#define HELP()  do { \
	help(*argv); \
	ABORT(); } while (0)

int main(int argc, char *argv[])
{
	if (argc >= 2)
		respawner(argc, argv, 2);
	INFO("sp (recv)");
	if (argc < 2)
		HELP();
	char *end;
	u32 port = (u32)strtoul(argv[1], &end, 10);
	if (port == 0 || port >= 0xFFFF)
		DIE("invalid port");
	u32 devhash = 0, slack = 0, rsqual = 9;
	bool quiet = false, listdev = false;
	enum {
#ifdef SP_SIO
		BACKEND_SIO,
#endif
#ifdef SP_PW
		BACKEND_PW,
#endif
#ifdef SP_WAS
		BACKEND_WAS,
#endif
	} backend = 0;
#ifdef SP_SIO
	enum SoundIoBackend sioback = SoundIoBackendNone;
#endif
	for (int i = 2; i < argc; ++i) {
		if (*argv[i] == '~')
			continue;
		else if (*argv[i] != '-')
			HELP();
		switch (argv[i][1]) {
		case 'q':
			quiet = true;
			break;
		case 'o':
			if (++i >= argc)
				HELP();
			if (false) {}
#ifdef SP_PW
			else if (strcmp(argv[i], "pipewire") == 0)
				backend = BACKEND_PW;
#endif
#ifdef SP_SIO
			else if (strcmp(argv[i], "alsa") == 0)
				sioback = SoundIoBackendAlsa;
			else if (strcmp(argv[i], "jack") == 0)
				sioback = SoundIoBackendJack;
			else if (strcmp(argv[i], "pulse") == 0)
				sioback = SoundIoBackendPulseAudio;
#endif
			else
				HELP();
			break;
		case 'd':
			if (++i >= argc)
				HELP();
			devhash = (u32)strtoul(argv[i], &end, 16);
			break;
		case 'L':
			listdev = true;
			break;
		case 's':
			if (++i >= argc)
				HELP();
			slack = (u32)strtoul(argv[i], &end, 10);
			break;
#ifdef USE_RESAMPLE
		case 'r':
			if (++i >= argc)
				HELP();
			rsqual = (u32)strtoul(argv[i], &end, 10);
			if (rsqual > 10)
				HELP();
			break;
#endif
		case 'h':
		default:
			HELP();
		}
	}
	Pcfg pcfg = {.rsqual = rsqual};
	Inst *inst;
	switch (backend) {
#ifdef SP_SIO
	case BACKEND_SIO:
		INFO("selected backend: libsoundio");
		inst = sioinst(pcfg, sioback, devhash, listdev);
		break;
#endif
#ifdef SP_PW
	case BACKEND_PW:
		INFO("selected backend: pipewire");
		inst =  pwinst(pcfg, devhash, listdev);
		break;
#endif
#ifdef SP_WAS
	case BACKEND_WAS:
		INFO("selected backend: WASAPI");
		inst = wasinst(pcfg,  devhash, listdev);
		break;
#endif
	}
	if (listdev)
		goto done;
	run((u16)port, inst, slack, quiet);
done:
	INFO("ok");
	return 0;
}
