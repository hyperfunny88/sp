/* Copyright (C) 2023 hyperfunny88
 * SPDX-License-Identifier: GPL-3.0-or-later */

#include "util.h"
#include <arpa/inet.h>
#include <spa/param/audio/format-utils.h>
#include <pipewire/pipewire.h>

typedef struct {
	Socket sfd;
	struct pw_main_loop *loop;
	struct pw_context *ctx;
	struct pw_core *core;
	struct pw_stream *stream;
	struct spa_hook streamlstn;
	u32 ip, rate, channels, id, namesz;
	u16 port;
	char name[MAX_NAME];
} S;

static Hdr hdr(S *s, u32 bufsz)
{
	return (Hdr){
		.cfg = {
			.rate = s->rate,
			.bytes = 4,
			.isfloat = true,
			.channels = s->channels,
			.id = s->id,
			.bufsz = bufsz
		},
		.cmd = CMD_NONE
	};
}

static void makesock(S *s)
{
	s->sfd = socket(AF_INET, SOCK_DGRAM, 0);
	CHK(s->sfd > 0, "make socket");
}

static void proc(void *p)
{
	S *s = p;
	struct pw_buffer *pb = pw_stream_dequeue_buffer(s->stream);
	if (pb == NULL) {
		WARN("out of buffers");
		return;
	}
	struct spa_data *sd = pb->buffer->datas;
	u32 off = MIN(sd->chunk->offset, sd->maxsize),
	    bsz = MIN(sd->chunk->size, sd->maxsize - off),
	    fsz = sizeof(f32) * s->channels,
	    nframes = bsz / fsz;
	u8 buf[BUF_SZ], *b = buf;
	enchdr(buf, hdr(s, nframes));
	b += HDR_SZ;
	memcpy(b, s->name, s->namesz + 1);
	b += s->namesz + 1;
	memcpy(b, sd->data, bsz);
	b += bsz;
	struct sockaddr_in sa = {
		.sin_family = AF_INET,
		.sin_port = htons(s->port),
		.sin_addr.s_addr = s->ip
	};
	u32 tosend = (u32)(b - buf);
	/* might be bad since this is realtime thread and sendto can block
	 * but should... be fine on a fast network card like virtio-net */
	sendto(s->sfd, buf, tosend, 0, (struct sockaddr*)&sa, sizeof(sa));
	pw_stream_queue_buffer(s->stream, pb);
}

static const struct pw_stream_events streamevts = {
	PW_VERSION_STREAM_EVENTS,
	.process = proc
};

static void makepw(S *s)
{
	pw_init(NULL, NULL);
	s->loop = pw_main_loop_new(NULL);
	s->ctx = pw_context_new(pw_main_loop_get_loop(s->loop),
				pw_properties_new(PW_KEY_CONFIG_NAME,
						  "client-rt.conf", NULL), 0);
	CHK(s->ctx, "make context");
	s->core = pw_context_connect(s->ctx, NULL, 0);
	CHK(s->core, "connect to pipewire");
	struct pw_properties *props = pw_properties_new(
		PW_KEY_NODE_NAME, "sp-send",
		PW_KEY_MEDIA_TYPE, "Audio",
		PW_KEY_MEDIA_CLASS, "Audio/Sink",
		PW_KEY_FACTORY_NAME, "support.null-audio-sink",
		PW_KEY_NODE_PASSIVE, "out",
		PW_KEY_NODE_VIRTUAL, "true",
		"audio.position", "FL,FR",
		NULL);
	s->stream = pw_stream_new(s->core, "sp-send", props);
	CHK(s->stream, "make sink stream");
	pw_stream_add_listener(s->stream, &s->streamlstn, &streamevts, s);
	const struct spa_pod *params[1];
	struct spa_pod_builder pb;
	u8 buf[1024];
	spa_pod_builder_init(&pb, buf, sizeof(buf));
	params[0] = spa_format_audio_raw_build(
		&pb, SPA_PARAM_EnumFormat,
		&(struct spa_audio_info_raw){
			.format = SPA_AUDIO_FORMAT_F32,
			.rate = s->rate,
			.channels = s->channels,
		});
	int res = pw_stream_connect(
		s->stream, PW_DIRECTION_INPUT, PW_ID_ANY,
		(PW_STREAM_FLAG_AUTOCONNECT | PW_STREAM_FLAG_MAP_BUFFERS |
		 PW_STREAM_FLAG_RT_PROCESS), params, 1);
	CHK(res >= 0, "connect sink stream");
}

static void help(const char *argv)
{
	const char *s =
		"options:\n\t"
		"-r\trate = 48000\n\t"
		"-c\tchannels = 2\n\t"
		"-R\t(automatically respawn process on death)";
	INFO("usage: %s <ip> <port> [options...]\n%s", argv, s);
}

#define HELP() do { \
	help(*argv); \
	ABORT(); } while (0)

int main(int argc, char *argv[])
{
	if (argc >= 3)
		respawner(argc, argv, 3);
	INFO("sp (send | linux)");
	WARN("!! Make sure to disable vm.overrides in pipewire.conf !!");
	if (argc < 3)
		HELP();
	char *end;
	S s = {
		.streamlstn = {0},
		.ip = inet_addr(argv[1]),
		.rate = 48000,
		.channels = 2,
		.port = (u16)strtol(argv[2], &end, 10),
		.id = rndid()
	};
	for (int i = 3; i < argc; ++i) {
		if (*argv[i] == '~') {
			continue;
		} else if (*argv[i] != '-') {
			HELP();
		} if (argv[i][1] == 'r') {
			if (++i >= argc)
				HELP();
			s.rate = (u32)strtoul(argv[i], &end, 10);
		} else if (argv[i][1] == 'c') {
			if (++i >= argc)
				HELP();
			s.channels = (u32)strtoul(argv[i], &end, 10);
			if (s.channels < 1 || s.channels > 16)
				HELP();
		} else
			HELP();
	}
	char hostname[256];
	CHK(gethostname(hostname, sizeof(hostname)) == 0, "get hostname");
	u32 sz = (u32)snprintf(s.name, MAX_NAME - 1, "%s/sp-send", hostname);
	s.namesz = MIN(sz, MAX_NAME - 1);
	makesock(&s);
	makepw(&s);
        pw_main_loop_run(s.loop);
	return 0;
}
