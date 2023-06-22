/* Copyright (C) 2023 hyperfunny88
 * SPDX-License-Identifier: GPL-3.0-or-later */

#include "util.h"
#include <MinHook.h>
#include <fcntl.h>
#include <psapi.h>

#define FNCOM(t, fn) static typeof(((t*)NULL)->lpVtbl->fn) o_##fn
#define TRY(x, gt) if (!(x)) goto gt
#define TRYH(hr, gt) TRY(SUCCEEDED(hr), gt)

/* support multiple clients in the same proc */
enum {MAX_CLIENTS = 4};

typedef struct {
	u8 *buf;
	u64 t;
	Cfg cfg;
	u32 cnt;
	f32 mastervol, streamvols[MAX_CHANNELS];
} V;

typedef struct {
	Socket sfd;
	u32 n;
	IAudioClient *cs[MAX_CLIENTS];
	IAudioRenderClient *rcs[MAX_CLIENTS];
	ISimpleAudioVolume *savs[MAX_CLIENTS];
	IAudioStreamVolume *asvs[MAX_CLIENTS];
	V vs[MAX_CLIENTS];
	SRWLOCK srw;
	char name[MAX_NAME];
	u32 namesz;
} S;

static S *s = NULL, sstore;

static volatile enum {
	LOAD_WAIT,
	LOAD_DISABLE,
	LOAD_OK
} loadstate = LOAD_WAIT;

static HANDLE loadevt = NULL,
	      pipe = NULL;

typedef struct {
	char namebuf[MAX_PATH], *name;
	u32 namesz, ip;
	u16 port;
} Prg;

static Prg prg;

#ifdef SP_DEBUG
static volatile bool conready = false;
#endif

static V *vfromrc(IAudioRenderClient *rc)
{
	for (u32 i = 0; i < s->n; ++i) {
		if (s->rcs[i] == rc)
			return &s->vs[i];
	}
	return NULL;
}

static V *vfromc(IAudioClient *c)
{
	for (u32 i = 0; i < s->n; ++i) {
		if (s->cs[i] == c)
			return &s->vs[i];
	}
	return NULL;
}

static V *vfromsav(ISimpleAudioVolume *sav)
{
	for (u32 i = 0; i < s->n; ++i) {
		if (s->savs[i] == sav)
			return &s->vs[i];
	}
	return NULL;
}

static V *vfromasv(IAudioStreamVolume *asv)
{
	for (u32 i = 0; i < s->n; ++i) {
		if (s->asvs[i] == asv)
			return &s->vs[i];
	}
	return NULL;
}

#define HOOKCALL WINAPI

FNCOM(IAudioRenderClient, GetBuffer);
static HRESULT HOOKCALL h_GetBuffer(IAudioRenderClient *rc,
				    UINT32 NumFramesRequested, BYTE **ppData)
{
	HRESULT r = o_GetBuffer(rc, NumFramesRequested, ppData);
	V *v = vfromrc(rc);
	if (!v)
		goto ret;
	v->buf = *ppData;
ret:
	return r;
}

static bool sendbuf(V *v, Cmd cmd, const void *buf, u32 sz)
{
	if (!s->sfd)
		return false;
	struct sockaddr_in sinv = {
		.sin_family = AF_INET,
		.sin_port = htons(prg.port),
		.sin_addr.s_addr = prg.ip
	};
	u8 bufv[BUF_SZ], *b = bufv;
	enchdr(b, (Hdr){v->cfg, cmd});
	b += HDR_SZ;
	memcpy(b, s->name, s->namesz + 1);
	b += s->namesz + 1;
	memcpy(b, buf, sz);
	b += sz;
	u32 tosend = (u32)(b - bufv);
	int sent = sendto(s->sfd, (const char*)bufv, (int)tosend, 0,
			  (struct sockaddr*)&sinv, sizeof(sinv));
	if ((u32)sent < sz) {
		DEBUG("sent (%i) < to send (%u)", sent, sz);
		return false;
	}
	return true;
}

static bool sendlocked(V *v, u8 cmd, const void *buf, u32 sz)
{
	AcquireSRWLockExclusive(&s->srw);
	bool r = sendbuf(v, cmd, buf, sz);
	ReleaseSRWLockExclusive(&s->srw);
	return r;
}

FNCOM(IAudioRenderClient, ReleaseBuffer);
static HRESULT HOOKCALL h_ReleaseBuffer(IAudioRenderClient *rc,
					UINT32 NumFramesWritten, DWORD dwFlags)
{
	/* ideally copy bytes into another temp buffer, call the original, then
	 * check if the HRESULT was ok before sending */
	V *v = vfromrc(rc);
	if (!v || !v->buf || !s->sfd) {
		DEBUG("bad: %u, %u, %u", !v, v ? !v->buf : 0, !s->sfd);
		goto ret;
	}
	DWORD flags = dwFlags;
	dwFlags |= AUDCLNT_BUFFERFLAGS_SILENT;
	u32 sz = framesz(v->cfg) * NumFramesWritten;
	if (sz > BUF_SZ / 2) {
		DEBUG("bad: %u > %u", sz, BUF_SZ / 2);
		goto ret;
	}
	bool sent;
	if (flags & AUDCLNT_BUFFERFLAGS_SILENT)
		sent = sendlocked(v, CMD_SILENT, &sz, sizeof(sz));
	else
		sent = sendlocked(v, CMD_NONE, v->buf, sz);
	v->buf = NULL;
	u64 t = ns();
	if (sent && v->cnt % 200 == 0) {
		DEBUG("send (%u): %u bytes - %u us", v->cnt, sz,
		      (u32)(t - v->t) / 1000);
	}
	++v->cnt;
	v->t = t;
ret:
	return o_ReleaseBuffer(rc, NumFramesWritten, dwFlags);
}

FNCOM(IAudioRenderClient, Release);
static ULONG HOOKCALL h_Release(IAudioRenderClient *rc)
{
	V *v = vfromrc(rc);
	if (v) {
		AcquireSRWLockExclusive(&s->srw);
		u32 idx = (u32)(v - s->vs), last = --s->n;
		s->cs[idx] = s->cs[last];
		s->rcs[idx] = s->rcs[last];
		s->vs[idx] = s->vs[last];
		sendbuf(v, CMD_RM, NULL, 0);
		ReleaseSRWLockExclusive(&s->srw);
	}
	return o_Release(rc);
}

static bool cfgfrom(const WAVEFORMATEX *wf, u32 bufsz, Cfg *cfg)
{
	*cfg = (Cfg){
		.rate = wf->nSamplesPerSec,
		.bytes = wf->wBitsPerSample / 8,
		.channels = wf->nChannels,
		.isfloat = false,
		.id = rndid(),
		.bufsz = bufsz,
	};
	if (wf->wFormatTag == WAVE_FORMAT_EXTENSIBLE) {
		WAVEFORMATEXTENSIBLE *wfe = (void*)wf;
		GUID guid = wfe->SubFormat;
		if (IsEqualGUID(&guid, &KSDATAFORMAT_SUBTYPE_IEEE_FLOAT))
			cfg->isfloat = true;
		else if (!IsEqualGUID(&guid, &KSDATAFORMAT_SUBTYPE_PCM))
			return false;
	} else if (wf->wFormatTag == WAVE_FORMAT_IEEE_FLOAT) {
		cfg->isfloat = true;
	} else if (wf->wFormatTag != WAVE_FORMAT_PCM)
		return false;
	return true;
}

FNCOM(IAudioClient, Reset);
static HRESULT HOOKCALL h_Reset(IAudioClient *c)
{
	V *v = vfromc(c);
	if (v)
		sendlocked(v, CMD_RESET, NULL, 0);
	return o_Reset(c);
}

FNCOM(IAudioClient, Start);
static HRESULT HOOKCALL h_Start(IAudioClient *c)
{
	DEBUG("start");
	V *v = vfromc(c);
	if (v)
		sendlocked(v, CMD_START, NULL, 0);
	return o_Start(c);
}

FNCOM(IAudioClient, Stop);
static HRESULT HOOKCALL h_Stop(IAudioClient *c)
{
	DEBUG("stop");
	V *v = vfromc(c);
	if (v)
		sendlocked(v, CMD_PAUSE, NULL, 0);
	return o_Stop(c);
}

FNCOM(ISimpleAudioVolume, SetMasterVolume);
static HRESULT HOOKCALL h_SetMasterVolume(ISimpleAudioVolume *sav, float fLevel,
					  LPCGUID EventContext)
{
	HRESULT hr = o_SetMasterVolume(sav, fLevel, EventContext);
	V *v = vfromsav(sav);
	if (!v)
		return hr;
	u32 n = v->cfg.channels;
	f32 vols[MAX_CHANNELS];
	for (u32 i = 0; i < n; ++i)
		vols[i] = (v->mastervol = fLevel) * v->streamvols[i];
	sendlocked(v, CMD_VOL, vols, sizeof(*vols) * n);
	return hr;
}

FNCOM(ISimpleAudioVolume, SetMute);
static HRESULT HOOKCALL h_SetMute(ISimpleAudioVolume *sav, const BOOL bMute,
				  LPCGUID EventContext)
{
	HRESULT hr = o_SetMute(sav, bMute, EventContext);
	if (FAILED(hr))
		return hr;
	V *v = vfromsav(sav);
	if (!v)
		return hr;
	u32 n = v->cfg.channels;
	f32 vols[MAX_CHANNELS];
	for (u32 i = 0; i < n; ++i) {
		vols[i] = (v->mastervol = bMute ? 0.0f : 1.0f)
			* v->streamvols[i];
	}
	sendlocked(v, CMD_VOL, vols, sizeof(*vols) * n);
	return hr;
}

FNCOM(IAudioStreamVolume, SetChannelVolume);
static HRESULT HOOKCALL h_SetChannelVolume(
	IAudioStreamVolume *asv, UINT32 dwIndex, float fLevel)
{
	HRESULT hr = o_SetChannelVolume(asv, dwIndex, fLevel);
	if (FAILED(hr))
		return hr;
	V *v = vfromasv(asv);
	if (!v)
		return hr;
	v->streamvols[dwIndex] = fLevel;
	u32 n = dwIndex + 1;
	f32 vols[MAX_CHANNELS];
	for (u32 i = 0; i < n; ++i)
		vols[i] = v->streamvols[i]  * v->mastervol;
	sendlocked(v, CMD_VOL, vols, sizeof(*vols) * n);
	return hr;
}

FNCOM(IAudioStreamVolume, SetAllVolumes);
static HRESULT HOOKCALL h_SetAllVolumes(
	IAudioStreamVolume *asv, UINT32 dwCount, const float *pfVolumes)
{
	HRESULT hr = o_SetAllVolumes(asv, dwCount, pfVolumes);
	if (FAILED(hr))
		return hr;
	V *v = vfromasv(asv);
	if (!v)
		return hr;
	u32 n = MIN(v->cfg.channels, dwCount);
	f32 vols[MAX_CHANNELS];
	for (u32 i = 0; i < n; ++i)
		vols[i] = (v->streamvols[i] = pfVolumes[i]) * v->mastervol;
	sendlocked(v, CMD_VOL, vols, sizeof(*vols) * n);
	return hr;
}

static void makesock(void)
{
	WSADATA ws;
	CHK(WSAStartup(MAKEWORD(2, 2), &ws) == 0, "init winsock2");
	s->sfd = socket(AF_INET, SOCK_DGRAM, 0);
	char hostname[MAX_NAME];
	*hostname = '\0';
	if (gethostname(hostname, MAX_NAME - 1) == 0) {
		int n = snprintf(s->name, MAX_NAME - 1, "%s/%s", hostname,
				 prg.name);
		s->namesz = MIN(MAX_NAME - 1, (u32)n);
	}
}

static void make(void)
{
	*s = (S){
		.n = 0,
		.cs = {0},
		.rcs = {0},
		.vs = {0},
		.srw = SRWLOCK_INIT,
		.name = {0},
		.namesz = 0
	};
	makesock();
}

static bool hookclient(IAudioClient *c, IAudioRenderClient *rc,
		       ISimpleAudioVolume *sav, IAudioStreamVolume *asv)
{
	bool ok = false;
	MH_STATUS mh = MH_OK;
#define HOOK_(p, fn)  mh |= MH_CreateHook(p, (void*)h_##fn,(void**)&o_##fn)
#define HOOK(fn) HOOK_(fn, fn)
#define HOOKCOM(p, fn)  HOOK_(p->lpVtbl->fn, fn)
	HOOKCOM(c, Start);
	HOOKCOM(c, Stop);
	HOOKCOM(c, Reset);
	HOOKCOM(rc, GetBuffer);
	HOOKCOM(rc, ReleaseBuffer);
	HOOKCOM(rc, Release);
	HOOKCOM(sav, SetMasterVolume);
	HOOKCOM(sav, SetMute);
	HOOKCOM(asv, SetChannelVolume);
	HOOKCOM(asv, SetAllVolumes);
	TRY(mh == MH_OK, ecreatehook);
	mh = MH_EnableHook(MH_ALL_HOOKS);
	TRY(mh == MH_OK, eenablehook);
	ok = true;
eenablehook:
	if (!ok)
		MH_DisableHook(MH_ALL_HOOKS);
ecreatehook:
	if (!ok)
		F(rc, Release);
	return ok;
}

static void logv(V *v)
{
	(void)v;
#ifdef SP_DEBUG
	if (!conready)
		return;
	DEBUG("rate: %u, bits: %u, channels: %u, type: %s", v->cfg.rate,
	      v->cfg.bytes * 8, v->cfg.channels,
	      v->cfg.isfloat ? "f32" : "pcm");
#endif
}

#ifdef SP_DEBUG
static void makecon(void)
{
	AllocConsole();
	freopen("CONOUT$", "w", LOGFILE);
	SetConsoleOutputCP(CP_UTF8);
	HANDLE con = CreateFile("CONOUT$",  GENERIC_READ | GENERIC_WRITE,
				FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
				OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	SetStdHandle(STD_OUTPUT_HANDLE, con);
	SetStdHandle(STD_ERROR_HANDLE, con);
	DWORD mode;
	GetConsoleMode(con, &mode);
	SetConsoleMode(con, (mode | ENABLE_PROCESSED_OUTPUT |
			     ENABLE_VIRTUAL_TERMINAL_PROCESSING));
	CONSOLE_FONT_INFOEX cfi = {
		.cbSize = sizeof(cfi),
		.dwFontSize.Y = 18,
		.FontFamily = FF_DONTCARE,
		.FontWeight = FW_NORMAL,
		.FaceName = L"Cascadia Code"
	};
	SetCurrentConsoleFontEx(con, FALSE, &cfi);
	conready = true;
}

static DWORD WINAPI makedbg(void)
{
	makecon();
	DEBUG("sp (send | low-latency | win)");
	struct in_addr ia = {.S_un.S_addr = prg.ip};
	DEBUG("sending to %s:%u", inet_ntoa(ia), prg.port);
	if (s->n)
		logv(&s->vs[0]);
	return 0;
}
#endif

static V* addclient(u32 idx, IAudioClient *c, IAudioRenderClient *rc,
		    ISimpleAudioVolume *sav, IAudioStreamVolume *asv, V *v)
{
	s->cs[idx] = c;
	s->rcs[idx] = rc;
	s->savs[idx] = sav;
	s->asvs[idx] = asv;
	s->vs[idx] = *v;
	logv(&s->vs[idx]);
	return &s->vs[idx];
}

FNCOM(IAudioClient, Initialize);
static HRESULT HOOKCALL h_Initialize(
	IAudioClient *c, AUDCLNT_SHAREMODE ShareMode, DWORD StreamFlags,
	REFERENCE_TIME hnsBufferDuration, REFERENCE_TIME hnsPeriodicity,
	const WAVEFORMATEX *pFormat, LPCGUID AudioSessionGuid)
{
	bool skip = true;
	if (!c)
		goto gskip;
	if (StreamFlags & AUDCLNT_SHAREMODE_EXCLUSIVE)
		goto gskip;
	/* Initialize gets hooked before we definitely get the result of
	 * loadstate so lazily check the status here and if the application is
	 * blacklisted forward the call to the original and the hook should get
	 * removed soon by dllmain */
	if (loadstate == LOAD_WAIT) {
		/* if dllmain gets the result of loadstate first and it is not
		 * OK, loadevt will be set to NULL */
		if (!loadevt)
			goto gskip;
		WaitForSingleObject(loadevt, INFINITE);
	}
	if (loadstate == LOAD_DISABLE)
		goto gskip;
	bool dohook = false;
	if (!s) {
		s = &sstore;
		make();
#ifdef SP_DEBUG
		makedbg();
#endif
		dohook = true;
	}
	u32 potidx = s->n;
	if (potidx == MAX_CLIENTS)
		goto gskip;
	skip = false;
gskip:;
	V v;
	Cfg cfg;
	if (!skip && cfgfrom(pFormat, 0, &cfg)) {
		v = (V){.cfg = cfg};
		for (u32 i = 0; i < MAX_CHANNELS; ++i)
			v.streamvols[i] = 1.0f;
		v.mastervol = 1.0f;
		sendbuf(&v, CMD_PREP, NULL, 0);
	}
	HRESULT hr = o_Initialize(c, ShareMode, StreamFlags, hnsBufferDuration,
				  hnsPeriodicity, pFormat, AudioSessionGuid);
	if (FAILED(hr) || skip || vfromc(c))
		goto ret;
	IAudioRenderClient *rc;
	F(c, GetService, &IID_IAudioRenderClient, (void*)&rc);
	ISimpleAudioVolume *sav;
	F(c, GetService, &IID_ISimpleAudioVolume, (void*)&sav);
	IAudioStreamVolume *asv;
	F(c, GetService, &IID_IAudioStreamVolume, (void*)&asv);
	if (dohook && !hookclient(c, rc, sav, asv)) {
		DEBUG("failed to hook client");
		goto ret;
	}
	UINT32 bufsz;
	F(c, GetBufferSize, &bufsz);
	u32 fsz = framesz(cfg);
	if (bufsz * fsz > BUF_SZ * 9 / 10) {
		DEBUG("buffer required too big for reciever");
		goto ret;
	}
	v.cfg.bufsz = bufsz;
	fd_set fds;
	FD_ZERO(&fds);
	FD_SET(s->sfd, &fds);
	i32 timeoutms = 200;
	struct timeval tv = {.tv_usec = timeoutms * 1000};
	int r = select((int)s->sfd + 1, &fds, NULL, NULL, &tv);
	if (r <= 0) {
		DEBUG("[%u]: timeout for response from receiver", potidx);
		goto ret;
	}
	u8 res;
	recvfrom(s->sfd, (char*)&res, sizeof(res), 0, NULL, NULL);
	if (res != 1) {
		DEBUG("[%u]: wrong response from receiver", potidx);
		goto ret;
	}
	AcquireSRWLockExclusive(&s->srw);
	if (s->n != MAX_CLIENTS)
		addclient(s->n++, c, rc, sav, asv, &v);
	ReleaseSRWLockExclusive(&s->srw);
ret:
	return hr;
}

#define MSG(s, ...) do { \
	char msgbuf[1024]; \
	sprintf(msgbuf, s, ##__VA_ARGS__); \
	MessageBoxA(NULL, msgbuf, "", MB_OK); } while (0)

static bool reqpipe(OVERLAPPED *wop, u64 *sentms)
{
	bool ok = false;
	pipe = CreateFileW(PIPE_NAME, GENERIC_READ | GENERIC_WRITE,
				FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
				OPEN_EXISTING, FILE_FLAG_OVERLAPPED, NULL);
	TRY(pipe != INVALID_HANDLE_VALUE, ret);
	_Static_assert(PIPE_BUF_SZ > sizeof(prg.namebuf), "");
	TRY(WriteFile(pipe, prg.name, prg.namesz, NULL, wop), ret);
	*sentms = ms();
	ok = true;
ret:
	return ok;
}

static bool handlepipereply(u64 sentms)
{
	LlInfo info;
	OVERLAPPED rop = {0};
	BOOL r = ReadFile(pipe, &info, sizeof(info), NULL, &rop);
	if (!r && GetLastError() != ERROR_IO_PENDING)
		return false;
	DWORD read;
	u64 waitms = 30, to = sentms + waitms, t = ms(),
	    wait = to - MIN(t, to);
	r = GetOverlappedResultEx(pipe, &rop, &read, (DWORD)wait, FALSE);
	if (!r || read != sizeof(info))
		return false;
	prg.ip = info.ip;
	prg.port = info.port;
	return !info.disable;
}

static DWORD WINAPI loadpcfg(LPVOID p)
{
	(void)p;
	u64 sentms;
	OVERLAPPED wop = {0};
	bool r = reqpipe(&wop, &sentms);
	TRY(r, rej);
	if (strstr(prg.name, "sp-"))
		goto rej;
	r = handlepipereply(sentms);
	TRY(r, rej);
	loadstate = LOAD_OK;
	SetEvent(loadevt);
rej:
	if (pipe != INVALID_HANDLE_VALUE)
		CloseHandle(pipe);
	return 0;
}

static void getprgname(void)
{
	prg.namesz = GetModuleFileNameA(NULL, prg.namebuf, MAX_PATH);
	if (!prg.namesz)
		return;
	u32 lastslash = 0, at = 0;
	for (char *c = prg.namebuf; *c; ++c, ++at)
		lastslash = *c == '\\' ? at : lastslash;
	prg.name = prg.namebuf + lastslash + 1;
	prg.namesz -= lastslash + 1;
	for (char *c = prg.name + prg.namesz - 1; c != prg.name; --c) {
		if (*c == '.') {
			*c = '\0';
			break;
		}
	}
}

static DWORD WINAPI dllmain(void *p)
{
	HINSTANCE inst = p;
	bool ok = false;
	loadevt = CreateEventW(NULL, TRUE, FALSE, NULL);
	/* doing all of this thread stuff because we need to hook Initialize
	 * fast before the program calls it */
	HANDLE th = CreateThread(NULL, 0, loadpcfg, NULL, 0, NULL);
	getprgname();
	CoInitializeEx(NULL, COINIT_MULTITHREADED);
	MH_Initialize();
	HRESULT hr;
	IMMDeviceEnumerator *devenum;
	hr = CoCreateInstance(&CLSID_MMDeviceEnumerator, NULL, CLSCTX_ALL,
			      &IID_IMMDeviceEnumerator, (void**)&devenum);
	TRYH(hr, end);
	IMMDevice *dev;
	hr = F(devenum, GetDefaultAudioEndpoint, eRender, eConsole, &dev);
	TRYH(hr, edev);
	IAudioClient *c;
	hr = F(dev, Activate, &IID_IAudioClient, CLSCTX_ALL, NULL, (void**)&c);
	TRYH(hr, eclient);
	MH_STATUS mh = MH_OK;
	HOOKCOM(c, Initialize);
	TRY(mh == MH_OK, ecreatehook);
	mh = MH_EnableHook(c->lpVtbl->Initialize);
	TRY(mh == MH_OK, eenablehook);
	WaitForSingleObject(th, INFINITE);
	if (loadstate != LOAD_OK) {
		HANDLE evt = loadevt;
		loadevt = NULL;
		CloseHandle(evt);
	} else
		ok = true;
eenablehook:
	if (!ok)
		MH_RemoveHook(&c->lpVtbl->Initialize);
ecreatehook:
	F(c, Release);
eclient:
	F(dev, Release);
edev:
	F(devenum, Release);
end:
	if (!ok) {
		MH_Uninitialize();
		CoUninitialize();
		FreeLibraryAndExitThread((HMODULE)inst, 0);
	}
	return 0;
}

BOOL WINAPI DllMain(HINSTANCE dll, DWORD reason, void *reserved)
{
	(void)dll, (void)reserved;
	if (reason == DLL_PROCESS_ATTACH)
		CreateThread(NULL, 0, dllmain, dll, 0, NULL);
	return TRUE;
}
