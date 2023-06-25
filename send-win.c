/* Copyright (C) 2023 hyperfunny88
 * SPDX-License-Identifier: GPL-3.0-or-later */

#include "util.h"

enum {MAX_BLACKLIST = 512, MAX_BLACKLIST_LOAD = MAX_BLACKLIST * 2/3};

typedef struct {
	IMMDeviceEnumerator *devenum;
	IMMDevice *dev;
	IAudioClient *client, *silc;
	IAudioCaptureClient *capt;
	IAudioRenderClient *silrc;
	char name[MAX_NAME];
	Cfg cfg;
	HANDLE evt, silevt;
	Socket sfd;
	u32 blacklist[MAX_BLACKLIST];
	u32 ip, evtto, namesz, silbufsz, silto;
	u16 port;
} S;

static IMMDevice *defdev(S *s)
{
	HRESULT hr;
	IMMDevice *dev;
	hr = F(s->devenum, GetDefaultAudioEndpoint, eRender, eConsole, &dev);
	CHKH(hr, "get default audio endpoint");
	return dev;
}

static void logdev(IMMDevice *dev)
{
	IPropertyStore *ps;
	if (FAILED(F(dev, OpenPropertyStore, STGM_READ, &ps)))
		DIE("failed to open property store");
	PROPVARIANT name;
	PropVariantInit(&name);
	F(ps, GetValue, &PKEY_DeviceInterface_FriendlyName, &name);
	char buf[512];
	int n = WideCharToMultiByte(CP_UTF8, 0, name.pwszVal,
				    (int)wcslen(name.pwszVal), buf, sizeof(buf),
				    NULL, NULL);
	INFO("device: %.*s", n, buf);
	PropVariantClear(&name);
	F(ps, Release);
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

static void makeclient(S *s)
{
	HRESULT hr;
	hr = F(s->dev, Activate, &IID_IAudioClient, CLSCTX_ALL, NULL,
	       (void**)&s->client);
	CHKH(hr, "get audio client");
	REFERENCE_TIME def, min;
	hr = F(s->client, GetDevicePeriod, &def, &min);
	CHKH(hr, "get device period");
	s->evtto = (u32)(min / 1000);
	WAVEFORMATEX *wf;
	hr = F(s->client, GetMixFormat, &wf);
	CHKH(hr, "get mix format");
	hr = F(s->client, Initialize, AUDCLNT_SHAREMODE_SHARED,
	       (AUDCLNT_STREAMFLAGS_LOOPBACK |
		AUDCLNT_STREAMFLAGS_EVENTCALLBACK),
	       min, 0, wf, NULL);
	CHKH(hr, "make audio client");
	UINT32 bufsz;
	hr = F(s->client, GetBufferSize, &bufsz);
	CHKH(hr, "get buffer size");
	Cfg cfg;
	CHK(cfgfrom(wf, (u32)bufsz, &cfg), "build cfg");
	s->cfg = cfg;
	INFO("config: id = %08X, rate = %u, bits = %u, channels = %u, "
	     "type = %s, buffer size: %u frames (%u ms)", cfg.id, cfg.rate,
	     cfg.bytes * 8, cfg.channels, cfg.isfloat ? "f32" : "pcm",
	     cfg.bufsz / framesz(cfg), cfg.bufsz * 1000 / cfg.rate);
}

static void makecapt(S *s)
{
	HRESULT hr;
	hr = F(s->client, GetService, &IID_IAudioCaptureClient,
	       (void**)&s->capt);
	CHKH(hr, "make audio capture client");
	s->evt = CreateEventW(NULL, FALSE, FALSE, NULL);
	CHK(s->evt, "make event");
	hr = F(s->client, SetEventHandle, s->evt);
	CHKH(hr, "set event handle");
}

static void makesock(S *s)
{
	WSADATA ws;
	CHK(WSAStartup(MAKEWORD(2, 2), &ws) == 0, "make winsock2");
	s->sfd = socket(AF_INET, SOCK_DGRAM, 0);
	CHK(s->sfd > 0, "make socket");
}

static HANDLE makepipe(void)
{
	HANDLE pipe = CreateNamedPipeW(
		PIPE_NAME, PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED,
		PIPE_TYPE_BYTE | PIPE_READMODE_BYTE,
		PIPE_UNLIMITED_INSTANCES, PIPE_BUF_SZ, PIPE_BUF_SZ,
		INFINITE, NULL);
	if (pipe == INVALID_HANDLE_VALUE)
		DIE("failed to make new pipe event");
	return pipe;
}

typedef enum {
	PIPESTATE_NONE,
	PIPESTATE_CON,
	PIPESTATE_PREP_READ,
	PIPESTATE_PREP_WRITE,
} PipeState;

typedef struct {
	HANDLE pipe;
	u8 buf[PIPE_BUF_SZ];
	OVERLAPPED *op;
	u8 ps;
} Client;

static char *trimws(char *s)
{
	for (char *p = s + strlen(s) - 1; isspace(*p); *p-- = '\0');
	return s;
}

static bool servepipe(S *s, Client *c, u32 idx)
{
	(void)idx;
	DWORD tmp;
	BOOL or = GetOverlappedResult(c->pipe, c->op, &tmp, TRUE);
#	define PREP_READ_MSG "failed to read prep data from low-latency client"
	switch (c->ps) {
	case PIPESTATE_CON: {
		if (!or) {
			WARN("failed to connect to low-latency client");
			return false;
		}
		if (!ReadFile(c->pipe, c->buf, PIPE_BUF_SZ, NULL, c->op)) {
			WARN(PREP_READ_MSG);
			return false;
		}
		c->ps = PIPESTATE_PREP_READ;
		break;
	}
	case PIPESTATE_PREP_READ: {
		if (!or) {
			WARN(PREP_READ_MSG);
			return false;
		}
		char *name = (char*)c->buf;
		name[PIPE_BUF_SZ - 1] = '\0';
		if (!*trimws(name)) {
			WARN("invalid low-latency hook name");
			return false;
		}
		/* hash-table search; should help when large number of entries
		 * in blacklist */
		u32 h = (u32)mix(strhash(name)), p = h % MAX_BLACKLIST;
		bool disable = false;
		for (; s->blacklist[p] && !(disable = (s->blacklist[p] == h));
		     ++p);
		LlInfo info = {
			.ip = s->ip,
			.port = s->port,
			.disable = disable
		};
		if (!WriteFile(c->pipe, &info, sizeof(info), NULL, c->op)){
			WARN("failed to send reply back to low-latency hook");
			return false;
		}
		c->ps = PIPESTATE_PREP_WRITE;
		if (!disable)
			INFO("new low-latency hooked program: %s", name);
		else {
			INFO("blacklisted low-latency hooked program disabled: "
			     "%s", name);
		}
	} }
	return true;
}

static DWORD WINAPI pipeproc(LPVOID p)
{
	S *s = p;
	enum {MAX_CLIENTS = 128};
	Client cs[MAX_CLIENTS], tmp;
	HANDLE evtsb[MAX_CLIENTS + 1], *evts = evtsb + 1;
	u64 lastt[MAX_CLIENTS];
	u32 n = 0;
	const u32 timeout = PIPE_TIMEOUT_MS * 3 / 2;
	u64 nextto = ms() + timeout;
	bool havetmp = false;
	for (u64 t; t = ms(), true;) {
		if (!havetmp && n < MAX_CLIENTS) {
			*evtsb = CreateEventW(NULL, FALSE, TRUE, NULL);
			if (!*evtsb)
				DIE("failed to make new pipe event");
			tmp = (Client){
				.pipe = makepipe(),
				.op = malloc(sizeof(*tmp.op)),
				.ps = PIPESTATE_CON
			};
			*tmp.op = (OVERLAPPED){.hEvent = *evtsb};
			ConnectNamedPipe(tmp.pipe, tmp.op);
			havetmp = true;
		}
		u32 ismax = n == MAX_CLIENTS;
		DWORD towait = (DWORD)(nextto - MIN(t, nextto)),
		      waitr = WaitForMultipleObjects(n + !ismax, evtsb + ismax,
						     FALSE, towait);
		if (waitr == WAIT_FAILED)
			DIE("failed to wait");
		else if (waitr == WAIT_TIMEOUT)
			goto chkto;
		u32 idxb = waitr - WAIT_OBJECT_0, idx = idxb - 1;
		Client *c;
		if (idxb == 0)
			c = &tmp;
		else
			c = &cs[idx];
#		define CLOSE(idx) do { \
			CloseHandle(cs[idx].pipe); \
			CloseHandle(evts[idx]); \
			u32 to = --n; \
			cs[idx] = cs[to]; \
			evts[idx] = evts[to]; \
			lastt[idx] = lastt[to]; } while (0)
		bool r = servepipe(s, c, !idxb ? n : idx);
		if (idxb == 0) {
			idx = n++;
			cs[idx] = tmp;
			evts[idx] = *evtsb;
			havetmp = false;
		}
		lastt[idx] = t;
		if (!r)
			CLOSE(idx);
	chkto:
		if (ms() < nextto)
			continue;
		for (u32 i = 0; i < n; ++i) {
			if (t < lastt[i] + timeout)
				continue;
			CLOSE(i);
		}
		nextto = ms() + timeout;
#		undef CLOSE
	}
}

static void loadblacklist(S *s, const char *blacklist)
{
	FILE *f = fopen(blacklist, "r");
	if (!f)
		DIE("error loading blacklist file");
	u32 n = 0;
	while (n < MAX_BLACKLIST_LOAD) {
		char buf[1024];
		if (!fgets(buf, sizeof(buf), f))
			break;
		if (!*trimws(buf))
			continue;
		u32 h = (u32)mix(strhash(buf)), p = h % MAX_BLACKLIST;
		for (; s->blacklist[p]; ++p);
		s->blacklist[p] = h;
		++n;
	}
	if (n == MAX_BLACKLIST_LOAD)
		WARN("too many entries in blacklist");
	fclose(f);
	INFO("loaded %u entries into blacklist", n);
}

static void make(S *s, u32 ip, u16 port, const char *blacklist)
{
	*s = (S){
		.blacklist = {0},
		.ip = ip,
		.port = port,
		.name = {0},
		.namesz = 0
	};
	timeBeginPeriod(1);
	makesock(s);
	char hostname[MAX_NAME];
	gethostname(hostname, MAX_NAME - 1);
	int n = snprintf(s->name, MAX_NAME - 1, "%s/sp-send", hostname);
	s->namesz = MIN((u32)n, MAX_NAME - 1);
	if (blacklist)
		loadblacklist(s, blacklist);
	CreateThread(NULL, 0, pipeproc, s, 0, NULL);
}

static void makecapture(S *s)
{
	HRESULT hr;
	hr = CoInitializeEx(NULL, COINIT_MULTITHREADED);
	CHKH(hr, "make com instance");
	hr = CoCreateInstance(&CLSID_MMDeviceEnumerator, NULL, CLSCTX_ALL,
			      &IID_IMMDeviceEnumerator, (void**)&s->devenum);
	CHKH(hr, "make device enumerator");
	CHK(s->dev = defdev(s), "get default device");
	logdev(s->dev);
	makeclient(s);
	makecapt(s);
	CHKH(F(s->client, Start), "start audio client");
	SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);
}

static void makesilent(S *s)
{
	HRESULT hr;
	hr = F(s->dev, Activate, &IID_IAudioClient, CLSCTX_ALL, NULL,
	       (void**)&s->silc);
	CHKH(hr, "get silent audio client");
	REFERENCE_TIME def, min;
	hr = F(s->silc, GetDevicePeriod, &def, &min);
	CHKH(hr, "get silent device period");
	s->silto = (u32)(def / 1000);
	WAVEFORMATEX *wf;
	hr = F(s->silc, GetMixFormat, &wf);
	bool sup = (wf->wFormatTag == WAVE_FORMAT_IEEE_FLOAT ||
		    (wf->wFormatTag == WAVE_FORMAT_EXTENSIBLE &&
		     IsEqualGUID(&((WAVEFORMATEXTENSIBLE*)wf)->SubFormat,
				 &KSDATAFORMAT_SUBTYPE_IEEE_FLOAT)));
	if (!sup)
		DIE("unsupported mix format");
	CHKH(hr, "get silent mix format");
	hr = F(s->silc, Initialize, AUDCLNT_SHAREMODE_SHARED,
	       AUDCLNT_STREAMFLAGS_EVENTCALLBACK, def, 0, wf, NULL);
	CHKH(hr, "make silent audio client");
	UINT32 bufsz;
	hr = F(s->silc, GetBufferSize, &bufsz);
	CHKH(hr, "get silent buffer size");
	s->silbufsz = (u32)bufsz;
	hr = F(s->silc, GetService, &IID_IAudioRenderClient,
	       (void**)&s->silrc);
	CHKH(hr, "make silent audio render client");
	s->silevt = CreateEventW(NULL, FALSE, FALSE, NULL);
	CHK(s->silevt, "make silent event");
	hr = F(s->silc, SetEventHandle, s->silevt);
	CHKH(hr, "set silent event handle");
	CHKH(F(s->silc, Start), "start silent audio client");

}

static DWORD WINAPI silentproc(LPVOID p)
{
	S *s = p;
	while (true) {
		DWORD r = WaitForSingleObjectEx(s->silevt, s->silto, FALSE);
		if (r == WAIT_FAILED)
			continue;
		u32 padding;
		HRESULT hr = F(s->silc, GetCurrentPadding, &padding);
		if (FAILED(hr))
			continue;
		u32 nframes = s->silbufsz - padding;
		f32 *frames;
		hr = F(s->silrc, GetBuffer, nframes, (BYTE**)&frames);
		if (FAILED(hr))
			continue;
		F(s->silrc, ReleaseBuffer, nframes, AUDCLNT_BUFFERFLAGS_SILENT);
	}
	return 0;
}

static void runsilent(S *s)
{
	CreateThread(NULL, 0, silentproc, s, 0, NULL);
}

static bool iszero(u8 *b, u32 sz)
{
	enum {N = 8};
	u8 a[N] = {0};
	for (u32 i = 0; i < sz; ++i)
		a[i % N] |= b[i];
	u8 v = 0;
	for (u32 i = 0; i < N; ++i)
		v |= a[i];
	return !v;
}

static void record(S *s)
{
	DWORD r = WaitForSingleObjectEx(s->evt, s->evtto, FALSE);
	if (r == WAIT_FAILED) {
		WARN("failed to wait for buffer");
		return;
	}
	while (true) {
		u32 packsz;
		HRESULT hr = F(s->capt, GetNextPacketSize, &packsz);
		if (FAILED(hr))
			DIE("get next packet size");
		if (packsz == 0)
			return;
		DWORD flags;
		u8 *frames;
		u32 read;
		hr = F(s->capt, GetBuffer, (BYTE**)&frames, &read, &flags,
		       NULL, NULL);
		if (hr != S_OK)
			DIE("get buffer");
		u8 buf[BUF_SZ], *b = buf + HDR_SZ;
		memcpy(b, s->name, s->namesz + 1);
		b += s->namesz + 1;
		u32 sz = read * framesz(s->cfg);
		u8 cmd = CMD_NONE;
		if (flags & AUDCLNT_BUFFERFLAGS_SILENT || iszero(frames, sz)) {
			memcpy(b, &sz, sizeof(sz));
			sz = sizeof(sz);
			cmd = CMD_SILENT;
		} else
			memcpy(b, frames, sz);
		enchdr(buf, (Hdr){s->cfg, cmd});
		struct sockaddr_in sin = {
			.sin_family = AF_INET,
			.sin_port = htons(s->port),
			.sin_addr.s_addr = s->ip
		};
		u32 tosend = sz + s->namesz + 1 + HDR_SZ;
		int sent = sendto(s->sfd, (const char*)buf, (int)tosend, 0,
				  (struct sockaddr*)&sin, sizeof(sin));
		if ((u32)sent < tosend)
			WARN("sent (%i) < to send (%u)", (int)sent, tosend);
		F(s->capt, ReleaseBuffer, read);
	}
}

static void setupcon(void)
{
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
}

static void help(const char *argv)
{
	const char *s =
		"options:\n\t"
		"-m\t(mute, only use as connector with sp-send-ll)\n\t"
		"-b\tblacklist file for sp-send-ll (line separated, without "
		".exe extension)\n\t"
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
	setupcon();
	INFO("sp (send | win)");
	if (argc < 3)
		HELP();
	bool mute = false;
	const char *blacklist = NULL;
	for (int i = 3; i < argc; ++i) {
		if (*argv[i] == '~')
			continue;
		else if (*argv[i] != '-')
			HELP();
		if (argv[i][1] == 'm')
			mute = true;
		else if (argv[i][1] == 'b' && i + 1 < argc)
			blacklist = argv[++i];
		else
			HELP();
	}
	char *end;
	S s;
	make(&s, inet_addr(argv[1]), (u16)strtol(argv[2], &end, 10),
	     blacklist);
	if (!mute) {
		makecapture(&s);
		makesilent(&s);
		/* prevent the device from sleeping by flushing silent audio */
		runsilent(&s);
		while (true)
			record(&s);
	} else {
		INFO("audio muted, keeping alive as connector");
		Sleep(INFINITE);
	}
	INFO("ok");
	return 0;
}
