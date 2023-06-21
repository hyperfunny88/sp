/* Copyright (C) 2023 hyperfunny88
 * SPDX-License-Identifier: GPL-3.0-or-later */

#pragma once

#ifndef _POSIX_C_SOURCE
#	define _POSIX_C_SOURCE 200809L
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>
#include <time.h>
#include <math.h>
#include <pthread.h>

#ifdef __unix__
#	include <unistd.h>
#	include <netdb.h>
#	include <sys/socket.h>
#	include <sys/types.h>
#	include <sys/time.h>
#elif defined(_WIN32)
#	define WIN32_LEAN_AND_MEAN
#	define NOMINMAX
#	include <initguid.h>
#	include <audioclient.h>
#	include <mmdeviceapi.h>
#	include <winsock2.h>
#	include <ws2tcpip.h>
#	include <timeapi.h>
#	undef ERROR
#else
#	error unsupported platform
#endif

typedef int8_t i8;
typedef uint8_t u8;
typedef int16_t i16;
typedef uint16_t u16;
typedef int32_t i32;
typedef uint32_t u32;
typedef int64_t i64;
typedef uint64_t u64;
typedef size_t usz;
typedef float f32;
typedef double f64;

#define COUNTOF(x) (sizeof(x) / sizeof(*x))
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define COLOR(c, h, p, s, ...) \
      fprintf(LOGFILE, "\x1B[" c "m\x1B[1m" h "\x1B[90m\x1B[1m" p "%s(%u)" \
              ":%s:\x1B[0m\t" s "\n", __FILE__, __LINE__, __func__, \
	      ##__VA_ARGS__)
#define INFO(s, ...) COLOR("32", "info", "    ", s, ##__VA_ARGS__)
#define WARN(s, ...) COLOR("33", "warn", "    ", s, ##__VA_ARGS__)
#define ERROR(s, ...) COLOR("31", "error", "   ", s, ##__VA_ARGS__)
#ifdef SP_DEBUG
#	define LOGFILE stderr
#	define DEBUG(s, ...) COLOR("34", "debug", "   ", s, ##__VA_ARGS__)
#else
#	define LOGFILE stdout
#	define DEBUG(...)
#endif
#define ABORT() exit(1)
#define DIE(s, ...) do { \
	ERROR(s, ##__VA_ARGS__); \
	ABORT(); } while (0)
#define CHK(x, s, ...) do { \
	if (UNLIKELY(!(x))) { \
		COLOR("31", "fail", "    ", s, ##__VA_ARGS__); ABORT(); } \
	else COLOR("32", "ok", "      ", s, ##__VA_ARGS__); } while (0)
#define LIKELY(x) __builtin_expect((x), 1);
#define UNLIKELY(x) __builtin_expect((x), 0)
#ifndef NDEBUG
#	define ASSERT(x) assert(x)
#else
#	define ASSERT(x) (void)(x)
#endif

typedef enum {
	CMD_NONE = 0,
	CMD_RESET,
	CMD_PAUSE,
	CMD_START,
	CMD_PREP,
	CMD_RM,
	CMD_VOL,
	CMD_SILENT
} Cmd;

typedef struct {
	u32 rate, bytes;
	bool isfloat;
	u32 channels, chnmask /* unused */, id, bufsz;
} Cfg;

typedef struct {
	Cfg cfg;
	Cmd cmd; /* want to pack nicly into 16 bytes */
} Hdr;

#define WRITE(p, v) memcpy(p, v, sizeof(*v)); p += sizeof(*v)
#define READ(v, p) memcpy(v, p, sizeof(*v)); p += sizeof(*v)

enum {HDR_SZ = 4 + 4 + 4 + 4};
enum {MAX_NAME = 256};

inline static void enchdr(u8 *out, Hdr hdr)
{
	u8 *o = out;
	u32 v = 0;
	--hdr.cfg.bytes; /* encode as bytes-1 which fits into 2 bits */
#	define B(x, n) v = (v << n) | (x & ((1 << n) - 1))
	B(hdr.cfg.rate, 18); /* 18 */
	B(hdr.cfg.bytes, 2); /* 20 */
	B(hdr.cfg.isfloat, 1); /* 21 */
	B(hdr.cfg.channels, 3); /* 24 */
	B(hdr.cfg.chnmask, 8); /* 32 */
#	undef B
	v = htonl(v);
	WRITE(o, &v);
	hdr.cfg.id = htonl(hdr.cfg.id);
	hdr.cfg.bufsz = htonl(hdr.cfg.bufsz);
	WRITE(o, &hdr.cfg.id);
	WRITE(o, &hdr.cfg.bufsz);
	WRITE(o, &hdr.cmd);
	ASSERT((u32)(o - out) == HDR_SZ);
}

inline static void dechdr(const u8 *b, Hdr *hdr)
{
	*hdr = (Hdr){0};
	u32 v;
	READ(&v, b);
	v = ntohl(v);
#	define B(x, n) x = v & ((1 << n) - 1); v >>= n
	B(hdr->cfg.chnmask, 8);
	B(hdr->cfg.channels, 3);
	B(hdr->cfg.isfloat, 1);
	B(hdr->cfg.bytes, 2);
	B(hdr->cfg.rate, 18);
#	undef B
	++hdr->cfg.bytes;
	READ(&hdr->cfg.id, b);
	READ(&hdr->cfg.bufsz, b);
	READ(&hdr->cmd, b);
	hdr->cfg.id = ntohl(hdr->cfg.id);
	hdr->cfg.bufsz = ntohl(hdr->cfg.bufsz);
}

inline static u32 framesz(Cfg cfg)
{
	return cfg.channels * cfg.bytes;
}

#ifndef K_BUF_SZ
#	define K_BUF_SZ 9 * (32768 + 16384)
#endif
enum {MAX_CHANNELS = 16};
enum {BUF_SZ = K_BUF_SZ};
typedef struct {
	u8 buf[BUF_SZ];
	volatile u32 r, w;
} RingBuf;

inline static void ringbuf_set(RingBuf *rb, u8 c, u32 sz)
{
	u32 w = rb->w;
	sz = MIN(sz, BUF_SZ);
	u32 sz0 = MIN(sz, BUF_SZ - w),
	    sz1 = sz - sz0;
	memset(rb->buf + w, c, sz0);
	if (sz1)
		memset(rb->buf, c, sz1);
	rb->w = (w + sz) % BUF_SZ;
}

inline static void ringbuf_write(RingBuf *rb, const void *p, u32 sz)
{
	u32 w = rb->w;
	sz = MIN(sz, BUF_SZ);
	u32 sz0 = MIN(sz, BUF_SZ - w),
	    sz1 = sz - sz0;
	memcpy(rb->buf + w, p, sz0);
	if (sz1)
		memcpy(rb->buf, (u8*)p + sz0, sz1);
	rb->w = (w + sz) % BUF_SZ;
}

inline static u32 ringbuf_readable(RingBuf *rb)
{
	u32 r = rb->r, w = rb->w,
	    tillend = BUF_SZ - r;
        return r <= w ? w - r : tillend + w;
}

inline static u32 ringbuf_read(RingBuf *rb, u8 *p, u32 n)
{
	u32 r = rb->r, w = rb->w,
	    tillend = BUF_SZ - r,
	    read = 0;
	if (r == w)
		return 0;
	if (r > w) {
		read = MIN(tillend, n);
		memcpy(p, rb->buf + r, read);
		n -= read;
		r = 0;
	}
	n = MIN(n, w - r);
	memcpy(p + read, rb->buf + r, n);
	read += n;
	rb->r = (rb->r + read) % BUF_SZ;
	return read;
}

inline static void ringbuf_reset(RingBuf *rb)
{
	rb->r = rb->w = 0;
}

inline static u64 ns(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_REALTIME, &ts);
	return 1000000000 * (u64)ts.tv_sec + (u64)ts.tv_nsec;
}

inline static u64 us(void)
{
	return ns() / 1000;
}

inline static u64 ms(void)
{
	return ns() / 1000000;
}

inline static u64 mix(u64 x)
{
	const u64 C = UINT64_C(0xBEA225F9EB34556D);
	x ^= x >> 32, x *= C;
	x ^= x >> 29, x *= C;
	x ^= x >> 32, x *= C;
	x ^= x >> 29;
	return x;
}

inline static u32 rndid(void)
{
	return (u32)mix(ns());
}

#ifdef __unix__
typedef int Socket;
#elif defined(_WIN32)
typedef SOCKET Socket;

DEFINE_PROPERTYKEY(PKEY_DeviceInterface_FriendlyName, 0x026E516E, 0xB814,
		   0x414B, 0x83, 0xCD, 0x85, 0x6D, 0x6F, 0xEF, 0x48, 0x22, 2);

#define CHKH(x, s, ...) CHK(SUCCEEDED(x), s, ##__VA_ARGS__)
#define F(ptr, fn, ...) ptr->lpVtbl->fn(ptr, ##__VA_ARGS__)
#define PIPE_NAME L"\\\\.\\pipe\\sp-send-pipe"
enum {PIPE_BUF_SZ = 512,
      PIPE_TIMEOUT_MS = 5000};

enum {
	PIPE_NONE,
	PIPE_PREP
} PipeCmd;

/* only for win so layout and whatnot will be the same */
typedef struct {
	u32 ip;
	u16 port;
	bool disable;
} LlInfo;
#endif
