# sp - simple/sound pipe

Pretty good audio over UDP. Targeted for VM guest-\>host or guest-\>guest audio
playback.

Can achieve better latencies than native Windows / passing through a soundcard.

This is new software, so please file an issue if you encounter any problems
(compatability, etc.) :)

# Features

- Low-latency
- Provides stable audio
- Provides 'accurate' audio
- Supports both sending and receiving on Windows and Linux
- Supports many backends (ALSA, PulseAudio, JACK (provided by libsoundio),
  native PipeWire, and WASAPI)
- Simple (4 source files for each built program and a header)

Contains 3 programs:
- `sp-recv`: Receives audio and plays it. For Linux and Windows. Has ALSA,
  PulseAudio, JACK (provided by libsoundio), native PipeWire, and WASAPI
  backends.
- `sp-send`: Captures audio and sends it to `sp-recv`. For Linux and Windows.
- `sp-send-ll`: An injected DLL that hooks the native WASAPI functions to yank
  the audio frames straight from the buffer of the hooked program before WASAPI
  gets it, therefore bypassing the whole Windows audio engine (mixer, resampler,
  etc.). This allows for 'perfect' latency. Windows-only (forever).

`sp-send*` sends the raw stream captured as soon as it gets it, without
buffering, directly to `sp-recv`. `sp-send-ll` sends all streams separately, and
has a separate instance for each application. This means that all separate
streams will have their own separate stream in `sp-recv` at the receiver's end
which enables (if your sound server supports it) routing and processing streams
separately if you wish to do so.

# Using

Grab the binaries in releases or follow the building steps in the 'Building'
section below.

Start `sp-recv <port>` via a terminal on the machine or virtual machine that you
want to receive audio to (the one that your physical sound system is connected
to).

Start `sp-send <receiver ip> <port>` via a terminal on all of your machines or
virtual machines that you want to send audio from.

`sp-send` on Linux requires PipeWire. After you start it, you will need to need
to change your default output device to `sp-send`. Also, PipeWire's default
config for VMs has bad latency. Make sure you edit your `pipewire.conf` and
`pipewire-pulse.conf` (if you don't already have them, copy them from
/usr/share/pipewire/ to ~/.config/pipewire/ and edit that) and set
`default.clock.min-quantum` in `vm.overrides` to something like 128 and
`pulse.min.quantum` to 128/48000 in each respective file. If you get crackling,
you can try increasing the 128 to something higher or revert it back to 1024.

If you want to have low-latency audio and separated streams and are on Windows,
run `install-ll.bat` as administrator **(after reading 'Caveats' below)**. To
uninstall, run `uninstall-ll.bat` as administrator. `sp-send-ll` requires both
`sp-recv` and `sp-send` (non-ll) to be running and connected to work.

You can change the backends with '-o'. On Linux, I recommend the PipeWire or
Jack backend. (Please don't use the PulseAudio it's shit. I get ~2 seconds of
latency with it).

# Building

## Dependencies

### For Linux:

- A C11 compiler supporting a small subset of GNU extensions (TCC works)

#### For sp-recv:

For ALSA, PulseAudio, and Jack backends:
- libsoundio
- speexdsp

AND/OR for PipeWire backend:
- PipeWire

#### For sp-send:

- PipeWire

### For cross-compiling to Windows:
- MinGW
- meson
- CMake

#### For sp-recv:
- speexdsp

#### For sp-send:
- MinHook

`speexdsp` and `MinHook` are included as submodules for cross-compilation. For a
native build, you want to install `speexdsp` and `libsoundio`, or `pipewire` via
your system package manager.

Compilation for Windows is done by cross-compilation with meson, and CMake (via
meson). Compiling for Windows on Windows is not supported, but shouldn't be
difficult.

For the Linux build you do not need a build system. You can just run `cc` on
`recv.c` and link against the libraries above.

For a quick build, there are some scripts included. Simply run `./build/unix &&
./build/win`. You will find the binaries located in build/.

# Caveats

sp is targeted towards VM communication, and is not optimized to be used over a
physical network. The way that sp sends data is likely unfriendly to most
routers and network cards. As mentioned earlier, sp does not do any extra
buffering, batching, or processing at the sender's side. Therefore
if you are using `sp-send-ll`, every hooked program will create it's own
	connection to send the stream. This means that with `sp-send-ll`, the
	more clients you have, the more bandwidth it uses.

`sp-send-ll` will not work with some protected programs (ex. web browser). It
will also not work with audio capture programs (like OBS) as it mutes the stream
to WASAPI when it gets hooked so you don't get double audio with `sp-send`.

Due to `sp-send-ll`'s hooking nature, it requires being injected into every
application. It therefore may be incompatible with some anticheat systems. The
method of injection that is provided in releases is to use the
[AppInit\_DLLs](https://learn.microsoft.com/en-us/windows/win32/dlls/secure-boot-and-appinit-dlls)
registry entries. These entries have also been used by malware as an attack
vector. However, do note:
- Editing those entries requires administrator privileges
- The DLLs that are loaded will be placed in a protected folder that also
  requires administrator privileges to modify
- You should ideally be installing `sp-send-ll` only in a VM

I personally keep my sensitive information out of my VMs, so this is not a
problem for me.

It may be possible to create a method that does not require AppInit\_DLLs,
however that will require finding the struct offset of the WAVEFORMATEX
structure in IAudioClient (this is the way that Discord does its per-application
audio sharing without being launched or hooked before the application runs).
These offsets can differ with every Windows version, so will require a lot of
maintainance. If someone knows a database of such offsets or an alternative
method please let me know.

It is also currently possible to use a DLL injector to run a program with
`sp-send-ll` . However this is inconvenient and requires the DLL to be injected
and setup before the WASAPI audio client gets initialized.

(Note: this all is only with `sp-send-ll`. `sp-send` uses regular Windows APIs
to grab the audio stream).

# Todo

- Make the thing user friendly(-er)
- Linux audio capture/client
- Optimize the header data

It was very simple at one point but now it's a less simple. Might tidy it up one
day.

# License

GPL v3.0
