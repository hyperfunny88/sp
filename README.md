# sp - simple/sound pipe
Pretty good audio over UDP. Targeted for VM guest-\>host or guest-\>guest audio
playback.

Can achieve better latencies than native Windows / passing through a soundcard.

Contains 3 programs:
- `sp-recv`: Recieves audio and plays it. For Unix and Windows.
- `sp-send`: Captures audio and sends it to the reciever. Windows-only
  (right now).
- `sp-send-ll`: An injected DLL that hooks the native WASAPI functions to yank
  the audio frames straight from the buffer of the hooked program even before
  WASAPI gets it. This allows for 'perfect' latency. Windows-only (forever).

It was very simple at one point but now it's a little less simple. Might tidy it
up one day.

# Dependencies
- speexdsp

For Unix:
- libsoundio

For cross-compiling to Windows:
- MinGW
- MinHook
- meson
- CMake

`speexdsp` and `MinHook` are included as submodules for cross-compilation. For a
native build, you want to install `speexdsp` and `libsoundio` via your system
package manager.

# Building
Compilation for Windows is done by cross-compilation with meson, and CMake (via
meson).

For the Unix build you do not need a build system. You can just run `cc` on
recv.c and link against the libraries above.

For a quick build, there are some scripts included. Simply run `./build/unix && ./build/win`.
You will find the binaries located in build/.

# Todo
- Make the thing user friendly(-er)
- Unix audio capture/client
- Tidy the thing up

# License
GPL v3.0
