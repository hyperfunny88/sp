# only for cross-compiling to windows
project(
	'sp',
	'c',
	version : '0.1.0',
	license : 'BSD-1-Clause',
	default_options : [
		'c_std=gnu11',
		'buildtype=minsize',
		'default_library=static',
		'warning_level=2'
	]
)
cflags = ['-fvisibility=hidden', '-Wno-missing-braces', '-Wno-attributes']
cmake = import('cmake')
x32 = host_machine.cpu_family() == 'x86'
if not x32
	opt = cmake.subproject_options()
	opt.add_cmake_defines({'USE_SSE': true}) # maybe support ARM windows later
	speexdsp = cmake.subproject('speexdsp', options : opt).dependency('speexdsp')
endif
minhook = cmake.subproject('minhook').dependency('minhook')
ldflags = ['-static', '-lws2_32', '-lwinmm', '-lksuser', '-lsetupapi']
deps = []
add_project_arguments(cflags, language : 'c')
add_project_link_arguments(ldflags, language : 'c')
include = include_directories('.')
shared_library(
	'sp-send-ll',
	'send-ll-win.c',
	include_directories : [include],
	dependencies : [deps, minhook]
)
if not x32
	executable(
		'sp-send',
		'send-win.c',
		include_directories : include,
		dependencies : deps
	)
	executable(
		'sp-recv',
		'recv.c',
		include_directories : include,
		link_args : ldflags, # meson gets rid of ldflags for some reason
		dependencies : [dependency('threads'), speexdsp]
	)
endif
