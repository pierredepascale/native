# Makefile for compiling the runtime

rt: rt.c entry_x86.s
	gcc -m32 -o rt -g rt.c entry_x86.s obj.c debug.c io.c fasl.c

