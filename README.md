# elfort

A Forth metacompiler that directly emits an executable binary for x86-64 Linux written in [Arkam](https://github.com/jinhanada/arkam).

[meta.f](meta.f) is a metacompiler including elf emitter and x86-64 inline assembler. It compiles [core.f](core.f).


## run

1. Build `arkam` from [Arkam](https://github.com/jinhanada/arkam) and move it into anywhere in your $PATH.
2. `make run`
3. type `"hello" prn bye" then press RETURN
