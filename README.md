СОЙУЗ
-----

Advanced utility library for the DCPU-16 architecture. 

Core goals:

* All outputs will be compatible with official tools, and follow any official specs.
* Within that, go crazy with optimizations and features.
* Provide both command line tools, and developer libraries.

Semi-recent library docs are at http://amtal.github.com/soyuz.

Command Line Tool
-----------------

There are three main modes. Currently, the most interesting one is for assembly.

```
$ ./soyuz --help
Сойуз 0.0.0, amtal <alex.kropivny@gmail.com>

soyuz [OPTIONS] <FILE>

Mode of operation:
  -p --prettyprint      Assembly -> consistently formatted assembly
  -a --assemble         Assembly -> machine code
  -d --disassemble      Machine code -> assembly
Optimization:
     --no-optimize      Disable short literal/label optimization
General:
  -o --output=<FILE>    Write to file instead of stdout
  -h --hexdump          Encode binary data in a 16-bit hexdump
     --smooth-brackets  Parse (a) instead of [a] for indirect mode
  -? --help             Display help message
  -V --version          Print version information

Documentation and source at https://github.com/amtal/soyuz or on Hackage.
$ ./soyuz asm/notchspec.asm -a -h
0000: 7c01 0030 7de1 1000 0020 7803 1000 c00d
0008: ddc1 a861 7c01 2000 2161 2000 8463 806d
0010: b1c1 9031 d410 ddc1 9037 61c1 ddc1
$ ./soyuz asm/notchspec.asm -a -h --no-optimize
0000: 7c01 0030 7de1 1000 0020 7803 1000 7c0d
0008: 0010 7dc1 0021 7c61 000a 7c01 2000 2161
0010: 2000 7c63 0001 7c6d 0000 7dc1 000f 7c31
0018: 0004 7c10 001e 7dc1 0021 7c37 0004 61c1
0020: 7dc1 0021
```

The disassembler mode is straightforward. No heuristics to avoid mixed in dat instructions and other pitfalls - but it does add obvious labels.

```
$ ./soyuz asm/notchspec.asm -a -o notch.bin
$ ./soyuz notch.bin -d
                set a, 0x30
                set [0x1000], 0x20
                sub a, [0x1000]
                ifn a, 0x10
                set pc, 0x17
                set i, 0xa
                set a, 0x2000
:jump.000c      set [i], [0x2000+a]
                sub i, 1
                ifn i, 0
                set pc, 0xc
                set x, 4
                jsr 0x15
                set pc, 0x17
:func.0015      shl x, 4
                set pc, pop
:jump.0017      set pc, 0x17
```

Finally, pretty-print mode consistently re-formats the input.

```
$ ./soyuz asm/notchspec.asm -p
; Try some basic stuff
                set a, 0x30             ; 7c01 0030
                set [0x1000], 0x20      ; 7de1 1000 0020
                sub a, [0x1000]         ; 7803 1000
                ifn a, 0x10             ; c00d 
                set pc, crash           ; 7dc1 001a [*]
; Do a loopy thing
                set i, 0xa              ; a861
                set a, 0x2000           ; 7c01 2000
:loop           set [0x2000+i], [a]     ; 2161 2000
                sub i, 1                ; 8463
                ifn i, 0                ; 806d
                set pc, loop            ; 7dc1 000d [*]
; Call a subroutine
                set x, 4                ; 9031
                jsr testsub             ; 7c10 0018 [*]
                set pc, crash           ; 7dc1 001a [*]
:testsub        shl x, 4                ; 9037
                set pc, pop             ; 61c1
; Hang forever. X should now be 0x40 if everything went right.
:crash          set pc, crash           ; 7dc1 001a [*]
; [*]: Note that these can be one word shorter and one cycle faster by using the short form (0x00-0x1f) of literals,
;      but my assembler doesn't support short form labels yet.     
```


Installing Soyuz
----------------

For the command line tool, either:

* Get a binary for [Ubuntu](https://github.com/downloads/amtal/soyuz/soyuz-0.0.0-ubuntu-32.tgz).
* Build from source. Get the [Haskell Platform](http://hackage.haskell.org/platform/) and either:
  * `cabal install soyuz` for 0.0.0 from Hackage
  * `git clone git://github.com/amtal/soyuz.git; cd soyuz; cabal install` for HEAD



Parser Error Messages
---------------------

No really, they're quite nice. They're even coloured!*

```
*DCPU16.Assembler.Parser> parseFile "test.masm"
test.masm:2:19: error: expected: "0", "[",
    "a", "b", "c", "i", "j", "o", "pc", "peek",
    "pop", "push", "sp", "x", "y", "z", digit,
    letter or digit
              set #a, 0x30              ; 7c01 0030 
                  ^                                 
Nothing
*DCPU16.Assembler.Parser> parseFile "lower.masm"
lower.masm:14:29: error: label "lop" not defined
                 set pc, lop          ; 7dc1 000d [*] 
                            ^                         
Nothing
*DCPU16.Assembler.Parser> 
```

* No colour visible in copy-pastes.
