Advanced utility library for the DCPU-16 architecture.

Core Goals
----------

* All outputs will be compatible with official tools, and follow any official specs.
* Within that, go crazy with optimizations and features.
* Provide both command line tools, and developer libraries.

Semi-recent library docs are at http://amtal.github.com/soyuz.

Command Line Tool
-----------------

Output of --help:

```
$ soyuz --help
soyuz 0.0.0, amtal <alex.kropivny@gmail.com>

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
     --uppercase        Parse uppercase symbols (but never mixed case)
     --smooth-brackets  Parse (a) instead of [a] for indirect mode
  -? --help             Display help message
  -V --version          Print version information

Documentation and source at https://github.com/amtal/soyuz or on Hackage.
```

Usage demonstration:

```
$ cat > fib.asm
; Fibonacci filler from reddit
SET A, 1
SET B, 1
SET PEEK, 1
:loop ADD A, B ; A is now 2, B is still 1
SET PUSH, A
SET A, B
SET B, PEEK
IFG SP, 10 ; 10 because that's how much space 
; this program takes 
SET PC, loop
$ soyuz -p fib.asm --uppercase
; Fibonacci filler from reddit
                set a, 0x1
                set b, 0x1
                set peek, 0x1
:loop           add a, b                ; A is now 2, B is still 1
                set push, a
                set a, b
                set b, peek
                ifg sp, 0xa             ; 10 because that's how much space 
; this program takes 
                set pc, loop
$ soyuz -a fib.asm -h --upper
0000: 8401 8411 8591 0402 01a1 0401 6411 a9be
0008: 8dc1
```

More advanced tools can be written using the libraries.

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
