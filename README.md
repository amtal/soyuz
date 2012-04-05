Work in progress, stay tuned...

Thus far, I've got:
* abstract instruction set representation
* machine code encode/decode
* assembly parse/pretty-print

Needs some finishing touches, then can write optimizing assembler.

Examples
--------

Trifecta parse error messages, with colour:

```haskell
*DCPU16.Assembler.Parser> parseFile "test.masm"
test.masm:2:19: error: expected: "0", "[",
    "a", "b", "c", "i", "j", "o", "pc", "peek",
    "pop", "push", "sp", "x", "y", "z", digit,
    letter or digit
              set #a, 0x30              ; 7c01 0030 
                  ^                                 
Nothing
*DCPU16.Assembler.Parser> 
```

Parser + pretty printer demonstration:

```haskell
Test.hs:
    import DCPU16.Assembler.Printer
    import DCPU16.Assembler.Parser

    test = do
        (Just x) <- parseFile "lower.masm"
        putStrLn $ pprint x

Prelude> :l Test
...
Ok, modules loaded: DCPU16.Assembler.Printer, DCPU16.Assembler.Parser, Main, DCPU16.Instructions.
*Main> test
...
; try some basic stuff
                set a, 0x30             ; 7c01 0030
                set [0x1000], 0x20      ; 7de1 1000 0020
                sub a, [0x1000]         ; 7803 1000
                ifn a, 0x10             ; c00d 
                set pc, "crash"         ; 7dc1 001a [*]
; do a loopy thing
                set i, 0xa              ; a861
                set a, 0x2000           ; 7c01 2000
:loop           set [0x2000+i], [a]     ; 2161 2000
                sub i, 0x1              ; 8463
                ifn i, 0x0              ; 806d
                set pc, "loop"          ; 7dc1 000d [*]
; call a subroutine
                set x, 0x4              ; 9031
                jsr "testsub"           ; 7c10 0018 [*]
                set pc, "crash"         ; 7dc1 001a [*]
:testsub        shl x, 0x4              ; 9037
                set pc, pop             ; 61c1
; hang forever. x should now be 0x40 if everything went right.
:crash          set pc, "crash"         ; 7dc1 001a [*]
; [*]: note that these can be one word shorter and one cycle faster by using the short form (0x00-0x1f) of literals,
;      but my assembler doesn't support short form labels yet.
*Main> 
```
