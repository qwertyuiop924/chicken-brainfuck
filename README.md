# CHICKEN Brainfuck
NOTE: this version requires both vector-lib, and anaphora.
This is my fork of the CHICKEN brainfuck interpreter by Federico Ramirez, aka gosukiwi.
I'm cleaning it up a bit, but it basically has the same structure of the original.
This is an interactive interpreter for the 
[Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) programming language.

## Usage
Simple enough. Just execute the `/usr/bin/brainfuck` executable.

## Build from source
To build, you'll need the [CHICKEN Scheme](http://code.call-cc.org/) compiler.
Just follow the instructions, get the compiler, navigate to the repo and run
`chicken-install -s`.

```
git clone git@github.com:qwertyuiop924/chicken-brainfuck.git
cd chicken-brainfuck
chicken-install && brainfuck
Welcome to CHICKEN Brainfuck!
brainfuck>
```

Done! Your interpreter lives in `/usr/bin/brainfuck` (or possibly elsewhere,
depending on how your chicken install has been configured).
This has only been tested on Linux (Archlinux, with chicken 4.10.0).

##Non-standard Syntax
 *`%` sets all cells to 0
 *`!` exits, returning zero

## TODO
 * ~~`reset` function maybe? Just something to type and be able to reset the state~~ 
 * add support for script running
 * add support for brainfork extensions (when running a script)
