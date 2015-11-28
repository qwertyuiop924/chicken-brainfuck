# CHICKEN Brainfuck
This is an interactive interpreter for the 
[Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) programming language.

## Usage
Simple enough. Just execute the generated `build/brainfuck` executable.

## Build from source
To build, you'll need the [CHICKEN Scheme](http://code.call-cc.org/) compiler.
Just follow the instructions, get the compiler, navigate to the repo and run
`make`.

```
git clone git@github.com:gosukiwi/chicken-brainfuck.git
cd chicken-brainfuck
make && build/brainfuck
Welcome to CHICKEN Brainfuck!
brainfuck>
```

Done! Your interpreter lives in `build/brainfuck`. This has only been tested on
Linux (openSUSE Leap).

## TODO
 * `reset` function maybe? Just something to type and be able to reset the state
