# NAME
game-of-life - Conway's Game of Life simulator.

# SYNOPSIS

    game-of-life [OPTION] [PATTERN]

# DESCRIPTION
Simulate Conway's Game of Life.

If you specify PATTERN from the following candidates, you can simulate well-known patterns.

- blinker
- beacon
- toad
- pinwheel
- galaxy
- glider

# OPTIONS

    -n N
        Simulate in the universe of n * n.
        If this option is not specified, it will simulate in 10 * 10 universe.

# EXAMPLES
Simulate the in the universe 10 * 10.

    $ paren game-of-life

Simulate the in the universe 30 * 30.

    $ paren game-of-life -n30

Simulate the galaxy pattern.

    $ paren game-of-life galaxy
