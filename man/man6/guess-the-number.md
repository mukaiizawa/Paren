# NAME
guess-the-number -- number guessing game.

# SYNOPSIS

    guess-the-number

# DESCRIPTION
This is a game where the computer guesses the numbers you are thinking of.

Run the program and imagine one of the numbers from 0 to 100.

The computer will guess the number you think of, so enter `h` if the number you think of is heigher, `l` if it is lower, or `e` if it is equal.

# NOTES
The computer can guess the answer within seventh times.

This is because a single guess can reduce the number of candidates in half.

Therefore, this search is a so-called `binary search`, and the following inequality sign holds.

    100 < 2^7 (<=> 128)
