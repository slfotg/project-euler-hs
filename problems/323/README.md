## Problem 323 - [Bitwise-OR operations on random integers](https://projecteuler.net/problem=323)

Let *y*<sub>0</sub>, *y*<sub>1</sub>, *y*<sub>2</sub>,... be a sequence of random unsigned 32 bit integers
(i.e. 0 ≤ *y<sub>i</sub>* < 2<sup>32</sup>, every value equally likely).

For the sequence *x<sub>i</sub>* the following recursion is given:

 - *x*<sub>0</sub> = 0 and
 - *x<sub>i</sub>* = *x<sub>i-1</sub>* | *y<sub>i-1</sub>*, for *i* > 0. ( | is the bitwise-OR operator)

It can be seen that eventually there will be an index N such that xi = 2<sup>32</sup> -1 (a bit-pattern of all ones) for all *i* ≥ N.

Find the expected value of N.
Give your answer rounded to 10 digits after the decimal point.
