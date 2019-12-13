## Problem 692 - [Siegbert and Jo](https://projecteuler.net/problem=692)

Siegbert and Jo take turns playing a game with a heap of <i>N</i> pebbles:
1. Siegbert is the first to take some pebbles. He can take as many pebbles as he wants. (Between 1 and <i>N</i> inclusive.)
2. In each of the following turns the current player must take at least one pebble and at most twice the amount of pebbles taken by the previous player.
3. The player who takes the last pebble wins.

Although Siegbert can always win by taking all the pebbles on his first turn, to make the game more interesting he chooses to take the smallest number of pebbles that guarantees he will still win (assuming both Siegbert and Jo play optimally for the rest of the game).

Let <i>H</i>(<i>N</i>) be that minimal amount for a heap of <i>N</i> pebbles.
<i>H</i>(1)=1, <i>H</i>(4)=1, <i>H</i>(17)=1, <i>H</i>(8)=8 and <i>H</i>(18)=5

Let <i>G</i>(<i>n</i>) be ∑<sub><i>k</i>=1..<i>n</i></sub><i>H</i>(<i>k</i>).
<i>G</i>(13)=43

Find <i>G</i>(23416728348467685)