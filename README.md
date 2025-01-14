# Minesweeper

[![Build Status](https://travis-ci.org/ljishen/Minesweeper.svg?branch=master)](https://travis-ci.org/ljishen/Minesweeper)

An ASCII style Minesweeper, and AI solver implemented by using backtracking algorithm written in Haskell.

Build and run
--------
```
stack build --ghc-options="-O2 -threaded -stsopts -eventlog"
stack exec -- Minesweeper 30 16 99 +RTS -N2 -ls
```


Features
--------

- Player will never lose the game on the first reveal
- The AI solver can output a set of possible safe squares for each continuous path
- The output set includes all determinable safe squares

Example Output
--------------

```
[[('A',0)],[('B',7),('B',8)],[('H',1),('I',1),('J',1)],[('G',6),('G',7),('G',9),('H',7),('H',8),('J',8)]]
```

![Precise Result and Guessing Result](https://raw.githubusercontent.com/ljishen/Minesweeper/master/resources/Precise%20Result%20and%20Guessing%20Result.png)

Each sublist in the result represents the possible safe locations in the relative continuous path. For the continuous path in the upper left corner, the result shows (A, 0) is a safe location. For the bottom left corner, the result shows (H, 1), (I, 1), (J, 1) are the three safe locations. Beside its precision, in the bottom right corner, there is one mine hidden among the two closed squares marked in red line. Based on the number information only, this program produces a possible suggestion by guessing one of them.

One key concept in this implementation is **continuous path**. The following image shows two continuous paths in red

![Two continuous path in red](https://raw.githubusercontent.com/ljishen/Minesweeper/master/resources/Two%20continuous%20path%20in%20red.png)

Reference Paper
---------------

[Minesweeper and AI Solver Implementation in Haskell](https://drive.google.com/open?id=0B9Q3i4Vp4rm2cU0tOTNiMkRLeE0)
