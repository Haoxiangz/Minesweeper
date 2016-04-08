# Minesweeper

An ASCII style Minesweeper, and AI solver implemented by using backtracking algorithm written in Haskell.

**Features**
- Player will never lose the game on the first reveal
- The AI solver can output a set of possible safe squares for each continuous path
- The output set includes all determinable safe squares

The following image shows two **continuous path** in red
<p align="center">
  <img src="https://raw.githubusercontent.com/ljishen/Minesweeper/master/Two%20continuous%20path%20in%20red.png" alt="Two continuous path in red" />
</p>

The possible safe sqaures from output is
<p align="center">
  <b>[[('A',0)],[('B',7),('B',8)],[('H',1),('I',1),('J',1)],[('G',6),('G',7),('G',9),('H',7),('H',8),('J',8)]]</b>
  <p align="center"><img src="https://raw.githubusercontent.com/ljishen/Minesweeper/master/Precise%20Result%20and%20Guessing%20Result.png" alt="Two continuous path in red" /></p>
</p>

Reference Paper: [Minesweeper and AI Solver Implementation in Haskell] (https://drive.google.com/file/d/0B9Q3i4Vp4rm2RGxhZWVfSEVCUXM/view?usp=sharing])
