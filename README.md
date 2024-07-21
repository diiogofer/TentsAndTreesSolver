# TentsAndTreesSolver

### Project Description
This project is an automatic solver for the "Tents and Trees" game developed in Prolog. By using pure logic, the program solves the game puzzles, providing an efficient and elegant approach to finding solutions. This hack showcases the power of logic programming and Prolog in solving complex problems automatically.

### Usage Example

Original puzzle (unsolved):
```
puzzle(6-14, 
([
[_, a, _, a, _, _],
[a, _, _, _, _, _],
[_, _, _, _, _, _],
[_, _, a, a, _, _],
[_, _, _, _, _, _],
[_, a, _, _, a, _]],
[3, 0, 1, 1, 1, 1], [2, 1, 1, 1, 2, 0])).
```

To solve a puzzle from the "Tents and Trees" game in the Prolog compiler terminal, use the following command:
```
?- puzzle(6-14, P), resolve(P).
```

The output will be something like:
```
P = ([[t,a,t,a,t,r],
      [a,r,r,r,r,r],
      [r,r,r,t,r,r],
      [r,t,a,a,r,r],
      [r,r,r,r,t,r],
      [t,a,r,r,a,r]],
     [3,0,1,1,1,1],
     [2,1,1,1,2,0]).
```

- _ represents an empty cell. </br>
- t represents a tent. </br>
- a represents a tree. </br>
- r represents grass. </br>
- [3,0,1,1,1,1]: This list represents the number of tents in each row of the board, from top to bottom. </br>
- [2,1,1,1,2,0]: This list represents the number of tents in each column of the board, from left to right. </br>

#### Puzzle File
The puzzles used are defined in the puzzlesAcampar.pl file. The predicate puzzle(6-14, P) returns a specific puzzle that can be solved by the program. </br>
Feel free to change this file and add your own unsolved puzzles.

#### Limitations
Although the solver uses only logic and heuristics programmed by me, it may not solve large puzzles due to complexity and computational limitations. Additional adjustments and optimizations may be necessary to handle more complex puzzles.

## How to Run
Clone the repository:
```
git clone https://github.com/username/TentsAndTreesSolver.git
```
```
cd TentsAndTreesSolver
```
Ensure you have Prolog installed. You can use SWI-Prolog, which is a popular implementation:
```
sudo apt-get install swi-prolog
```
Load the main file in the Prolog terminal:
```
?- [puzzlesAcampar].
```
Solve a puzzle using the resolve/1 predicate:
```
?- puzzle(6-14, P), resolve(P).
```

#### Contributions
Contributions are welcome! Feel free to open an issue or submit a pull request with improvements and fixes.

