# Onitama in Haskell
## 5f2836518c36e098175d6e2f3c3fdb5b
### Game Representation
To represent the game in the code I use a data type called “Game”. This data type contains the cards in play, the pieces of both players, turn counter and an error index. The turn counter is used to keep track of who should move. If the turn is 1, player one should move, if turn is 2 then player two should move and else there is an error somewhere. The error index keeps track of how many succesfull moves has been played, which means that if an error occurs 20 moves in, the error index would be 20, and that exact line can be printed to the screen. 
The cards also has its custom data type, which contains the name, which can be compared to a string from the move input, and  a list of moves the piece could use. The values in the list of moves are how the piece can move relative to itself, which means that if the piece can move one forward, it would have a move called (1,0). 

### Challenges During Development
#### IsValid
During the development of isValid, the most difficulties were to remember to check for every single possible outcome. The initial board check was not too hard, but with every move, the board should mirror, which could have given some problems when the checks are done, but mainly the biggest hurtle was my lacking knowledge of Haskell. I have had the most trouble of stop my imperative thinking and start thinking in functional programming. 

#### GenerateRandom
The methods of the generateRandom procedures main difficulty was to use StdGen. After I learned how to use the StdGen it was pretty simple to implement everything, since the actual moving part was implemented for isValid.