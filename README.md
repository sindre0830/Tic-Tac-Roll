# [Assignment 1 - tic tac roll](https://git.gvk.idi.ntnu.no/course/prog2006/prog2006-2021/-/wikis/Tasks/Assignment-1:-tic-tac-roll)

### Information

- Author: Sindre Eiklid
    - While the submission is individual, I have discussed the tasks with Rickard Loland. We have also helped each other with problems that occurred during development ([rubber ducking](https://en.wikipedia.org/wiki/Rubber_duck_debugging) mostly).
- I am using the Haddock formatting style for documentation.
- I am using Stack to format the structure of my repository.

### Setup

Open a terminal and navigate to the project directory ```cd (...)/assignment-one```
1. Type in ```stack build --exec assignment-one-exe``` to build and run the program
2. Type in ```stack test``` to build and run the tests

### Usage

When you execute the program you will be greeted by the menu. Here you can type in ```-h``` to view all available commands.

I've made three game modes: **PVP** (Player VS Player), **PVE** (Player VS Entity), and **EVE** (Entity VS Entity). Entity means computed moves and player means inputted moves. 

Each game mode can be executed with the optional parameter *n* to determine the board size. I.e. ```pvp 3``` will give a 3x3 board. This parameter will have to be larger than 1 and will default to 3 if user input is invalid.

During a game, you will have to input position and (optionally) rotation direction, i.e. ```5 right``` will place a mark at cell 5 and rotate the board 90 degrees to the right. Before rotating, it will swap the top corners.

The computed moves are very simplistic and only chooses a random position of all available cells. It will also randomize rotation (left, right, and none).

If a user makes an invalid move they can try again. I know the assignment said we could end the game in this case, but I decided to be nice to the user. Ending the game at this point would have been very simple to implement, just remove the recursive functions when verifyMove returns False and print switchMark won.

To exit the program you can press ```ctrl-c``` at any time.

### Notes

During the early parts of development, I used this [solution](https://dev.to/nt591/writing-a-tictactoe-game-in-haskell-545e) to print a fancy board but found the rest of his code to be very hardcoded. I decided early on that I wanted a dynamic board size and the possibility for it to be user-defined.

In the early parts of development, I went with a position-specific board check, this meant that it would only check the relevant row, column, and/or diagonal for winners. This was very fun to implement, but as soon as I started to work on the swapping corners feature I decided it would be easier to check the entire board and had to rewrite my implementation. If you would like to check my previous solution you can click [here](https://git.gvk.idi.ntnu.no/course/prog2006/as/sindre0830/assignment-one/-/blob/dc9686c9a53d282467adc8d35cb80eee19187042/src/Lib.hs#L67).

I had some trouble with ```sqrt``` function in Haskell, so I had to send board size as a parameter to each function needing it. I have since managed to create a function to get it from the board length itself (getBoardSize), but I didn't have time to implement it in my program. Have a look at that function to see where my next code improvement would be.

### Reflections

I didn't remember to keep a time log so it will be a rough estimate, what I can say with certainty is that I wasted a lot of time manually testing each function. I didn't use testing until after implementing the base program and spent a long time figuring which function failed where (changing return type to IO to see what I get back, etc...). In the late stage of the development, I was a lot more efficient since I could quickly test that everything worked after each modification. 

I also wasted a few hours on piping, but ended up with a EVE mode so it all worked out in the end.

```
Thinking about the various problems and design: 4 hours
Searching for things: 5 hours
Own coding/documenting: 15 hours
Total: 24 hours to complete the assignment with a default strategy.
```

One new thing I did for this project was using the ChangeLog.md file. It reminded me of doing atomic commits and writing descriptive comments.
