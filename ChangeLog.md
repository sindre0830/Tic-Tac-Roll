# Changelog for assignment-one

[1.0.0] setup: Initial commit

[1.1.0] development: Added new types and a newBoard function

[1.2.0] development: Added function to render board

[1.3.0] development: Added function to validate move

[1.4.0] development: Added simple game loop and a function to update board

[1.5.0] development: Added win condition if a row match

[1.6.0] development: Changed row validation to only check relevant row

[1.7.0] development: Added win condition if a column match

[1.8.0] development: Added win condition if a diagonal match

[1.9.0] development: Added tie condition if no win condition is met and board is full

[1.9.1] formatting: Reformatted some code to keep consistency

[1.10.0] development: Added rotate function

[1.11.0] development: Changed board validation to validate the entire board

[1.12.0] development: Added swapping function

[1.12.1] formatting: Added GoLint suggestions

[1.12.2] formatting: Simplified PvP gameloop

[1.13.0] development: Added PvE gameloop with simple entity AI

[1.13.1] formatting: Changed version format

[1.13.2] formatting: Fixed indentation

[1.13.3] formatting: Added custom type Board

[1.13.4] formatting: Renamed type Move to Mark

[1.13.5] formatting: Simplified newBoard and verifyMove

[1.13.6] formatting: Added and modified types for readability

[1.13.7] formatting: Removed redundancy and simplified code

[1.13.8] formatting: Simplified rotation and renamed variables

[1.13.9] formatting: Switched from guards to nested if statements in verifyBoard

[1.13.10] formatting: Modified print layout

[1.13.11] setup: Added hie.yaml file to prevent cradle issues with VSCode

[1.14.0] development: Added menu for choosing gamemodes

[1.15.0] development: Added EvE gamemode

[1.16.0] testing: Added spec testing on switchMark and renderRow

[1.16.1] setup: Split Lib module into multiple modules for readability

[1.17.0] development: Changed boardSize from function to parameter

[1.17.1] formatting: Converted from tabs to spaces (heresy)

[1.18.0] development: Changed updateBoard to not modify board if position is out of scope

[1.19.0] testing: Added testing to Grid module

[1.19.1] formatting: Added indentation in testing description for readability

[1.20.0] development: Added condition in dividingLine if size is less then 1

[1.21.0] testing: Added testing to Render module

[1.22.0] development: Using Monads in filterGameInput to handle invalid user input

[1.23.0] testing: Added testing to InputFilter module

[1.24.0] development: Added condition in rotateL to return empty Matrix if input is empty

[1.25.0] development: Added condition in listToMatrix to return empty Matrix if input is empty

[1.26.0] development: Added condition in swapPieces to do nothing if swapping isn't possible

[1.27.0] testing: Added testing to Transformation module

[1.28.0] development: Added handling of edge cases in Validation module

[1.29.0] testing: Added testing to Validation module

## Unreleased changes
