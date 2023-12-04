# Project 4 - Five Card Stud - Jokers and Statistical Improvements

## Description
Just like previous two iterations of the project, this card game immitates a special version of poker called Five Card Stud. This game does not allow for the splitting of the pot or ties. There is a clear winner each time depending on the hand's type, high card, and suit. There is no "draw phase" either, a randomized deck is looped through giving 1 card to one of six hands at a time (unless using a file input test deck). In this version, there is the abiility to add 2 jokers to the deck.

## Game Flags
There are three variations for this project that can be initiated by the following command line arguements. If no arguements are present, a normal randomized game is run.

**FLAGS ARE CASE SENSITIVE**

### -j
This command line arguements adds two jokers to the randomized deck. These will have face designation X and suit designation either R or B. So the red joker would be XR and the black joker would be XB. Jokers are wild and can take on ANY CARD identity except for those found in the current hand. In other words, a given hand cannot have two of the same card.

### -f { filename }
This command line argument specifies that the next command line argument is a handset file for
testing. The ability to have two jokers in the handsets is possible. Files must have the following formatting:

* Space before all cards ranks/faces except for 10 (including the first card per line)
* Capital suit labels
* Comma after each suit except for the last card per line


### -s { count }
This command line arguement turns on the statistics collection flag. If there is a second command line argument then it
specifies the number of samples to run. So running with ”-s 1000” would run your program 1000 time
generating 6000 poker hand. If no count is specified, a default 1000 runs will occur.

## Build Instructions
To build an executable in Go, use the following command:

* go build -o {name of your choosing} Card.go Deck.go FiveHand.go Hand.go

For example:

* go build -o **Poker** Card.go Deck.go FiveHand.go Hand.go

To run the executable for the different modes, type the following and press enter:

* {executable} -j

* {executable} -s
* {executable} -s {number}

* {executable} -s -f
* {executable} -s {number} -f

* {executable} -f {file name}

**The order in which -s and -f does not matter, its just the number of steps after -s must come after**


## Extra Credit: ADA - Compilation Instructions

Insert compilation instructions for the ADA files.

## Authors
Group: Poker Penguins

* Philomina Ekezie
* Dirk Kotze
* Davis Guest
* Shruti Senthil
* Willem Wilcox

Dec 2023, CSC 330, Dr. Pounds, Mercer University
