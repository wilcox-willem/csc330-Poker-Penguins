package main

import (
	"fmt"
	"os"
	"strconv"
)

// Represents the game of Five Hand, a poker game with 6 hands.
// The game can be played with a randomized deck or a deck loaded from a file.
// author: Davis Guest
type FiveHand struct {
	deck *Deck
	hands []*Hand
	j_Flag bool
	s_Flag bool
	s_Count int
}


// Initializes a new Five Hand game with a list of 6 empty hands and a deck of cards.
// If command line arguments are provided, it builds a file deck; otherwise, it builds a randomized deck.
// param: file - String representing the file to build the deck from.
// param: j - Boolean representing the command line arg flag that will add TWO JOKERS to the deck of cards.
// param: f - Boolean representing the command line arg flag specifies that the next command line argument is a handset file for testing.
// param: s - Boolean representing the command line arg that turns on the statistics collection flag.
// param: count - int representing the amount of times to collect statistics
func initFiveHand(file string, j bool, s bool, count int) *FiveHand {
	// initialize deck and hand array
	var d = initDeck()
	var h = make([]*Hand, 0)

	// initializes 6 hands inside of the hand array
	for i := 0; i < 6; i++ {
		h = append(h, initHand())
	}

	if (file != "") { d.buildFileDeck(file) 
	} else  { d.buildRandDeck(j) }

	return &FiveHand {
		deck : d,
		hands : h,
		j_Flag : j,
		s_Flag : s,
		s_Count : count,
	}
}


// Starts a Five Hand game.
// Type of game is determined if there is an input file.
// Then determines the winning hands in descending order.
// param: file - String representing the file to build the deck from.
func (f *FiveHand) play(file string) {

	fmt.Println("\n*** P O K E R   H A N D   A N A L Y Z E R ***\n")

	gameType := 0
	if (file != "") { gameType = 1 }

	if (gameType == 0) {
		fmt.Println( "\n*** USING RANDOMIZED DECK OF CARDS ***\n" + "\n*** Shuffled 52 card deck\n" + f.deck.toString())

	} else {
		fmt.Println( "\n*** USING TEST DECK ***\n" + "\n*** File: " + file + "\n" + f.deck.toString())
	}

	if (f.deck.duplicate != nil) {
		fmt.Println( "\n*** ERROR - DUPLICATED CARD FOUND IN DECK ***\n" + "\n*** DUPLICATE: " + f.deck.duplicate.toString() + " ***\n")
		return
	}

	f.drawCards(gameType)

	fmt.Println("\n*** Here are the six hands...")

	f.printAllHands()

	if (gameType == 0) {
		fmt.Println("\n*** Here is what remains in the deck...\n" + f.deck.toString())
	}

	fmt.Println("\n--- WINNING HAND ORDER ---")

	f.sortHands()
	f.printAllHands()
	fmt.Println()
}


// Draws 30 cards to set up 6 hands of 5 cards.
// Alternates drawing cards among the hands.
// param: gameType - int representing if the hands should be drawn randomized or from a file input.
func (f *FiveHand) drawCards(gameType int) {
	handNum := 0
	if (gameType == 0) {
		for i := 0; i < 30; i++ {
			if (handNum == 6) { handNum = 0 }
			f.hands[handNum].addCard(f.deck.drawCard())
			handNum++
		}
	} else {
		for i := 1; i <= 30; i++ {
			f.hands[handNum].addCard(f.deck.drawCard())
			if (i % 5 == 0) { handNum++ }
		}
	}
}


// Prints all the hands to the console.
func (f *FiveHand) printAllHands() {
	for i := 0; i < len(f.hands); i++ {
		fmt.Println(f.hands[i].toString())
	}
}


// Sorts the hands to the winning order
func (f *FiveHand) sortHands() {
	for j := 0; j < len(f.hands) - 1; j++ {
        for i := 0; i < len(f.hands) - 1; i++ {
            if f.hands[i].compare(f.hands[i + 1]) < 0 {
                temp := f.hands[i + 1]
                f.hands[i + 1] = f.hands[i]
                f.hands[i] = temp
            }
        }
    }
}


// Main Method Calls
func main() {
	f := ""
	j_Flag := false
	s_Flag := false
	s_Count := 0

	if (len(os.Args) > 1) {

		for i:=1; i < len(os.Args); i++ {

			if (os.Args[i] == "-j") { // if Args[i] equals "-j", sets j_Flag to true
				j_Flag = true

			} else if (os.Args[i] == "-f") { // if Args[i] equals "-f", sets the file name Args[i + 1] and then increments i by 1
				f = os.Args[i + 1]
				i++

			} else if (os.Args[i] == "-s") { // if Args[i] equals "-s", sets s_Flag to true
				s_Flag = true
				if (i + 1 < len(os.Args) && (os.Args[i + 1] != "-j" || os.Args[i + 1] != "-s")) { // if the next index is still in bounds and the next value of Args[i]
																								  // != -j or -s, sets s_Count to Args[i + 1]
					count, err := strconv.Atoi(os.Args[i + 1])
					if err != nil {
						fmt.Println("Error converting string to integer:", err)
						return
					}
					s_Count = count
				}
			}
		}
	}
	game := initFiveHand(f, j_Flag, s_Flag, s_Count)
	game.play(f)
}