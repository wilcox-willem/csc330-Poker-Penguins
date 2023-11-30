package main

import (
	"fmt"
	// WW: "os"
)

// Represents the game of Five Hand, a poker game with 6 hands.
// The game can be played with a randomized deck or a deck loaded from a file.
// author: Davis Guest
type FiveHand struct {
	deck *Deck
	hands []*Hand
}


// Initializes a new Five Hand game with a list of 6 empty hands and a deck of cards.
// If command line arguments are provided, it builds a file deck; otherwise, it builds a randomized deck.
// param: file - String representing the file to build the deck from.
func initFiveHand(file string) *FiveHand {
	
	var d = initDeck()
	var h = make([]*Hand, 0)

	for i := 0; i < 6; i++ {
		h = append(h, initHand())
	}

	if (file != "") { d.buildFileDeck(file) 
	} else  { d.buildRandDeck() }

	return &FiveHand {
		deck : d,
		hands : h,
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

////// WW: BEGIN - Stats mode functions,


// Returns an int array, containing the
// values of the players hands
//
// altered version of sortHands(), no sorting to speed
// up run time (hopefully)
func (f *FiveHand) handsStats() []int {
	playerScores := make([]int, len(f.hands))
	for i := 0; i < len(f.hands) - 1; i++ {
        f.hands[i].assessHand()
        playerScores[i] = f.hands[i].handType
    }

    return playerScores
}


// Prints the stats collected from
// number of games run
//
// altered version of play(), collects values
// of hand scores from each player via handsStats()
func playStats(numSamples int) {
	// default to 1000 inputs
	if (numSamples == 0) {
		numSamples = 1000
	}

	gameStats := make([]int, 10)

	fmt.Println("\n---- STATISTICAL ANALYSIS ----\n") // 30 char long
	for i := 0; i < numSamples - 1; i++ {

		game := initFiveHand("")
		game.drawCards(0) // gameType 0 for rand mode
		playerScores := game.handsStats()
		
		// update frequency of scores in gameStats
		for i := 0; i < len(playerScores) - 1; i++ {
			currentScore := playerScores[i] - 1
			gameStats[currentScore]++
		}
	
	}
	
	// print results
	handTitles := []string{
		"HIGH CARD",
		"PAIR",
		"TWO PAIR",
		"THREE OF A KIND",
		"STRAIGHT",
		"FLUSH",
		"FULL HOUSE",
		"FOUR OF A KIND",
		"STRAIGHT FLUSH",
		"ROYAL STRAIGHT FLUSH"}

	// prints RSF -> High Card
	for i := len(handTitles) - 1; i > -1; i-- {
		fmt.Printf("%-20s%10d\n", handTitles[i], gameStats[i]) // 30 char long
	}
}

////// WW: END

// Main Method Calls
func main() {
	//f := ""
	//if (len(os.Args) > 1) { f = os.Args[1] }
	// game := initFiveHand(f)
	// game.play(f)

	// WW: Test stats
	playStats(166666667)
}





