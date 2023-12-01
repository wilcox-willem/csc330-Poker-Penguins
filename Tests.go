package main

import (
	"fmt"
	"os"
)

func main() {

	f := ""
	if (len(os.Args) > 1) { f = os.Args[1] }

	// Card.jl tests
	fmt.Println("\n*** Card.jl tests ***")
	card1 := initCard(2, 0)
	card2 := initCard(3, 0)
	
	fmt.Println("\nCard - Card Functions:")
	fmt.Println("  " + card1.toString()) ; fmt.Println("  " + card2.toString()) ; fmt.Printf("  %d\n", card1.compare(card2))
	
	// ---------------------------------------
	
	// Deck.jl tests

	fmt.Println("\n*** Deck.jl tests ***")
	deck1 := initDeck()
	deck2 := initDeck()
	
	fmt.Println("\nRandom Deck - Initializing Variables:")
	fmt.Println("  " , deck1.cards) ; fmt.Println("  " , deck1.duplicate) ; fmt.Println("  " , deck1.deckType)
	
	fmt.Println("\nRandomized Deck:")
	deck1.buildRandDeck() ; fmt.Println(deck1.toString())
	
	fmt.Println("\nFile Deck - Initializing Variables:")
	if f != "" {
		fmt.Println("  " , deck2.cards) ; fmt.Println("  " , deck2.duplicate) ; fmt.Println("  " , deck2.deckType)
	
		fmt.Println("\nFile Deck:")
		deck2.buildFileDeck(f) ; fmt.Println(deck2.toString())
	
	} else { fmt.Println("  No File Present") }
		
	
	
	// ---------------------------------------
	
	// Hand.jl tests

	fmt.Println("\n*** Hand.jl tests ***")
	hand1 := initHand()
	hand2 := initHand()
	
	fmt.Println("\nHands - Initializing Variables:")
	fmt.Println("  Card 1:") ; fmt.Println("  " , hand1.cards) ; fmt.Println("  " , hand1.sorted) ; fmt.Println("  " , hand1.handType)
	fmt.Println("\n  Card 2:") ; fmt.Println("  " , hand2.cards) ; fmt.Println("  " , hand2.sorted) ; fmt.Println("  " , hand2.handType)
	
	fmt.Println("\nDrawing Cards")
	for i := 1; i <= 5; i++ {
		hand1.addCard(deck1.drawCard())
		hand2.addCard(deck2.drawCard())
	}
	
	fmt.Println("\nHand1 Functions:")
	fmt.Println(hand1.toString())
	hand1.assessHand() ; fmt.Println(hand1.toString())
	
	fmt.Println("\nHand2 Functions:")
	fmt.Println(hand2.toString())
	hand2.assessHand() ; fmt.Println(hand2.toString())
	
	// ---------------------------------------
	
	// FiveHand.jl tests

	//fmt.Println("\n*** FiveHand.jl tests ***")
	
	
	// ---------------------------------------

}