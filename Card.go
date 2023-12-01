package main

import (
	"fmt"
)

// Represents a playing card.
// This struct defines a card object with a rank and suit.
type Card struct {
	rank int
	suit int
}


// Initializes a new card with the specified rank and suit.
// param: r - int representing the card's rank.
// param: s - int representing the card's suit.
func initCard(r int, s int) *Card {
	return &Card {
		rank : r,
		suit : s,
	}
}


// Returns a string representation of the card.
// return: String representing the card object.
func (c *Card) toString() string {
	var suitLabel string
	if (c.suit == 0) {suitLabel = "D"
	} else if (c.suit == 1) {suitLabel = "C"
	} else if (c.suit == 2) {suitLabel = "H"
	} else if (c.suit == 3) {suitLabel = "S"
	} else if (c.suit == 4) {suitLabel = "R"
	} else if (c.suit == 5) {suitLabel = "B"}

	var face string
	if (c.rank == 11) {face = "J"
	} else if (c.rank == 12) {face = "Q"
	} else if (c.rank == 13) {face = "K"
	} else if (c.rank == 14) {face = "A"
	} else if (c.rank == 15) {face = "X"}

	if (face == "") {
		return fmt.Sprintf("%d%s", c.rank, suitLabel)
	} 
	return face + suitLabel
}


// Compares this card with another card based on their ranks.
// param: other - Card representing the card to be compared.
// return: int representing the difference between the two card's ranks.
func (c *Card) compare(other *Card) int {
	return c.rank - other.rank
}