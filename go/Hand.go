package main

import (
	"sort"
)

// Represents a hand of playing cards.
// This class defines a hand object that can hold a collection of card objects.
type Hand struct {
	cards []*Card
	sorted []*Card
	handType int
}


// Initializes a new hand with an empty list of cards, its sorted variant, and a default hand type of 0.
func initHand() *Hand {
	return &Hand {
		cards : make([]*Card, 0),
		sorted : make([]*Card, 0),
		handType : 0,
	}
}


// Adds a card to the hand's list of cards.
// param: card - Card representing the input card.
func (h *Hand) addCard(card *Card) {
	h.cards = append(h.cards, card)
}


// Returns a string representation of the hand.
// return: string representing the hand object.
func (h *Hand) toString() string {
	list := ""

	for i := 1; i <= len(h.cards); i++ {
		if (h.cards[i - 1].rank != 10) { list += " " }

		list += h.cards[i - 1].toString()

		if (i == 0 || i % 5 != 0) { list += " " }
	}

	if h.handType == 0 { return list
	} else if h.handType == 10 { list += " - Royal Straight Flush"
	} else if h.handType == 9 { list += " - Straight Flush"
	} else if h.handType == 8 { list += " - Four of a Kind"
	} else if h.handType == 7 { list += " - Full House"
	} else if h.handType == 6 { list += " - Flush"
	} else if h.handType == 5 { list += " - Straight"
	} else if h.handType == 4 { list += " - Three of a Kind"
	} else if h.handType == 3 { list += " - Two Pair"
	} else if h.handType == 2 { list += " - Pair"
	} else { list += " - High Card" }

	return list
}


// Compares this hand with another hand based on their ranks.
// Used to sort each hand type and tiebreakers accordingly.
// param: other - Hand representing the hand to be compared.
// return: int representing the which hand is greater or less than.
func (h *Hand) compare(other *Hand) int {
	h.assessHand()
	other.assessHand()

	typeComparison := h.handType - other.handType

	return h.compareHelper(other, typeComparison, 0)
}


// Recursive helper method for the compare_hand method.
// param: other - Hand representing the hands to compare to.
// param: diff - int representing the difference of the hands in the current iteration.
// param: pass - int representing the number for the current pass.
// return: int representing the difference between the two hands.
func (h *Hand) compareHelper(other *Hand, diff int, pass int) int {
	if (diff != 0) { return diff }

	thisBreaker := h.getTieBreakerCard(pass)
	otherBreaker := other.getTieBreakerCard(pass)

	rankDiff := thisBreaker.rank - otherBreaker.rank
	suitDiff := thisBreaker.suit - otherBreaker.suit

	if (pass < 2 && (h.handType == 2 || h.handType == 3)) {
		if (rankDiff == 0) {
			return h.compareHelper(other, rankDiff, pass + 1)
		}
		return rankDiff
	}

	if (rankDiff == 0) { return suitDiff }
	return rankDiff
}


// Analyzes the current collection of cards in the hand and determines its hand type.
// The sorted instance variable is updated to contain the cards sorted by rank.
func (h *Hand) assessHand() {

	// Initializes variables used in Joker assessment
	jokerIndex1 := -1
	jokerIndex2 := -1

	jokerSuit1 := 0
	jokerSuit2 := 0

	availCards1 := make([]*Card, 1)
	availCards2 := make([]*Card, 1) // Must have length 1 in order for the joker to be processed

	// Checks to see if there is a joker in the hand
	for i, c := range h.cards {

		if c.rank == 15 { // hand has a joker
			if jokerIndex1 == -1 {
				jokerIndex1 = i
				availCards1 = h.generateAvailableCards(c.suit)
				jokerSuit1 = c.suit

			} else { // hand has a second joker
				jokerIndex2 = i
				availCards2 = h.generateAvailableCards(c.suit)
				jokerSuit2 = c.suit
				break
			}
		}

	}

	topScore := 0          // Highest hand rank generated from the different possibility of joker substitutions
	topSorted := h.sorted  // Keeps track of the sorted version for the highest ranking hand possibility
						   // 	if not kept track of, the instance of the highest version is written over
						   //   (it cannot end early as there is a possibility of a better hand after)
	currScore := 0

	if jokerIndex1 != -1 { // if there is no joker

		for _, c1 := range availCards1 { // for every card in joker1 possibilities

			for _, c2 := range availCards2 { // for every card in joker2 possibilities
											 // (the reason the initialization of availCards2 needs to be 1)

				h.cards[jokerIndex1] = initCard(c1.rank, c1.suit) // sets the joker1 card of the hand to the current possibility

				if jokerIndex2 != -1 { // if there is a joker2
					h.cards[jokerIndex2] = initCard(c2.rank, c2.suit) // sets the joker2 card of the hand to the current possibility
				}

				h.sortHand()

				if (h.isRoyalStraightFlush()) { currScore = 10
				} else if (h.isStraightFlush()) { currScore = 9
				} else if (h.isFourOfAKind()) { currScore = 8
				} else if (h.isFullHouse()) { currScore = 7
				} else if (h.isFlush()) { currScore = 6
				} else if (h.isStraight()) { currScore = 5
				} else if (h.isThreeOfAKind()) { currScore = 4
				} else if (h.isTwoPair()) { currScore = 3
				} else if (h.isPair()) { currScore = 2
				} else { currScore = 1 }

				h.cards[jokerIndex1] = initCard(15, jokerSuit1) // sets the joker1 card back to its original value
															    // (this has to happen for the correct hand printing)

				if jokerIndex2 != -1 { // sets the joker2 card back to its original value
					h.cards[jokerIndex2] = initCard(15, jokerSuit2)
				}

				if currScore > topScore {
					topScore = currScore 
					topSorted = h.sorted
				}
			}
		}

		// After all possibile assessments, sets the hand's handType and sorted array to the highest possible values
		h.handType = topScore
		h.sorted = topSorted

	} else { // normal hand evaluation

		h.sortHand()

		if (h.isRoyalStraightFlush()) { h.handType = 10
		} else if (h.isStraightFlush()) { h.handType = 9
		} else if (h.isFourOfAKind()) { h.handType = 8
		} else if (h.isFullHouse()) { h.handType = 7
		} else if (h.isFlush()) { h.handType = 6
		} else if (h.isStraight()) { h.handType = 5
		} else if (h.isThreeOfAKind()) { h.handType = 4
		} else if (h.isTwoPair()) { h.handType = 3
		} else if (h.isPair()) { h.handType = 2
		} else { h.handType = 1 }
	
	}

	
}


// Determines if the hand is a royal straight flush.
// return: Boolean representing if the hand is an RSF.
func (h *Hand) isRoyalStraightFlush() bool {
	rankList := h.getRankList()

	return h.isStraightFlush() &&
		rankList[0] == 10 &&
		rankList[4] == 14
}


// Determines if the hand is a straight flush.
// return: Boolean representing if the hand is an SF.
func (h *Hand) isStraightFlush() bool {
	return h.isStraight() && h.isFlush()
}


// Determines if the hand is a straight flush.
// return: Boolean representing if the hand is an SF.
func (h *Hand) isFourOfAKind() bool {
	rankList := h.getRankList()

	return (rankList[0] == rankList[3]) ||
		(rankList[1] == rankList[4])
}


// Determines if the hand is a full house.
// return: Boolean representing if the hand is a FH.
func (h *Hand) isFullHouse() bool {
	rankList := h.getRankList()

	return (rankList[0] == rankList[1] && rankList[2] == rankList[4]) ||
		(rankList[0] == rankList[2] && rankList[3] == rankList[4])
}


// Determines if the hand is a flush.
// return: Boolean representing if the hand is a flush.
func (h *Hand) isFlush() bool {
	suit := h.sorted[0].suit

	for i := 0; i < 4; i++ {
		if h.sorted[i].suit != suit {
			return false
		}
	}

	return h.sorted[4].suit == suit
}


// Determines if the hand is a straight.
// return: Boolean representing if the hand is a straight.
func (h *Hand) isStraight() bool {
	rankList := h.getRankList()

	if rankList[4] == 14 && rankList[0] == 2 {
		rankList[4] = 1
		sort.Ints(rankList)
	}

	for i := 0; i < 4; i++ {
		if rankList[i+1] != rankList[i]+1 {
			return false
		}
	}

	return true
}


// Determines if the hand is a three of a kind.
// return: Boolean representing if the hand is a TOAK.
func (h *Hand) isThreeOfAKind() bool {
	rankList := h.getRankList()

	return (rankList[0] == rankList[2]) ||
		   (rankList[1] == rankList[3]) ||
		   (rankList[2] == rankList[4])
}


// Determines if the hand is a two pair.
// return: Boolean representing if the hand is a TP.
func (h *Hand) isTwoPair() bool {
	rankList := h.getRankList()

	return (rankList[0] == rankList[1] && rankList[2] == rankList[3]) ||
		(rankList[0] == rankList[1] && rankList[3] == rankList[4]) ||
		(rankList[1] == rankList[2] && rankList[3] == rankList[4])
}


// Determines if the hand is a pair.
// return: Boolean representing if the hand is a pair.
func (h *Hand) isPair() bool {
	rankList := h.getRankList()

	for i := 0; i < 4; i++ {
		if rankList[i] == rankList[i+1] {
			return true
		}
	}

	return false
}


// Determines the tie breaking card of the hand depending on its handType.
// param pass - int representing the pass number.
// return: Card representing the tie-breaker card.
func (h *Hand) getTieBreakerCard(pass int) *Card {
	if h.handType == 10 { return h.sorted[4]

	} else if h.handType == 9 {
		if h.sorted[4].rank == 14 && h.sorted[0].rank == 2 {
			return h.sorted[3]
		}
		return h.sorted[4]

	} else if h.handType == 8 { return h.sorted[2]
	} else if h.handType == 7 { return h.sorted[2]
	} else if h.handType == 6 { return h.sorted[4]
	} else if h.handType == 5 { return h.sorted[4]
	} else if h.handType == 4 { return h.sorted[2]
	} else if h.handType == 3 {
		pairList := make([]*Card, 0)

		for i := 1; i < len(h.sorted); i++ {
			currentCard := h.sorted[i]
			previousCard := h.sorted[i - 1]

			if currentCard.rank == previousCard.rank {
				pairList = append(pairList, currentCard)
			}
		}

		max := pairList[0]
		min := pairList[0]

		for _, card := range pairList {
			if card.rank > max.rank {
				max = card
			}
			if card.rank < min.rank {
				min = card
			}
		}

		if pass == 0 { return max
		} else if pass == 1 { return min
		} else { return h.getKicker() }

	} else if h.handType == 2 {
		var ret *Card = nil

		for i := 1; i < len(h.sorted); i++ {
			currentCard := h.sorted[i]
			previousCard := h.sorted[i - 1]

			if currentCard.rank == previousCard.rank {
				ret = currentCard
				break
			}
		}

		if pass == 0 { return ret
		} else if pass == 1 { return h.getKicker() }
	}

	return h.sorted[4]
}

// Helper method to get a sorted list of ranks in the hand.
// return: list representing the ranks in ascending order.
func (h *Hand) getRankList() []int {
	rankList := make([]int, 0)

	for i := 0; i < len(h.cards); i++ {
		rankList = append(rankList, h.cards[i].rank)
	}

	sort.Ints(rankList)

	return rankList;
}

// Gets the kicker card for pairs and two pairs.
// return: Card representing the kicker card.
func (h *Hand) getKicker() *Card {
	nonPairList := make([]*Card, 0)

	for i := 1; i < len(h.sorted); i++ {
		currentCard := h.sorted[i]
		previousCard := h.sorted[i - 1]

		if currentCard.rank != previousCard.rank {
			nonPairList = append(nonPairList, currentCard)
		}
	}

	max := nonPairList[0]

	for _, card := range nonPairList {
		if card.rank > max.rank {
			max = card
		}
	}

	return max
}


// Sets the sorted instance variable to a sorted version of a provided hand.
func (h *Hand) sortHand() {
	h.sorted = make([]*Card, 0)
	for i := 0; i < 5; i++ {
		h.sorted = append(h.sorted, initCard(h.cards[i].rank, h.cards[i].suit))
	}
	
	for j := 0; j < 4; j++ {
		for i := 0; i < 4; i++ {
			if h.sorted[i].compare(h.sorted[i + 1]) > 0 {
				temp := initCard(h.sorted[i + 1].rank, h.sorted[i + 1].suit)
				h.sorted[i + 1] = h.sorted[i]
				h.sorted[i] = temp
			}
		}
	}
}


// Generates an array to keep track of every card that can be used as a joker.
func (h *Hand) generateAvailableCards(c int) []*Card {
	availCards := make([]*Card, 0)

	for suit := 0; suit <= 3; suit ++ {

		// Optimization to end early if not the right color
		if ((suit == 0 || suit == 2 ) && c == 4) ||	  // if red and suit is 0 or 2
		   ((suit == 1 || suit == 3 ) && c == 5) {    // if black and suit is 1 or 3

			for rank := 2; rank <= 14; rank ++ {

				if (contains(h.cards, rank, suit)) {continue}
				availCards = append(availCards, initCard(rank, suit))

			}
		}

	}

	return availCards

}


// Helper method to see if a certain card of rank r and suit s is inside the array.
func contains(cards []*Card, r int, s int) bool {
	for _, c := range cards {
		if c.rank == r && c.suit == s{
			return true
		}
	}
	return false
}