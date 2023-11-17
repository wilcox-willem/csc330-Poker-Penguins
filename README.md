README

Dec 2023, CSC330, Dr. Pounds, Mercer University

Project: FCSPHA - Jokers Wild and Statistical Improvement
Group Name: Poker Penguins
Members:
	Davis Guest
	Dirk Kotze
	Philomina Ekezie
	Shruti Senthil
	Willem Wilcox


---------------------------------------------------------------
EDITING NOTES:
	Any line with a "// " is a comment where something
	above it needs editing later. -WW
---------------------------------------------------------------
| Build Instructions |

	To build an executable, use the following command
	"go build -o PHA Card.go Deck.go FiveHand.go Hand.go"
// If any of the file structure changes

	To run, use the following
	"PHA"
// 	PHA (poker hand analyzer) can be replaced
//	with a different name if anyone would like

| Default Mode |

	By default, the program runs a game of five card stud poker with 6 hands
	then displays the players in winning order (w/ tie-breaking).

| Test File Mode |

	By adding a file path after 
// With the addition of the "-f" flag, will this still work?

| Features and Flags |

	By using various flags, the actions of the program can be changed. 
	They are listed below as, |flag| then a description of what it does.

	|-j| Adds two wild jokers to the deck, a red (XR) and a 
		 black (XB). The jokers can be any card, except those within 
		 their hand. Futhermore, the jokers will become the card that
		 maximizes its hand's score.

	|-f| Specifies that the next command line argument is a
		 handset file (handset files can include jokers)

	|-s #| This flag turns on statistical collection, where # is the number
		 of time the program will run. If no number is listed, it will run
		 1000 times by default. Furthermore, once the program completes it
		 will print the number of times of each hand ranking made. 
		 Finally, this flag can be used with the "-j" flag.
