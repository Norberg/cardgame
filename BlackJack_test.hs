import BlackJack
import Test.HUnit


tests = 
 [
	TestCase $ assertEqual "value of King" 10 
		(getValueCard (Card Hearts King)) ,
	TestCase $ assertEqual "blackjack" 21
		 (getValueHand [Card Hearts King, Card Clubs Nine, Card Spades Two]) ,
	TestCase $ assertEqual "a deck should have 52 cards" 52
		 (length createDeck),
	TestCase $ assertEqual "a deck have a total value of" 380
		 (getValueHand createDeck),
	TestCase $ assertEqual "picking a card" (Card Hearts King, tail createDeck)
		 (pickCard createDeck),
	TestCase $ assertEqual "picking a card consumes a card in the deck" 51
		 (length $ snd $ pickCard createDeck),
	TestCase $ assertEqual "picking the last card" (Card Clubs Ace, [])
		 (pickCard [Card Clubs Ace])
 ]

main = runTestTT $ TestList tests
