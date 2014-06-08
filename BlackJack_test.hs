import BlackJack
import Test.HUnit


tests = 
 [
    TestCase $ assertEqual "value of King" 10 
               (getValueCard (Card Hearts King)) ,
    TestCase $ assertEqual "blackjack" 21
               (getValueHand [Card Hearts King, Card Clubs Nine, Card Spades Two]),
    TestCase $ assertEqual "a deck should have 52 cards" 52
               (length createDeck),
    TestCase $ assertEqual "a deck have a total value of" 380
               (getValueHand createDeck),
    TestCase $ assertEqual "picking a card" (Card Hearts King, tail createDeck)
               (pickCard createDeck),
    TestCase $ assertEqual "picking a card consumes a card in the deck" 51
               (length $ snd $ pickCard createDeck),
    TestCase $ assertEqual "picking the last card" (Card Clubs Ace, [])
               (pickCard [Card Clubs Ace]),
    TestCase $ do
        deck <- shuffle createDeck
        assertEqual "shuffle deck have 52 cards" 52 (length deck)
        assertBool "shuffled deck not equal to unshuffled deck"
                    (createDeck /= deck)
        ,
    TestCase $ do
        let deck1 = createDeck
        let (card1, deck2) = pickCard deck1
        let (card2, deck3) = pickCard deck2
        let dealerHand = [card1, card2]
        let (card3, deck4) = pickCard deck3
        let (card4, deck5) = pickCard deck4
        let (card5, deck6) = pickCard deck5
        let playerHand = [card3, card4, card5]
        assertEqual "dealer score is" 20 (getValueHand dealerHand)
        assertBool "dealer should stop" (shouldDealerStop dealerHand)
        assertEqual "player score is" 29 (getValueHand playerHand)
        assertEqual "house wins" House (getWinner playerHand dealerHand)

    
 ]

main = runTestTT $ TestList tests
