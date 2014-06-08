import System.Exit
import Test.HUnit

import BlackJack
import TestDecks

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
    ,
    TestCase $ do
        let initDeck = test_deck_1
        let initHand = [] :: Hand
        let (dealerHand, deck) = dealerFinishPlay initHand initDeck 
        assertEqual "dealers score is " 18 (getValueHand dealerHand) 
        let (dealerHand2, deck2) = dealerFinishPlay initHand deck 
        assertEqual "dealers score is " 21 (getValueHand dealerHand2) 
        let (dealerHand3, deck3) = dealerFinishPlay initHand deck2 
        assertEqual "dealers score is " 27 (getValueHand dealerHand3)
    ,
    TestCase $ do
        let initDeck = test_deck_1
        let (dealerHand1, deck1) = pickCards initDeck 1
        assertEqual "dealers score is " 6 (getValueHand dealerHand1)
        let (playerHand1, deck2) = pickCards deck1 1
        assertEqual "player score is " 10 (getValueHand playerHand1)
        let (playerDrawnCard1, deck3) = pickCard deck2
        let playerHand2 = playerDrawnCard1 : playerHand1
        assertEqual "player score is " 12 (getValueHand playerHand2)
        let (playerDrawnCard2, deck4) = pickCard deck3
        let playerHand3 = playerDrawnCard2 : playerHand2
        assertEqual "player score is BUSTED!" 22 (getValueHand playerHand3)
        let (dealerHand2, deck5) = dealerFinishPlay dealerHand1 deck4 
        assertEqual "dealers score is " 22 (getValueHand dealerHand2)
        assertEqual "house wins" House (getWinner playerHand3 dealerHand2)
        assertEqual "player stoped earlier.." Player (getWinner playerHand2 dealerHand2)
        
 ]





main = do
    result <- runTestTT $ TestList tests
    let errs = errors result
        fails = failures result
    System.Exit.exitWith (codeGet errs fails)

codeGet errs fails
 | fails > 0       = ExitFailure 2
 | errs > 0        = ExitFailure 1
 | otherwise       = ExitSuccess
