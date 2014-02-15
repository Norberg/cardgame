module BlackJack( Card(..), Color(..), Rank(..) , Hand, Deck, getValueCard
                , getValueHand, createDeck, pickCard) where

import System.Random


data Color = Hearts | Dimonds | Spades | Clubs deriving (Show, Eq, Enum)
data Rank = King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five 
	  | Four | Three | Two | Ace deriving (Show, Eq, Enum)

data Card = Card{ color :: Color
                , rank :: Rank
} deriving (Show, Eq)

type Hand = [Card]

type Deck = [Card]

getValueRank :: Rank -> Int
getValueRank Ace = 11
getValueRank King = 10
getValueRank Queen = 10
getValueRank Jack = 10
getValueRank Ten = 10
getValueRank Nine = 9
getValueRank Eight = 8
getValueRank Seven = 7
getValueRank Six = 6
getValueRank Five = 5
getValueRank Four = 4
getValueRank Three = 3
getValueRank Two = 2

getValueRankLow :: Rank -> Int
getValueRankLow Ace = 1
getValueRankLow rank = getValueRank rank

getValueCard :: Card -> Int
getValueCard card = getValueRank $ rank card

getValueHand :: Hand -> Int
getValueHand hand = sum $ map getValueCard hand

shouldDealerStop :: Hand -> Bool
shouldDealerStop hand = getValueHand hand > 17

createDeck :: Deck
createDeck = [Card color rank | color <- [Hearts .. Clubs], rank <- [King .. Ace]]

shuffle :: [a] -> IO [a]
shuffle e = shuffle' e []
    where
        shuffle' [] acc = return acc
        shuffle' e acc = 
            do
                p <- randomRIO(0, length e - 1)
                let (lead, x:xs) = splitAt p e
                shuffle' (lead ++ xs) (x:acc)
				
pickCard :: Deck -> (Card, Deck)
pickCard deck = (head deck, tail deck)

main = do 
    deck <- shuffle createDeck
    putStrLn $ show $ deck