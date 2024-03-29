module BlackJack(
    Card(..),
    Color(..),
    Rank(..),
    Winner(..),
    Hand,
    Deck,
    getValueCard,
    getValueHand,
    createDeck,
    pickCard,
    pickCards,
    shouldDealerStop,
    getWinner,
    shuffle,
    play,
    scoreBoard,
    dealerFinishPlay
) where

import System.Random


data Color = Hearts | Dimonds | Spades | Clubs deriving (Show, Eq, Enum)
data Rank = King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five 
	  | Four | Three | Two | Ace deriving (Show, Eq, Enum)

data Card = Card{ color :: Color
                , rank :: Rank
} deriving (Eq)

instance Show Card where
    show (Card color rank) = show rank ++ " of " ++ show color

type Hand = [Card]

type Deck = [Card]

data Winner = Player | House | Draw deriving(Show, Eq)

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

pickCards :: Deck -> Int -> (Hand, Deck)
pickCards deck n = (take n deck, drop n deck)

getWinner :: Deck -> Deck -> Winner
getWinner player house
    | playerScore > 21 = House
    | houseScore > 21 = Player
    | playerScore > houseScore = Player
    | houseScore > playerScore = House
    | otherwise = Draw
    where 
        playerScore = getValueHand player
        houseScore = getValueHand house


dealerFinishPlay :: Hand -> Deck -> (Hand, Deck)
dealerFinishPlay hand deck
    | shouldDealerStop hand = (hand, deck)
    | otherwise = dealerFinishPlay (new_card : hand) new_deck
    where
        (new_card, new_deck) = pickCard deck

scoreBoard :: Hand -> String
scoreBoard hand = "Score: " ++ show (getValueHand hand) ++ "\n" ++
    "Hand: " ++ show(hand)

playerPlay :: Deck -> Hand -> IO (Hand, Deck)
playerPlay deck hand = do
    let (newCard, newDeck) = pickCard deck
    let newHand = newCard : hand
    putStrLn ("Player: " ++ scoreBoard newHand)
    shouldContinue <- drawOrStop
    if shouldContinue then 
        do
            playerPlay newDeck newHand
    else
        do
            return (newHand, newDeck)

drawOrStop :: IO Bool
drawOrStop = do
    putStrLn "(D)raw or (S)top?"
    args <- getLine
    let arg = head args
    case arg of
        'D' -> return True
        'd' -> return True
        'S' -> return False
        's' -> return False
        _  -> drawOrStop

play :: IO()
play = do 
    deck <- shuffle createDeck
    putStrLn "Black Jack!"
    putStrLn "Dealer Picks cards,"
    let (dealerInitHand, deck1) = pickCards deck 1
    putStrLn ("Dealer: " ++ scoreBoard dealerInitHand)
    (playerHand, deck2) <- playerPlay deck1 []
    let (dealerHand, deck3) = dealerFinishPlay dealerInitHand deck2
    putStrLn ("Dealer: " ++ scoreBoard dealerHand)
    putStrLn ("Winner is:" ++ show ( getWinner playerHand dealerHand)) 
