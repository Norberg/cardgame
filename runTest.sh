while true; do
	runhaskell BlackJack_test.hs
	date
	inotifywait -e modify -r  . --exclude '(.pyc|.swp|.png)'
done
