while true; do
	ghc --make BlackJack_test.hs
	ghc --make Play.hs
	#./BlackJack_test #+RTS -s
	./Play
	date
	inotifywait -e modify -r  . --exclude '(.pyc|.swp|.png)'
done
