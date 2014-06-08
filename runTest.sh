while true; do
	cabal test
	date
	inotifywait -e modify -r  . --exclude '(.pyc|.swp|.png)'
done
