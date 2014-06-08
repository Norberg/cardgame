while true; do
	cabal test
	cabal build
	cabal run
	inotifywait -e modify -r  . --exclude '(.pyc|.swp|.png)'
done
