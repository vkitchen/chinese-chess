# Chinese Chess

An implementation of (networked) Chinese Chess written in Elm 0.19.1 playable here [http://vaughan.kitchen/games/chinese-chess](http://vaughan.kitchen/games/chinese-chess)

# Build instructions
Either `make` or `elm make --output=main.js src/Main.elm` (if you don't want to use make)

# TODO
* Detect attempting to join a room for another game
* Fix modal flash on join
* Fix jump back/piece duplication bug
* Seperate out generic lobby code
* Restructure optionals in model
* Implement AI mode
* Profit
