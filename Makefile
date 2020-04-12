all: main.js

main.js: src/Main.elm
	elm make --output=main.js src/Main.elm
