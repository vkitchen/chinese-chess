all: main.js

main.js: src/Main.elm src/Xiangqi.elm
	elm make --output=main.js src/Main.elm
