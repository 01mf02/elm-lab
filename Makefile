all: bin/Main.js

bin/%.js: src/%.elm
	elm make $< --output=$@
