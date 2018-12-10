all: bin/svgedit.js

bin/%.js: src/%.elm
	elm make $< --output=$@
