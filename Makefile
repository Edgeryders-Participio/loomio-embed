
UGLIFYJS=./node_modules/uglify-js/bin/uglifyjs


out/main.js: src/Main.elm src/Loomio.elm
	elm make --optimize src/Main.elm --output=out/loomio-embed.js
	$(UGLIFYJS) out/loomio-embed.js --compress \
		    'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		    | $(UGLIFYJS) --mangle --output=out/loomio-embed.min.js
