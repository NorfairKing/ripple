build:
	mkdir -p public
	mkdir -p public/css

	cp src/index.html public
	cp src/site.webmanifest public
	cp -r src/css public/css
	
	elm make src/Main.elm --output=elm.js --optimize
	uglifyjs elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output public/elm.js

live:
	# Elm Live: https://github.com/wking-io/elm-live
	elm-live src/Main.elm --open --hot --dir=src --start-page=index.html --port 8080 -- --output=src/elm.js --debug
