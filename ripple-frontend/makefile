build:
	mkdir -p public
	mkdir -p public/css

	cp src/index.html public
	cp src/site.webmanifest public
	cp -r src/css public/css
	
	elm make src/Main.elm --output=app.js --optimize
	uglifyjs app.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output public/app.js

live:
	# Elm Live: https://github.com/wking-io/elm-live
	elm-live src/Main.elm --open --hot --dir=src --start-page=index.html --port 8080 --proxy-prefix=/ --proxy-host=http://localhost:9701/ -- --output=src/app.js --debug
