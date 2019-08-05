* develop

elm-live src/Main.elm --open -start-page=index.html -- --output=main.js --debug


*release

elm make src/Main.elm  --output=main.js --optimize
