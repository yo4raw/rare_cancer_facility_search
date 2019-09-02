* develop

elm-live src/Main.elm --open -start-page=index.html -- --output=main.js --debug


*release

elm make src/Main.elm  --output=main.js --optimize


*docker
docker build -t apache2 .
docker run -dit --name app -p 2222:80 apache2



