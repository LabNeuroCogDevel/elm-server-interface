# elm-server-interface
Server interface written in Elm

build with
``` 
./clean.sh && ./build.sh
```

## to play with e.g. Contact

```
# make sure module name is "Main"
sed 's/^module .* exposing .*/module Main exposing (..)/' src/Contact.elm
# save output to text.js
elm make src/Contact.elm --output build/test.js
# load up test index
firefox 0.0.0.0/test_index.html
```
