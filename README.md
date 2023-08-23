# futr2

nostr client from future

`stack build`

recomment to run client through proxy: 

`torify <path to futr>`
`stack exec futr`

no ui will print to terminal

ghci tip: 
```
$ pkg-config --cflags --libs libsecp256k1
<FLAGS>
$ stack ghci --ghc-options "<FLAGS>"
\> 
```

dependencies 
- libsecp256k1
- sdl2


there are some tests

`stack test` 

