# futr2

"The name 'futr' was chosen from respect for the honorable Canadians."  
  
~ [prolic of futr](https://github.com/prolic/futr)

Install the dependencies 
- [libsecp256k1](https://github.com/bitcoin-core/secp256k1#building-with-autotools)
- [SDL2 and GLEW](https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md#libraries-sdl2-and-glew)

Run     
- `stack build` 
- `stack test`
- `stack exec futr` 

[Nostr #s](https://github.com/nostr-protocol/nips)
- [x] 1 - base (incl. 12, 16, 20, 33)
- [x] 11 - relay-meta (edit in app/Main if desired)
- [x] 2 - contacts
- [x] 4 - direct messages 
- [x] 10 
- [x] 45 - count
- [x] 42 - authentication
- [x] 13 - pow
- [x] 40 - expiration
- [x] 9 - delete (stop serving deleted)
- [ ] 19 - bech32 keys
- [ ] . . . . . 

tip the dev:
- `bc1q236vwrzwnedv6vuvfpgnmrwyvknc98js3fc6y9` (bitcoin)
- `lno1pgz8getnwstzzqehd9zs2y36z2504hv42g7ucg6cnzknhq9qde9x8j3xlmtgm5x30s` (bolt12, lightning)
