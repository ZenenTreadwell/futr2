# futr

Time-travelling, multiverse-hopping Nostr ecosystem.

The name futr was chosen from respect for the honorable Canadians, apparently.

## Features

At the moment, futr has two main applications built in:

### Relay

Relay implements a standard Nostr relay, supporting the following NIPs:
[1,2,4,9,10,45,42,40,12,16,20,33]

### Futr

Futr is a desktop client for the Nostr network. *Still under construction.*

## Installation

Futr is built using the Haskell Tool Stack, or [stack](https://docs.haskellstack.org/en/stable/) for short.

Before building, make sure you install the dependencies:
- [libsecp256k1](https://github.com/bitcoin-core/secp256k1#building-with-autotools)
- [SDL2 and GLEW](https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md#libraries-sdl2-and-glew)

On Debian, these can be installed with the following command:
`sudo apt install libsecp256k1-dev libsdl2-dev libglew-dev`

Once installed, build and run with stack:     
- `stack build` 
- `stack test`
- `stack exec futr` 
- `stack exec relay`  

## Configuration

This relay shares information about the operator according to NIP-11 specifications. This information can be configured in the futr.conf file, which exists in ~/.futr/ by default. 

#### Example configuration
```
name = 
contact =  
description =  
pubkey = 
port = 9481
```

