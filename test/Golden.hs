module Golden where 

import Data.ByteString.Base16 as Hex
import Nostr.Keys
import Data.ByteString as BS
import Crypto.Hash.SHA256 as SHA
import Nostr.Event
import Data.Aeson as J
import Data.Text.Encoding 
import Data.Maybe

evid = Hex32 $ Hex.decodeLenient 
    "4376c65d2f232afbe9b882a35baa4f6fe8667c4e684749af565f981833ed6a65"
esig = Hex64 $ Hex.decodeLenient 
    "908a15e46fb4d8675bab026fc230a0e3542bfade63da02d542fb78b2a8513fcd0092619a2c8c1221e581946e0191f2af505dfdf8657a414dbca329186f009262"

wev = Event evid esig ev 

ev = Content
    1
    [ 
      ETag evref Nothing Nothing 
    , PTag keyref Nothing  
    ] 
    "Walled gardens became prisons, and nostr is the first step towards tearing down the prison walls."         
    1673347337
    pub

evref = Hex32 $ Hex.decodeLenient 
    "3da979448d9ba263864c4d6f14984c423a3838364ec255f03c7904b1ae77f206"

evmine = Hex32 $ Hex.decodeLenient 
    "000000000e9d97a1ab09fc381030b346cdd7a142ad57e6df0b46dc9bef6c7e2d"
trickmine = Hex32 $ Hex.decodeLenient 
    "002f79448d9ba263864c4d6f14984c423a3838364ec255f03c7904b1ae77f206"

keyref = Hex32 $ Hex.decodeLenient 
    "bf2376e17ba4ec269d10fcc996a4746b451152be9031fa48e74553dde5526bce"

pub = Hex32 $ Hex.decodeLenient 
    "6e468422dfb74a5738702a8823b9b28168abab8655faacb6853cd0ee15deee93"

    
lnpub = Hex32 . BS.drop 1 . Hex.decodeLenient $ 
    "0337694505123a12a8fadd95523dcc235898ad3b80a06e4a63ca26fed68dd0d17c"
sigTest1 = Hex64 $ Hex.decodeLenient 
    "4485de307707702f173ffbbedbc3def61b246fd93a4be8d68d096d2bb8d6acef681cfa352ea6e03d1659eb3c00376ca85d53f7f0e2cf0db7bc4f3bfecfa43341"
test1 = Hex32 . Hex.decodeLenient . Hex.encode $ SHA.hash . encodeUtf8 $ "test1"


mepriv = Hex96 . Hex.decodeLenient $ 
    "0f8e0da6260dd437b537c6f6ba8e334c32c567746e3d63ade096ba82bee4e053a2ff0f5203d6bdb6f73cf71b9831e7548f5ee274eb756e62f680a6c8d0921eb596d56089e42c11d7910a189565cefb7f2c953cad35aa95a4d61eea207e304bc7"

fromAmyth :: Event 
fromAmyth = fromJust . J.decode $  
   "{\"content\":\"rLnk1zAI6yUvdT/CkDv1ow==?iv=RpI2bryHCi6n6qKfRpVB/Q==\",\"created_at\":1698097606,\"id\":\"1f36f2b8ce53528959ba97fb433d76034f343bcdf95e092d86b36c923822c5a2\",\"kind\":4,\"pubkey\":\"394f5711480e96d231dfd4ed882547cae4e04474ef335fd0e60fb301e99ea2fb\",\"sig\":\"7617a4790550b060d22bd5c27eb6f0015d3f1c6507eb8b87b84571481e54b2fb4bf9403e508499712107b629ecf890ba1ef0370a4544642a172ccc0606925733\",\"tags\":[[\"p\",\"b51e92d0c8a680f6626e75eb74e25e8f54e731981bf73cf7b6bdd603520fffa2\"]]}"


pubamyth = Hex32 . Hex.decodeLenient $
    "394f5711480e96d231dfd4ed882547cae4e04474ef335fd0e60fb301e99ea2fb"
    -- encryptE me' pubamyth "domino" >>= castAll pool . Submit  
