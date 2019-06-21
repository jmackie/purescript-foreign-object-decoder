# `purescript-foreign-object-decoder`

Automatically decode (`Foreign`) objects coming from javascript into PureScript
types.

## Working example

The interesting functions are `decodeForeign` and `decodeObject`, both of which come
from `Foreign.Object.Decode`...

```purescript
-- Example.purs
module Example (main) where

import Prelude
import Effect (Effect)
import Effect.Exception as Exception
import Data.Foldable (intercalate)
import Foreign (Foreign)
import Effect.Console as Console
import Data.Either (Either(..))

import Foreign.Object.Decode (decodeForeign)

type Options =
  { port :: Int
  , host :: String
  , keys :: Array Int
  }

-- | Function to be called from javascript land.
main :: Foreign -> Effect Unit
main =
  decodeForeign >>> case _ of
    Left errors -> Exception.throw (intercalate "\n" errors)
    Right options -> run options

-- | _Actually_ run the program given the necessary `Options`.
run :: Options -> Effect Unit
run _ = Console.log "nice"
```

```javascript
// index.js
import { main } from "./output/Example/index.js";
main({ port: 4242, host: "localhost", keys: [] });
```

## Why?

This is useful if:

- You're writing functions (entrypoints or otherwise) to be
  called from javascript _with arguments_, and
- You are not necessarily in control of that calling code (in which
  case `unsafeCoerce` or similar would do), or
- You are in control of the calling code but would like to catch
  your own errors gracefully
