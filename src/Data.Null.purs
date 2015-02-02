module Data.Null where

import Data.Maybe

foreign import data N :: * -> *

foreign import null "var $$null = null" :: forall val. N val

foreign import asNull
  "function asNull(val) { return val }"
  :: forall val. val -> N val

foreign import unnullUnsafe
  "function unnullUnsafe(val) { return val }"
  :: forall val. N val -> val

foreign import nullish
  "function nullish(val) { return val == null }"
  :: forall val. N val -> Boolean

unnull :: forall val. N val -> Maybe val
unnull val =
  if nullish val
  then Nothing
  else Just (unnullUnsafe val)

unnullOr :: forall val. N val -> val -> val
unnullOr val alt =
  if nullish val
  then alt
  else (unnullUnsafe val)

foreign import falsey
  "function falsey(val) { return !val }"
  :: forall val. N val -> Boolean

foreign import truthy
  "function truthy(val) { return !!val }"
  :: forall val. N val -> Boolean
