module Test.Main where

import Prelude
import Data.Maybe (Maybe(..), fromJust)
import Data.String (length)
import Data.UUID (genUUID, genv3UUID, genv5UUID, parseUUID, toString)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  log "UUID tests"
  
  uuid1 <- genUUID
  assert $ (length <<< toString) uuid1 == 36
  
  let uuid3 = genv3UUID "foo" uuid1
  assert $ (length <<< toString) uuid3 == 36
  
  let uuid5 = genv5UUID "foo" uuid1
  assert $ (length <<< toString) uuid5 == 36
  
  let uuidStr = "d0778cf2-3a4c-42ef-acbd-1269b6bec204"
  let parsed = parseUUID uuidStr
  assert $ uuidStr == (toString $ unsafePartial $ fromJust parsed)
  
  let parsedInvalid = parseUUID "foo"
  assert $ parsedInvalid == Nothing
  
  let showUUID = "(UUID d0778cf2-3a4c-42ef-acbd-1269b6bec204)"
  assert $ showUUID == (show $ unsafePartial $ fromJust parsed)
  
  log "All done!"
