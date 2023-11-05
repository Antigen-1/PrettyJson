-- | SimpleJson.hs

module SimpleJson
  (
    JValue(..)
  , getString
  , getInteger
  , getDouble
  , getBool
  , getObject
  , getArray
  , isNull
  ) where

data JValue = JString String
            | JDouble Double
            | JInteger Integer
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JDouble d) = Just d
getDouble _           = Nothing

getInteger :: JValue -> Maybe Integer
getInteger (JInteger i) = Just i
getInteger _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _           = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _          = Nothing

isNull :: JValue -> Bool
isNull JNull = True
isNull _ = False

-- | Selectors
--
-- Examples:
--
-- >>> getString (JString "hello")
-- Just "hello"
--
-- >>> getString (JInteger 3)
-- Nothing
