-- | SimpleJson.hs
{-# LANGUAGE TypeSynonymInstances #-}

module SimpleJson
  (
    JValue(..),
    JAry(..),
    JObj(..),
    JSONError,
    JSON,
    fromJValue,
    toJValue,
    whenRight,
    mapEithers
  ) where

data JValue = JString String
            | JDouble Double
            | JInteger Integer
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
              deriving (Eq, Ord, Show, Read)
newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show, Read)
newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show, Read)

type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

returnJSONError t = Left ("Not a JSON " ++ t)

instance JSON JValue where
  toJValue = id
  fromJValue = Right
instance JSON String where
  toJValue = JString
  fromJValue (JString s) = Right s
  fromJValue _ = returnJSONError "string"
instance JSON Integer where
  toJValue = JInteger
  fromJValue (JInteger i) = Right i
  fromJValue _ = returnJSONError "integer"
instance JSON Double where
  toJValue = JDouble
  fromJValue (JDouble d) = Right d
  fromJValue _ = returnJSONError "double"
instance JSON Bool where
  toJValue = JBool
  fromJValue (JBool b) = Right b
  fromJValue _ = returnJSONError "bool"

-- Utilities
whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)
mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

instance (JSON a) => JSON (JAry a) where
  toJValue = jaryToJValue
    where jaryToJValue = JArray . JAry . map toJValue . fromJAry
  fromJValue = jaryFromJValue
    where jaryFromJValue (JArray (JAry a)) =
            whenRight JAry (mapEithers fromJValue a)
          jaryFromJValue _ = returnJSONError "array"
instance (JSON a) => JSON (JObj a) where
  toJValue = JObject . JObj . map (\(x,y) -> (x,toJValue y)) . fromJObj

  fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
    where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
  fromJValue _ = returnJSONError "object"

-- | Selectors and constructors
--
-- Examples:
--
-- >>> fromJValue (toJValue "hello") :: Either JSONError String
-- Right "hello"
--
-- >>> toJValue 3
-- JInteger 3
--
-- >>> whenRight fromJAry (fromJValue (toJValue (JAry [1.1,1.2,1.3,1.4,1.5]))) :: Either JSONError [Double]
-- Right [1.1,1.2,1.3,1.4,1.5]
