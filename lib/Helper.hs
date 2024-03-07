module Helper
  ( liftMaybe,
    liftEither,
    getString,
    getObject,
    fromObject,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Binary.Instances.Aeson ()
import Data.HashMap.Strict as SM
import Data.Text as ST
import Hakyll

liftMaybe :: (MonadFail m) => String -> Maybe a -> m a
liftMaybe err = maybe (fail err) return

liftEither :: (MonadFail m) => Either String a -> m a
liftEither (Left err) = fail err
liftEither (Right r) = return r

getString :: A.Value -> Maybe String
getString (A.String s) = Just (ST.unpack s)
getString _ = Nothing

getObject :: (MonadFail m) => A.Value -> m A.Object
getObject (A.Object obj) = return obj
getObject _ = fail "Not an Object"

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

fromObject :: (MonadFail m) => A.Value -> m (HashMap ST.Text A.Value)
fromObject val = do
  obj <- getObject val
  return $ SM.fromList (first Key.toText <$> KeyMap.toList obj)

instance Writable A.Value where
  write p item = write p (fmap A.encode item)
