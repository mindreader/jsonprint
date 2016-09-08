{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric  #-}
module Lib
    ( printValue, encodeFixed
    ) where

import Data.Aeson
import Data.Aeson.Types (unsafeToEncoding)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.List (intersperse, sortBy)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

import Data.Scientific

import Data.Monoid
import Control.Monad.Reader
import Control.Monad.Writer

import GHC.Generics

import Data.Function



valueComp :: Value -> Value -> Ordering
valueComp = compare `on` score
  where
    score :: Value -> Int
    score Null = 0
    score (Bool _) = 0
    score (Number _) = 0
    score (String _) = 0
    score (Array a) = max (length a) 1
    score (Object o) = max (length o * 2) 1


printValue :: Monad m => Value -> m BS.ByteString
printValue val = BS.toLazyByteString . snd <$> runWriterT (runReaderT (printval' val) [])
  where

    printval' :: (MonadWriter BS.Builder m, MonadReader [T.Text] m) => Value -> m ()
    printval' Null = printpath >> tell "null" >> tell "\n"
    printval' (Number num) = printpath >> (tell . BS.string8 . formatScientific Fixed (if isInteger num then Just 0 else Nothing)) num >> tell "\n"

    printval' (String txt) = printpath >> (tell . BS.byteString . T.encodeUtf8) txt >> tell "\n"

    printval' (Bool b) = printpath >> tell (if b then "true" else "false") >> tell "\n"

    printval' v@(Array arr) = do
      flip V.imapM_ arr $ \idx v' -> do
        local ((:) (T.pack $ show idx)) (printval' v')
      printpath >> (tell . BS.lazyByteString . encode . ValueFixed) v >> tell "\n"

    printval' v@(Object arr) = do

      -- | Lets sort objects by a rough approximation of their size before printing them.
      forM_ (sortBy (valueComp `on` snd) (HM.toList arr)) $ \(k, v') -> do
        local (\old -> ("\"" <> k <> "\"") : old) (printval' v')
      printpath >> (tell . BS.lazyByteString . encode . ValueFixed) v >> tell "\n"


printpath :: (MonadWriter BS.Builder m, MonadReader [T.Text] m) => m ()
printpath = mconcat . reverse . intersperse "," . map (BS.byteString . T.encodeUtf8) <$> ask >>= tell >> tell ": "

newtype ValueFixed = ValueFixed Value deriving Generic

-- | Like encode, but numbers will not use scientific notation.
-- | Since aeson (because of json's spec) "forgets" whether it read "1" vs "1.0", any whole number will
-- | be displayed as a decimal.
encodeFixed :: Value -> Encoding
encodeFixed (Number n) = unsafeToEncoding $ BS.string8 $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
encodeFixed (Array a) =
  unsafeToEncoding "[" <>
  (mconcat $ intersperse (unsafeToEncoding ",") $ V.toList $ fmap encodeFixed a) <>
  unsafeToEncoding "]"
encodeFixed (Object o) =
  unsafeToEncoding ("{") <>
  (mconcat . intersperse (unsafeToEncoding ",") .  fmap objEnc .  HM.toList) o <> 
  unsafeToEncoding ("}")
  where
    objEnc :: (T.Text, Value) -> Encoding
    objEnc (k, v)= toEncoding k <> unsafeToEncoding ":" <> encodeFixed v
encodeFixed v = toEncoding v

instance ToJSON ValueFixed where
  toEncoding (ValueFixed v) = encodeFixed v

