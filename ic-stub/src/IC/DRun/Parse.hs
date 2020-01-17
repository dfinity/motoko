{-# LANGUAGE ScopedTypeVariables #-}
module IC.DRun.Parse where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import qualified Text.Hex as H
import qualified Data.Text as T
import Data.Char
import Control.Exception
import Data.Word
import Text.Read

type MethodName = String
type Payload = B.ByteString
type Id = B.ByteString

data Ingress
    = Install Id FilePath Payload
    | Update Id MethodName Payload
    | Query Id MethodName Payload
    deriving Show

parseFile :: FilePath -> IO [Ingress]
parseFile input = do
    x <- parse <$> readFile input
    _ <- evaluate (show x) -- hack to evaluate until we have a proper parser
    return x

parse :: String -> [Ingress]
parse = map parseLine . lines

parseLine :: String -> Ingress
parseLine l = case words l of
    ["install", i, f, a] -> Install (parseId i) f (parseArg a)
    ["ingress", i, m, a] -> Update (parseId i) m (parseArg a)
    ["query", i, m, a] -> Query (parseId i) m (parseArg a)
    _ -> error $ "Cannot parse: " ++ show l

parseId :: String -> Id
parseId x = case readMaybe x of
    Just (n::Word64) -> B.toLazyByteString $ B.word64LE n
    Nothing -> error "Invalid canister id (decimal number)"

parseArg :: String -> Payload
parseArg ('0':'x':xs)
    | Just x <- B.fromStrict <$> H.decodeHex (T.pack xs) = x
parseArg ('"':xs)
    = B.pack $ go xs
  where
    go "" = error "Missing terminating \""
    go "\"" = []
    go ('\\':'x':a:b:ys)
        | Just h <- H.decodeHex (T.pack [a,b])
        = B.unpack (B.fromStrict h) ++ go ys
    go (c:ys) = fromIntegral (ord c) : go ys
parseArg x = error $ "Invalid argument " ++ x


