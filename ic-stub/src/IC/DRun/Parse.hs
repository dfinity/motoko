{-# LANGUAGE ScopedTypeVariables #-}
module IC.DRun.Parse where

import qualified Data.ByteString.Lazy as B
import qualified Text.Hex as H
import qualified Data.Text as T
import Data.Char
import Data.List
import Control.Exception

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

-- TODO: Implement proper and extract in own module
parseId :: String -> Id
parseId s
    | "ic:" `isPrefixOf` s
    , Just bs <- B.fromStrict <$> H.decodeHex (T.pack (drop 3 s))
    , B.length bs > 1
    = B.init bs
    | otherwise
    = error "Invalid canister id"

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


