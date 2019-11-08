module IC.DRun.Parse where

import qualified Data.ByteString.Lazy as B
import qualified Text.Hex as H
import qualified Data.Text as T
import Data.Char
import Control.Exception

data Type = Query | Update deriving Show
type MethodName = String

type Ingress = (Type, String, B.ByteString)

parseFile :: FilePath -> IO [Ingress]
parseFile input = do
    x <- parse <$> readFile input
    evaluate (show x) -- hack to evaluate until we have a proper parser
    return x

parse = map parseLine . lines

parseLine l = case words l of
    [t,m,a] -> (parseType t, m, parseArg a)

parseType "ingress" = Update
parseType "query" = Query
parseType x = error $ "Invalid ingress type " ++ x

parseArg ('0':'x':xs)
    | Just x <- B.fromStrict <$> H.decodeHex (T.pack xs) = x
parseArg ('"':xs)
    = B.pack $ go xs
  where
    go "" = error "Missing terminating \""
    go "\"" = []
    go ('\\':'x':a:b:xs)
        | Just h <- H.decodeHex (T.pack [a,b])
        = B.unpack (B.fromStrict h) ++ go xs
    go (c:xs) = fromIntegral (ord c) : go xs
parseArg x = error $ "Invalid argument " ++ x


