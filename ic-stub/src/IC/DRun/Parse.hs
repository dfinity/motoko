module IC.DRun.Parse where

import qualified Data.ByteString.Lazy as B
import qualified Text.Hex as H
import qualified Data.Text as T
import Data.Char
import Control.Exception

data Type = Query | Update deriving Show
type MethodName = String
type Payload = B.ByteString

type Ingress = (Type, String, Payload)

parseFile :: FilePath -> IO [Ingress]
parseFile input = do
    x <- parse <$> readFile input
    _ <- evaluate (show x) -- hack to evaluate until we have a proper parser
    return x

parse :: String -> [Ingress]
parse = map parseLine . lines

parseLine :: String -> Ingress
parseLine l = case words l of
    [t,m,a] -> (parseType t, m, parseArg a)
    _ -> error $ "Cannot parse: " ++ show l

parseType :: String -> Type
parseType "ingress" = Update
parseType "query" = Query
parseType x = error $ "Invalid ingress type " ++ x

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


