{-|

This module provides a thin wrapper around the winter Wasm engine, exposing just
the bits needed by the IC stub.

This is the interface at which one might plug in a different Wasm engine.
-}
module IC.Wasm.Winter
  ( Module
  , parseModule
  , exportedFunctions
  , Import
  , Imports
  , HostM
  , HostFunc
  , W.Value(..)
  , W.StackType
  , W.ValueType(..)
  , W.Address
  , W.Size
  , getBytes
  , setBytes
  , initialize
  , Instance
  , invokeExport
  , invokeTable
  )
where

import qualified Data.ByteString.Lazy as BS
import Control.Monad.Identity
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Text.Lazy as T
import Control.Monad.ST
import Data.Binary.Get (runGetOrFail)
import Data.Default.Class (Default (..))
import Data.Int

import qualified Wasm.Binary.Decode as W
import qualified Wasm.Exec.Eval as W
import qualified Wasm.Runtime.Func as W
import qualified Wasm.Runtime.Instance as W
import qualified Wasm.Runtime.Memory as W
import qualified Wasm.Syntax.AST as W
import qualified Wasm.Syntax.Types as W
import qualified Wasm.Syntax.Values as W
import qualified Wasm.Syntax.Memory as W

type Instance s = (IM.IntMap (W.ModuleInst Identity (ST s)), Int)

type HostM s = ExceptT String (ST s)

type HostFunc s = HostM s [W.Value]

type Import s = (String, W.StackType, W.StackType, [W.Value] -> HostFunc s)

type Imports s = [(String, [Import s])]

type Module = W.Module Identity

parseModule :: BS.ByteString -> Either String Module
parseModule bytes = case runGetOrFail W.getModule bytes of
  Left  (_,_,err) -> Left err
  Right (_,_,wasm_mod) -> Right wasm_mod


initialize :: Module -> Imports s -> HostM s (Instance s)
initialize mod imps = withExceptT show $ do
  let names = M.fromList (zip (map (T.pack . fst) imps) [1..])
      mods  = IM.fromList $ zip [1..]
        [ (W.emptyModuleInst def)
          { W._miGlobals  = [ ]
          , W._miTables   = [ ]
          , W._miMemories = [ ]
          , W._miFuncs    = [ ]
          , W._miExports  = M.fromList
            [ (,) (T.pack fname) $ W.ExternFunc $
              W.allocHostEff (W.FuncType arg_ty ret_ty)
                  (\ args -> runExceptT $ f args)
            | (fname, arg_ty, ret_ty, f) <- funcs
            ]
          }
        | (_name, funcs) <- imps
        ]
  (ref, inst) <- W.initialize (Identity mod) names mods
  let mods' = IM.insert ref inst mods
  return (mods', ref)


exportedFunctions :: Module -> [String]
exportedFunctions wasm_mod =
  [ T.unpack (W._exportName e)
  | Identity e <- W._moduleExports wasm_mod
  , W.FuncExport {} <- return $ W._exportDesc e
  ]


invokeExport :: Instance s -> String -> [W.Value] -> HostM s [W.Value]
invokeExport (mods', ref) method args = do
  let inst = mods' IM.! ref
  withExceptT show $
    W.invokeByName mods' inst (T.pack method) args

invokeTable :: Instance s -> Int32 -> [W.Value] -> HostM s [W.Value]
invokeTable (mods', ref) idx args = do
  let inst = mods' IM.! ref
  withExceptT show $ do
    func <- W.elem inst 0 idx def
    W.invoke mods' inst func args

getBytes :: Instance s -> W.Address -> W.Size -> HostM s BS.ByteString
getBytes (mods', ref) ptr len = do
  let inst = mods' IM.! ref
  let mem = head (W._miMemories inst)
  withExceptT show $ W.loadBytes mem ptr len

setBytes :: Instance s -> W.Address -> BS.ByteString -> HostM s ()
setBytes (mods', ref) ptr blob = do
  let inst = mods' IM.! ref
  let mem = head (W._miMemories inst)
  withExceptT show $ W.storeBytes mem (fromIntegral ptr) blob

