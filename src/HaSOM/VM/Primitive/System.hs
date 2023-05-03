{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module HaSOM.VM.Primitive.System (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import HaSOM.VM.Object
import Combinator ((.>))
import Data.Functor ((<&>))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Exit (exitWith)
import GHC.IO.Exception (ExitCode(..))
import System.IO (stderr)

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "System",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("global:", mkNativeFun global),
    ("global:put:", mkNativeFun globalPut),
    ("hasGlobal:", mkNativeFun hasGlobal),
    ("loadFile:", mkNativeFun loadFile),
    ("load:", mkNativeFun load),
    ("exit:", mkNativeFun exit),
    ("printString:", mkNativeFun printStrM),
    ("printNewline", mkNativeFun printNewlineM),
    ("errorPrintln:", mkNativeFun errorPrintln),
    ("errorPrint:", mkNativeFun errorPrint),
    ("printStackTrace", mkNativeFun nativeNotImplemented), -- TODO
    ("time", mkNativeFun nativeNotImplemented), -- TODO
    ("ticks", mkNativeFun nativeNotImplemented), -- TODO
    ("fullGC", mkNativeFun nativeNotImplemented) -- TODO
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

global :: (UniverseEff r, Lifted IO r) => Eff r ()
global = pureNativeFun @N1 $ \_ (g :+: Nil) -> do
  symbol <- castSymbol g
  idx <- internGlobalE symbol

  get @VMGlobalsNat >>= getGlobal idx .> \case
    Nothing -> getNil
    Just (ObjectGlobal oi) -> pure oi
    Just (ClassGlobal MkVMClass{asObject}) -> pure asObject

globalPut :: (UniverseEff r, Lifted IO r) => Eff r ()
globalPut = pureNativeFun @N2 $ \self (g :+: val :+: Nil) -> do
  symbol <- castSymbol g
  idx <- internGlobalE symbol

  setGlobalE idx (ObjectGlobal val)
  pure self

hasGlobal :: (UniverseEff r, Lifted IO r) => Eff r ()
hasGlobal = pureNativeFun @N1 $ \_ (s :+: Nil) -> do
  symbol <- castSymbol s
  idx <- internGlobalE symbol

  bool <- get @VMGlobalsNat >>= getGlobal idx .> \case
    Just _ -> newTrue
    Nothing -> newFalse

  addToGC bool

load :: (UniverseEff r, Lifted IO r) => Eff r ()
load = pureNativeFun @N1 $ \_ (s :+: Nil) -> do
  symbol <- castSymbol s
  idx <- internGlobalE symbol

  nilIx <- getNil

  get @VMGlobalsNat <&> getGlobal idx .> \case
    Just (ClassGlobal MkVMClass{asObject}) -> asObject
    Just (ObjectGlobal oi) -> oi
    Nothing -> nilIx

loadFile :: (Lifted IO r, UniverseEff r) => Eff r ()
loadFile = pureNativeFun @N1 $ \_ (fp :+: Nil) -> do
  str <- castString fp
  cont <- lift $ TIO.readFile (T.unpack str)

  newString cont >>= addToGC

exit :: (Lifted IO r, UniverseEff r) => Eff r ()
exit = pureNativeFun @N1 $ \_ (iIx :+: Nil) -> do
  i <- castInt iIx
  lift $ exitWith (ExitFailure i)

printIOStr :: (Lifted IO r, UniverseEff r) => (Text -> IO ()) -> Eff r ()
printIOStr printingF = pureNativeFun @N1 $ \self (strIx :+: Nil) -> do
  str <- castString strIx
  lift $ printingF str
  pure self

printIO :: (Lifted IO r, UniverseEff r) => IO () -> Eff r ()
printIO printingF = pureNativeFun @N0 $ \self Nil -> do
  lift printingF
  pure self

printStrM :: (Lifted IO r, UniverseEff r) => Eff r ()
printStrM = printIOStr TIO.putStr

printNewlineM :: (Lifted IO r, UniverseEff r) => Eff r ()
printNewlineM = printIO (putStrLn "")

errorPrintln :: (Lifted IO r, UniverseEff r) => Eff r ()
errorPrintln = printIOStr (TIO.hPutStrLn stderr)

errorPrint :: (Lifted IO r, UniverseEff r) => Eff r ()
errorPrint = printIOStr (TIO.hPutStr stderr)
