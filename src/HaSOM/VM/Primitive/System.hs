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
import System.IO (stderr)
import Data.Time.Clock.System (getSystemTime, SystemTime (systemSeconds, systemNanoseconds))
import HaSOM.VM.Disassembler (stackTrace)
import Control.Monad ((>=>))

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "System",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("global:", global),
    ("global:put:", globalPut),
    ("hasGlobal:", hasGlobal),
    ("loadFile:", loadFile),
    ("load:", load),
    ("exit:", exit),
    ("printString:", printStrM),
    ("printNewline", printNewlineM),
    ("errorPrintln:", errorPrintln),
    ("errorPrint:", errorPrint),
    ("printStackTrace", printStackTrace),
    ("time", systemTime),
    ("ticks", systemTicks),
    ("fullGC", fullGC)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

global :: NativeFun
global = pureNativeFun @N1 $ \_ (g :+: Nil) -> do
  symbol <- castSymbol g
  idx <- internGlobalE symbol

  get @VMGlobalsNat >>= getGlobal idx .> \case
    Nothing -> getNil
    Just (ObjectGlobal oi) -> pure oi
    Just (ClassGlobal MkVMClass{asObject}) -> pure asObject

globalPut :: NativeFun
globalPut = pureNativeFun @N2 $ \self (g :+: val :+: Nil) -> do
  symbol <- castSymbol g
  idx <- internGlobalE symbol

  setGlobalE idx (ObjectGlobal val)
  pure self

hasGlobal :: NativeFun
hasGlobal = pureNativeFun @N1 $ \_ (s :+: Nil) -> do
  symbol <- castSymbol s
  idx <- internGlobalE symbol

  bool <- get @VMGlobalsNat >>= getGlobal idx .> \case
    Just _ -> newTrue
    Nothing -> newFalse

  addToGC bool

load :: NativeFun
load = pureNativeFun @N1 $ \_ (s :+: Nil) -> do
  symbol <- castSymbol s
  idx <- internGlobalE symbol

  nilIx <- getNil

  get @VMGlobalsNat <&> getGlobal idx .> \case
    Just (ClassGlobal MkVMClass{asObject}) -> asObject
    Just (ObjectGlobal oi) -> oi
    Nothing -> nilIx

loadFile :: NativeFun
loadFile = pureNativeFun @N1 $ \_ (fp :+: Nil) -> do
  str <- castString fp
  cont <- lift $ TIO.readFile (T.unpack str)

  newString cont >>= addToGC

exit :: NativeFun
exit = mkNativeFun $ do
  i <- getLocal 0 1 >>= castInt
  pure (Just i)

printIOStr :: (Text -> IO ()) -> NativeFun
printIOStr printingF = pureNativeFun @N1 $ \self (strIx :+: Nil) -> do
  str <- castStringOrSymbol strIx
  lift $ printingF str
  pure self

printIO :: IO () -> NativeFun
printIO printingF = pureNativeFun @N0 $ \self Nil -> do
  lift printingF
  pure self

printStrM :: NativeFun
printStrM = printIOStr TIO.putStr

printNewlineM :: NativeFun
printNewlineM = printIO (putStrLn "")

errorPrintln :: NativeFun
errorPrintln = printIOStr (TIO.hPutStrLn stderr)

errorPrint :: NativeFun
errorPrint = printIOStr (TIO.hPutStr stderr)

printStackTrace :: NativeFun
printStackTrace = mkNativeFun $ do
  getSelf >>= pushStack
  _ <- popCallFrame
  get >>= (stackTrace >=> lift . TIO.putStrLn)
  pure Nothing

systemTime :: NativeFun
systemTime = pureNativeFun @N0 $ \_ Nil -> do
  initial <- runRuntimeStartTime <$> ask
  curr <- lift getSystemTime

  let res = fromIntegral (systemSeconds curr - systemSeconds initial)

  newInt res >>= addToGC

systemTicks :: NativeFun
systemTicks = pureNativeFun @N0 $ \_ Nil -> do
  initial <- runRuntimeStartTime <$> ask
  curr <- lift getSystemTime

  let res = fromIntegral ((systemNanoseconds curr - systemNanoseconds initial) `div` 1000)

  newInt res >>= addToGC

fullGC :: NativeFun
fullGC = pureNativeFun @N0 $ \_ Nil -> do
  put RunGC
  newTrue >>= addToGC
