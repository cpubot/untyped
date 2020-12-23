module Repl.LogUtil where

import System.Console.Pretty
    ( Pretty(color, style),
      Color(..),
      Style(Bold) )
import System.Console.Haskeline (InputT, outputStrLn)
import Control.Monad.IO.Class (MonadIO)

styleResult :: Pretty a => a -> a
styleResult = color Yellow . style Bold

stylePrompt :: Pretty a => a -> a
stylePrompt = color White . style Bold

styleError :: Pretty a => a -> a
styleError = color Red

styleInfo :: Pretty a => a -> a
styleInfo = color Cyan

er :: MonadIO m => String -> InputT m ()
er = outputStrLn . styleError
res :: MonadIO m => String -> InputT m ()
res = outputStrLn . styleResult
info :: MonadIO m => String -> InputT m ()
info = outputStrLn . styleInfo
