{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Terminal.Utils
-- Copyright   :  Alec Theriault 2019
-- License     :  BSD3
--
-- Maintainer  :  alec.theriault@gmail.com
-- Portability :  portable
--
-- Terminal-related utilities
module System.Terminal.Utils (
  getTerminalSize,
) where

import Control.Exception
import System.Win32.Console

-- | Try to get the number of rows and columns respectively in the terminal
getTerminalSize :: IO (Maybe (Int,Int))
getTerminalSize = fmap Just rowsCols `catch` \(_ :: IOException) -> pure Nothing
  where
    rowsCols = do
      smallRect <- srWindow <$> getCurrentConsoleScreenBufferInfo
      pure ( fromIntegral (bottom smallRect - top smallRect + 1)
           , fromIntegral (right smallRect - left smallRect + 1)
           )
