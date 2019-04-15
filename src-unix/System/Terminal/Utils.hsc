{-# LANGUAGE ForeignFunctionInterface #-}

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

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Alloc ( alloca )

#include <sys/ioctl.h>
#include <unistd.h>

-- | Try to get the number of rows and columns respectively in the terminal
getTerminalSize :: IO (Maybe (Int,Int))
getTerminalSize = alloca $ \ws -> do
  res <- ioctl (#const STDOUT_FILENO) (#const TIOCGWINSZ) ws
  if res == -1
    then pure Nothing
    else do
      WinSize row col <- peek ws
      pure (Just (fromIntegral row, fromIntegral col))

-- | @ioctl@ fills the struct at the pointer you passed in with the size info
foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr WinSize -> IO CInt

-- | Match @struct winsize@ in @sys/ioctl.h@.
data WinSize = WinSize CUShort CUShort

instance Storable WinSize where
  sizeOf _ = (#size struct winsize)
  alignment _ = (#alignment struct winsize) 
  peek ptr = do
    row <- (#peek struct winsize, ws_row) ptr
    col <- (#peek struct winsize, ws_col) ptr
    pure (WinSize row col)
  poke ptr (WinSize row col) = do
    (#poke struct winsize, ws_row) ptr row
    (#poke struct winsize, ws_col) ptr col
