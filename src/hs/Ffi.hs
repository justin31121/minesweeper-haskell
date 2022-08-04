{-# LANGUAGE ForeignFunctionInterface #-}

module Ffi where
import Foreign
import Data.Char
import Foreign.C.Types
getHiddenChar = fmap (chr.fromEnum) ffi_c_getch
ansi_enable = ffi_ansi_enable
ansi_disable = ffi_ansi_disable
foreign import ccall unsafe "conio.h getch" ffi_c_getch :: IO CInt
foreign import ccall "../c/ansi.h ansi_enable" ffi_ansi_enable :: IO ()
foreign import ccall "../c/ansi.h ansi_disable" ffi_ansi_disable :: IO ()
