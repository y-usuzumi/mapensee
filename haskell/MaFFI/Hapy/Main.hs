{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Util
import Text.Printf

foreign import ccall "PyPy.h rpython_startup_code"
  rpython_startup_code :: IO ()

foreign import ccall "PyPy.h pypy_setup_home"
  pypy_setup_home :: CString -> CInt -> IO CInt

foreign import ccall "PyPy.h pypy_execute_source"
  pypy_execute_source :: CString -> IO CInt

source :: String
source = [str|
import os

print os.listdir('/home/kj')
|]

main = do
  rpython_startup_code
  home <- newCString "/usr/lib/pypy"
  _ <- pypy_setup_home home (CInt 1)
  source <- newCString source
  (CInt ret) <- pypy_execute_source source
  printf "Result: %d\n" ret
