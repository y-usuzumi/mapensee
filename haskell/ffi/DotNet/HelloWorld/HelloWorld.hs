{-# LANGUAGE ForeignFunctionInterface #-}

module HelloWorld where

import Foreign.C.Types
import Foreign.C.String

foreign export ccall
  hello :: CString -> IO CInt

hello :: CString -> IO CInt
hello c_str = do
  str <- peekCString c_str
  result <- hsHello str
  return $ fromIntegral result

hsHello :: String -> IO Int
hsHello str = do
  putStrLn $ "Hello, " ++ str
  return (length str)
