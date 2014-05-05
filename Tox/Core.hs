{-# LANGUAGE ForeignFunctionInterface #-}
module Tox.Core where
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16

-- | Uninspectable data type for the Tox struct
data Tox

foreign import ccall "tox_new"
    c_tox_new :: CUInt -> IO (Ptr Tox)

-- | Call on startup, returns a pointer to a tox struct, if ipv6 is
-- True then tox runs in ipv6 mode.
toxNew :: Bool -> IO (Ptr Tox)
toxNew ipv6 = case (ipv6) of
                  True -> c_tox_new $ fromIntegral 1
                  False -> c_tox_new $ fromIntegral 0

foreign import ccall "tox_isconnected"
    c_tox_isconnected :: Ptr Tox -> IO (CInt)

toxIsconnected :: Ptr Tox -> IO (Bool)
toxIsconnected tox = do
        x <- c_tox_isconnected tox
        return $ (1==) $ fromIntegral x


foreign import ccall "tox_bootstrap_from_address"
    c_tox_bootstrap_from_address :: Ptr Tox -> Ptr CUChar -> CUChar -> CUShort -> Ptr CUChar -> IO ()

toxBootstrapFromAddress :: Ptr Tox -> String -> Bool -> Int -> String -> IO ()
toxBootstrapFromAddress tox addr ipv6 port key = do
        let bytes = (BSC.unpack $ fst $ BS16.decode $ BSC.pack key)
        putStrLn $ bytes
        putStrLn $ show $ length bytes
        k <- newArray $ map (CUChar . fromIntegral . fromEnum) bytes
        a <- newArray $ map (CUChar . fromIntegral . fromEnum) addr
        let p = CUShort . fromIntegral $ port
        let ipv = CUChar . fromIntegral $ case (ipv6) of
                      True -> 1
                      False -> 0
        c_tox_bootstrap_from_address tox a ipv p k
        free k
        free a

foreign import ccall "tox_get_address"
    c_tox_get_address :: Ptr Tox -> Ptr CUChar -> IO ()

toxGetAddress :: Ptr Tox -> IO (String)
toxGetAddress tox = do
        addrPtr <- (mallocArray 38 :: IO (Ptr CUChar))
        c_tox_get_address tox addrPtr
        addrArray <- peekArray 38 addrPtr
        free addrPtr
        return $ show $ BS16.encode $ BSC.pack $ (map (toEnum . fromIntegral) addrArray)

foreign import ccall "tox_do"
    c_tox_do :: Ptr Tox -> IO ()

toxDo :: Ptr Tox -> IO ()
toxDo tox = c_tox_do tox
