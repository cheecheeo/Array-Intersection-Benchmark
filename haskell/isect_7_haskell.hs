{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import qualified System.Environment as Env
import qualified Data.Time.Clock as T
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.IO as H
import qualified Control.Monad as M
import qualified Data.Maybe as Maybe

import qualified Common as C

-- requires hashtables package:
-- $ cabal install hashtables

isect :: [ByteString] -> [ByteString] -> IO [ByteString]
isect xs ys = do
  (xsHash :: H.CuckooHashTable ByteString ()) <- H.fromList (zip xs (repeat ()))
  C.revFilterM (\y -> Maybe.isJust <$> H.lookup xsHash y) ys

main :: IO ()
main =
  do [filename] <- Env.getArgs
     (count : sets) <- BS.lines <$> BS.readFile filename
     let (!xs, !ys) = unzip (map (\s -> let [x, y] = BS.words s
                                        in (x, y)) sets)
     t0 <- T.getCurrentTime
     !intersected <- isect xs ys
     t1 <- T.getCurrentTime
     putStrLn ("Set   | n = " ++ show (length xs) ++ " : " ++ show (length intersected) ++ " intersects found in " ++ show (T.diffUTCTime t1 t0) ++ " seconds")
