module Main (main) where

import Api.Server (runServer)

main :: IO ()
main = runServer 8080
