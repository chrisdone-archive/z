module Main where

import Z

main = getContents >>= run where
  run string = do
    result <- parseAndRun string
    case result of
      Left err -> error (show err)
      Right ranOK ->
        case ranOK of
          Left err -> error (show err)
          Right ok -> print ok
