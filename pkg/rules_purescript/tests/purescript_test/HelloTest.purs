module HelloTest
  ( main
  ) where

import IO as IO

main :: IO.IO {}
main = IO.do
  argv <- IO.argv
  case argv of
    [ helloPath ] -> IO.do
      result <- IO.exec helloPath
      case trim result of
        "Hello, world!" -> IO.exit 0
        _ -> IO.do
          IO.println "Got incorrect value:"
          IO.println (trim result)
          IO.exit 1
    _ -> IO.do
      IO.println "Recieved too many arguments:"
      IO.forEach argv \arg -> IO.do
        IO.println arg
      IO.exit 1

foreign import trim ::
  String ->
  String
