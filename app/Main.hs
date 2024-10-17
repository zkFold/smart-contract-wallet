module Main where

import           Options.Applicative
import           Prelude              (IO, Int, (<>))

import           ZkFold.Wallet.Server (run)

data Params = Params
    { port :: Int }

paramsParser :: Parser Params
paramsParser =
    Params <$> option auto
          ( long "port"
         <> short 'p'
         <> help "Port to run the server"
         <> showDefault
         <> value 3000
         <> metavar "INT" )

opts :: ParserInfo Params
opts = info (paramsParser <**> helper)
  ( fullDesc
  <> progDesc "Run a Wallet server" )

main :: IO ()
main = do
    Params port <- execParser opts
    run port
