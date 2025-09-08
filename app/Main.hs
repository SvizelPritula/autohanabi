module Main where

import Ansi (clearScreen, makeBlue, makeBold, makeGray, makeRed, makeUnderline)
import Cards (Card, CardColor (Blue, Green, Red, White, Yellow), CardNumber, cardColor, cardNumber, colored)
import Control.Monad (forM_)
import Control.Monad.State.Strict (State)
import Data.Char (chr, ord)
import Data.Foldable (find)
import Game (Action (Discard, Hint, Play), CardState (actual), GameState (fuseTokens, hands, informationTokens, piles), Hint (ColorHint, NumberHint), Player (Computer, Human), genStartingState, maxFuseTokens, maxInformationTokens, play)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin)
import System.Random (StdGen, initStdGen)
import System.Random.Stateful (StateGenM, runStateGen_)
import Vec (Vec (toListWithKey), (!))

runStateGenIO :: (StateGenM StdGen -> State StdGen a) -> IO a
runStateGenIO f = do
  rng <- initStdGen
  return (runStateGen_ rng f)

printGameState :: GameState -> IO ()
printGameState state = do
  let putElement = putStr . (' ' :)

  let printLine title contents = do
        putStr title
        forM_ contents putElement
        putChar '\n'

  let token color True = color "*"
      token _ False = makeGray "*"

  let pile color (Just card) = colored color (show card)
      pile color Nothing = colored color "0"

  putStr clearScreen

  printLine "Played:  " (map (uncurry pile) (toListWithKey (piles state)))

  printLine "Computer:" (map (show . actual) (hands state ! Computer))
  printLine "You:     " (map (const (makeGray "?")) (hands state ! Human))

  printLine "Infos:   " [token makeBlue (i <= informationTokens state) | i <- [1 .. maxInformationTokens]]
  printLine "Fuses:   " [token makeRed (i <= fuseTokens state) | i <- [1 .. maxFuseTokens]]

data ActionResult a = Selected a | Retry deriving (Functor)

promptRetry :: GameState -> IO () -> (Char -> IO (ActionResult a)) -> IO a
promptRetry state printPrompt handleAnswer = do
  printGameState state
  putChar '\n'
  printPrompt
  key <- getChar
  answer <- handleAnswer key
  case answer of
    Selected value -> return value
    Retry -> promptRetry state printPrompt handleAnswer

data Option a = Option {key :: Char, label :: String, action :: IO (ActionResult a), visible :: Bool}

prompt :: GameState -> String -> [Option a] -> IO a
prompt state heading allOptions =
  let options = filter visible allOptions
      printOption option = do
        putStr (makeBold [key option])
        putStr ": "
        putStrLn (label option)
      printPrompt = do
        putStrLn (makeUnderline heading)
        forM_ options printOption
      handleAnswer k = case find (\o -> key o == k) options of
        Just option -> action option
        Nothing -> return Retry
   in promptRetry state printPrompt handleAnswer

promptTurn :: GameState -> IO (Maybe Action)
promptTurn state =
  let handle wrapper = fmap (fmap (Just . wrapper))
   in prompt
        state
        "What do you want to do?"
        [ Option {key = 'p', label = "Play a card", action = handle Play (promptCard state "play"), visible = True},
          Option {key = 'd', label = "Discard a card", action = handle Discard (promptCard state "discard"), visible = True},
          Option {key = 'h', label = "Give a hint", action = handle Hint (promptHint state), visible = informationTokens state > 0},
          Option {key = 'Q', label = "Quit", action = return (Selected Nothing), visible = True}
        ]

backOption :: Option (ActionResult a)
backOption = Option {key = 'q', label = "Back", action = return (Selected Retry), visible = True}

numberToOrdinal :: Int -> String
numberToOrdinal 1 = "First"
numberToOrdinal 2 = "Second"
numberToOrdinal 3 = "Third"
numberToOrdinal 4 = "Fourth"
numberToOrdinal n = (show n) ++ "th"

promptCard :: GameState -> String -> IO (ActionResult Int)
promptCard state action =
  let cardOptions =
        [ Option
            { key = chr (ord '0' + i),
              label = numberToOrdinal i ++ " card",
              action = return (Selected (Selected (i - 1))),
              visible = True
            }
        | i <- [1 .. length (hands state ! Human)]
        ]
   in prompt
        state
        ("Which card do you want to " ++ action ++ "?")
        (cardOptions ++ [backOption])

colorKey :: CardColor -> Char
colorKey Red = 'r'
colorKey Yellow = 'y'
colorKey Green = 'g'
colorKey Blue = 'b'
colorKey White = 'w'

numberKey :: CardNumber -> Char
numberKey number = chr (ord '1' + fromEnum number)

promptHint :: GameState -> IO (ActionResult Hint)
promptHint state =
  let handle wrapper = fmap (fmap (Selected . wrapper))
   in prompt
        state
        "Which attribute do you want to hint?"
        [ Option
            { key = 'c',
              label = "Color",
              action = handle ColorHint (promptHintAttribute state "color" colorKey cardColor),
              visible = True
            },
          Option
            { key = 'n',
              label = "Number",
              action = handle NumberHint (promptHintAttribute state "number" numberKey cardNumber),
              visible = True
            },
          backOption
        ]

promptHintAttribute :: (Show a, Bounded a, Enum a, Eq a) => GameState -> String -> (a -> Char) -> (Card -> a) -> IO (ActionResult a)
promptHintAttribute state name key get =
  let options =
        [ Option
            { key = key attribute,
              label = show attribute,
              action = return (Selected (Selected attribute)),
              visible = any (\c -> get (actual c) == attribute) (hands state ! Computer)
            }
        | attribute <- [minBound .. maxBound]
        ]
   in prompt
        state
        ("Which " ++ name ++ " do you want to hint?")
        (options ++ [backOption])

runGame :: GameState -> IO ()
runGame state = do
  maybeAction <- promptTurn state
  case maybeAction of
    Just action -> do
      newState <- runStateGenIO (play state Human action)
      runGame newState
    Nothing -> return ()

main :: IO ()
main = do
  state <- runStateGenIO genStartingState
  hSetBuffering stdin NoBuffering

  runGame state
