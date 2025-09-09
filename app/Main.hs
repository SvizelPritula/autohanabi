module Main where

import Ai (pickAction)
import Ansi (clearScreen, makeBlue, makeBold, makeGray, makeRed, makeUnderline)
import Cards (CardColor (Blue, Green, Red, White, Yellow), CardNumber, cardColor, cardNumber, colorName, colored, longCardName)
import Control.Monad (forM_)
import Control.Monad.State.Strict (State)
import Data.Char (chr, ord, toUpper)
import Data.Foldable (find)
import Game (Action (Discard, Hint, Play), ActionResult (Discarded, Hinted, Played), CardState (actual), GameState (fuseTokens, hands, informationTokens, piles), Hint (ColorHint, NumberHint), Player (Computer, Human), enumerate, genStartingState, maxFuseTokens, maxInformationTokens, play)
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

data OptionResult a = Selected a | Retry deriving (Functor)

promptRetry :: GameState -> IO () -> (Char -> IO (OptionResult a)) -> IO a
promptRetry state printPrompt handleAnswer = do
  printGameState state
  putChar '\n'
  printPrompt
  key <- getChar
  answer <- handleAnswer key
  case answer of
    Selected value -> return value
    Retry -> promptRetry state printPrompt handleAnswer

data Option a = Option {key :: Char, label :: String, action :: IO (OptionResult a), visible :: Bool}

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

capitalize :: String -> String
capitalize "" = ""
capitalize (first : rest) = toUpper first : rest

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

backOption :: Option (OptionResult a)
backOption = Option {key = 'q', label = "Back", action = return (Selected Retry), visible = True}

numberToOrdinal :: Int -> String
numberToOrdinal 1 = "first"
numberToOrdinal 2 = "second"
numberToOrdinal 3 = "third"
numberToOrdinal 4 = "fourth"
numberToOrdinal n = (show n) ++ "th"

promptCard :: GameState -> String -> IO (OptionResult Int)
promptCard state action =
  let cardOptions =
        [ Option
            { key = chr (ord '0' + i),
              label = capitalize $ numberToOrdinal i ++ " card",
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

promptHint :: GameState -> IO (OptionResult Hint)
promptHint state =
  let handle wrapper = fmap (fmap (Selected . wrapper))
   in prompt
        state
        "Which attribute do you want to hint?"
        [ Option
            { key = 'c',
              label = "Color",
              action = handle ColorHint (promptHintColor state),
              visible = True
            },
          Option
            { key = 'n',
              label = "Number",
              action = handle NumberHint (promptHintNumber state),
              visible = True
            },
          backOption
        ]

promptHintColor :: GameState -> IO (OptionResult CardColor)
promptHintColor state =
  let options =
        [ Option
            { key = colorKey color,
              label = colored color $ capitalize $ colorName color,
              action = return (Selected (Selected color)),
              visible = any (\c -> cardColor (actual c) == color) (hands state ! Computer)
            }
        | color <- [minBound .. maxBound]
        ]
   in prompt
        state
        ("Which color do you want to hint?")
        (options ++ [backOption])

promptHintNumber :: GameState -> IO (OptionResult CardNumber)
promptHintNumber state =
  let options =
        [ Option
            { key = numberKey number,
              label = show number,
              action = return (Selected (Selected number)),
              visible = any (\c -> cardNumber (actual c) == number) (hands state ! Computer)
            }
        | number <- [minBound .. maxBound]
        ]
   in prompt
        state
        ("Which name do you want to hint?")
        (options ++ [backOption])

showMessage :: GameState -> IO () -> IO ()
showMessage state message = do
  printGameState state
  putChar '\n'

  message

  putChar '\n'
  putStrLn "Press any key to continue."

  _ <- getChar
  return ()

printAction :: Player -> ActionResult -> IO ()
printAction player action = do
  case player of
    Computer -> putStr "The computer "
    Human -> putStr "You "

  case action of
    Played card success -> do
      putStr ("played a " ++ (longCardName card))
      if success
        then putStrLn "."
        else putStrLn ", which was a misfire."
    Discarded card -> putStrLn ("discarded a " ++ (longCardName card) ++ ".")
    Hinted hint cards -> do
      case player of
        Computer -> putStr "told you your "
        Human -> putStr "told the computer his "

      let len = length cards
          listElementPrefix i
            | i == 0 = ""
            | i == (len - 1) = " and "
            | otherwise = ", "
      forM_ (enumerate cards) (\(i, c) -> putStr (listElementPrefix i ++ numberToOrdinal (c + 1)))

      putStr (if len == 1 then " card is " else " cards are ")

      case hint of
        ColorHint color -> putStr $ show color
        NumberHint number -> putStr $ "a " ++ show number

      putStrLn "."

showAction :: GameState -> Player -> ActionResult -> IO ()
showAction state player action = showMessage state (printAction player action)

runGame :: GameState -> IO ()
runGame state = do
  maybeAction <- promptTurn state
  case maybeAction of
    Just action -> do
      (state2, result1) <- runStateGenIO (play state Human action)
      showAction state2 Human result1

      let computerAction = pickAction state2
      (state3, result2) <- runStateGenIO (play state2 Computer computerAction)
      showAction state3 Computer result2

      runGame state3
    Nothing -> return ()

main :: IO ()
main = do
  state <- runStateGenIO genStartingState
  hSetBuffering stdin NoBuffering

  runGame state
