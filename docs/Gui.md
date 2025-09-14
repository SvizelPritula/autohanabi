# The UI and game loop

All of the logic related to the game's UI is located in the `Main` module,
except for some formatting helpers, which are located in the `Ansi` module.
The `Ansi` module is also used by the `Show` implementations in the `Cards` module,
so some types (`Card` and `CardColor`) will be colored when printed by default.

The top of the `Main.hs` file contains all of the functions that handle the UI,
like printing the game state, presenting and handling menus, etc.
The `printGameState` function also clears the screen,
since it's called by every function that repaints the screen.
The menu system is handled by the `prompt` function,
which is given a title and a list of options.
Each option contains a title, a key and an *action*,
a function that is called when the options is selected.
The action can display a submenu and return either `Selected a`,
in which case `a` is returned from `prompt`, or `Retry`,
in which case `prompt` will display the same menu again.

The game loop is implemented at the very bottom of `Main.hs`.
The `runGame` function is recursive and will call `runTurn` over and over again
until it returns `Nothing`.
`runGame` will alternate calling `runTurn` with `player` set to `Human` and `Computer`.
Based on the player, `runTurn` will either display a menu to the player
or ask the AI to pick a move, and then it will show the result.
As an optimization, `runTurn` doesn't actually show the result (unless the game has ended),
it will instead return an `IO ()` monad that will do so.
This monad is passed to the next call of `runTurn`.
This allows the next iteration of `runTurn` to start the AI in the background
while the player is still reading the result of his action.
