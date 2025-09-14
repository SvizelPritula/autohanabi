# Gameplay logic

The gameplay implementations is split across two modules, `Game` and `Cards`.
`Cards` contains a lot of type definitions related to cards,
as well as some functions that only operate on them,
like `drawCard`, which draws a random card from the deck.
The `Game` module contains all other types and functions.

## The game state

The main type defined by `Game` is the `GameState` record,
which contains all information about an in-progress game.
Aside from the obvious data like cards held in hand,
cards played on the table and available tokens,
it also remembers what each player knows about the cards they're holding.

The deck is represented by the `Deck` type, which is just an alias for `CardVec Int`.
It stores the number of cards remaining in the deck for each card type. 
It doesn't have an order, a random card is chosen by `drawCard` whenever needed.

The hand of each player is stored as a list of `CardState`s.
A player usually has 4 cards, but they can get down to 3 at the very end of the game.
A `CardState` stores two values: `actual`, the `Card` that the card really is,
and `knowledge`, which stores what the player knows about the card.
The `Knowledge` type consists of a `ColorVec Bool` and a `NumberVec Bool`,
each storing which colors/numbers the card could have, as far as the player knows.
This is only affected by hints, not e.g. by the cards that have already been played.
The hands of both players are stored in the `hands` field of `GameState`,
which is a `PlayerVec [CardState]`.
`Player` can be either `Human` or `Computer`.

The `piles` field stores what cards have been successfully played.
Its type is `ColorVec (Maybe CardNumber)`.
The value for each color is the number of the last/highest card of that color that was played,
or `Nothing` if no card of the given color has been played.
There are two functions, `cardNumberToInt` and `pileToInt` (which returns `0` for `Nothing`),
that can simplify checking whether a given card can be played.

## The game logic

The gameplay logic is split between three main functions,
`genStartingState`, `play` and `hasGameEnded`.

As the name would imply, `genStartingState` is responsible
for generating the initial state of the game.
Since the initial hand of each player is random, it needs access to a random number generator.
It uses the [`State`](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-State-Strict.html#t:State) monad for this,
as supported by the [stateful random number generators](https://hackage-content.haskell.org/package/random-1.3.1/docs/System-Random-Stateful.html#g:monadicadapters) defined by the `random` package.
See the implementation of `main` and `runStateGenIO` in `Main` for an example of how to call it.

The `play` function is responsible for evaluating a turn, or `Action`.
`Action` is an enum that defines the three possible actions a player can take during a turn
(play a card, discard a card, give a hint).
It is one of the largest functions in the entire program and can evaluate each type of action,
with the help of some helper methods.
It also uses the `State` monad, but this time there are two of them:
The inner `State` monad stores the `GameState`
while the wrapping `StateT` monad stores the random number generator.
The `runTurn` function in `Main` shows how `play` can be called by pure code.

Lastly, the `hasGameEnded` function decides if to stop the game.
Unlike the last two functions, it's a fully pure function without any monads.
