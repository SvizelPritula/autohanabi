# The AI

The AI of Autohanabi uses a recursive, state-space searching algorithm.
It is contained within the `Ai` module.
It uses a couple of utility functions from the `Game` module,
but it reimplements most of the gameplay logic — it doesn't call `play` at all.
This is partially because the AI doesn't know the entire game state, nor can it predict random draws.
It is also an optimization, since the AI doesn't model the game with 100 % accuracy on purpose,
as that would be too slow (more on that bellow).
For example, it doesn't model removing cards from the deck at all.

## The game state, as seen by the AI

The `Ai` module uses a new `AiGameState` type instead of `GameState`.
Unlike `GameState`, `AiGameState` only contains information the computer actually knows.
As such, `AiGameState` can be better thought of as a set of possible states,
rather than a single state.
There are three main differences:

- Instead of a `Card`, a single slot in the hand of a player is represented by a `CardVec Int`.
  This stores all the cards that could be in this slot, as far as the player knows.
  The computer derives this for his own cards based on his knowledge
  and the cards that have been played/discarded so far.
  Cards in the hand of the human player are represented by a `CardVec` with only one element set to `1`.
- The `deck` field is replaced by a `remainingCards` field,
  since the player doesn't know what cards are still in the deck,
  only what cards haven't been played yet.
- The state is represented from the view of the player whose turn it is.
  As such, the `hands` field is replaced by two fields, `ownCards` and `coplayerCards`.
  (Yes, [coplayer](https://en.wiktionary.org/wiki/coplayer) is a word, the dictionary agrees.)
  The values of those two fields must be swapped before recursing.

The `stateToAiState` function is used to construct the initial `AiGameState`.

## The recursive algorithm

The algorithm can be best described as a set of three mutually recursive functions:

**Picking an action:**
1. List all possible actions.
2. Compute the score of each action.
3. Return the action with the best score.

**Scoring an action**:
1. List all ways the action could play out.
   (What card could be played, which cards the hint could match …)
2. For every outcome, score the state that would result from it.
3. Return the average score across all outcomes, weighted by how likely they are.

**Scoring a state**:
1. Update the state based on the knowledge of you coplayer
   and determine the action they would pick.
2. Score the action they would pick, based on the "real" state.

It is not too dissimilar to the minimax algorithm:
The *max* step corresponds to either player picking the best action,
while the *min* step is replaced by a *mean* step where fate picks the outcome of the action,
obtaining what could be called a *meanimax* algorithm.
One more difference from a textbook minimax algorithm is the fact that
instead of simply returning the maximum score during the max step,
we use it to pick an action, which we then re-score using more complete information.

## The implementation

The three functions described above map exactly to the three main functions of the `Ai` module,
`pickActionRec`, `scoreActionRec` and `scoreStateRec`.

We obviously cannot recurse infinitely deep, so the depth of recursion is capped
to 3 calls of `scoreStateRec` — that is, the computer will try to predict the reaction of the player
to his next two actions.
This is the smallest possible depth that makes sense,
as otherwise the computer wouldn't be able to consider how the player would act if it
used two hints to tell them all information about a specific card.
When the maximum depth is exhausted, `scoreStateRec` will call `scoreStateHeuristic`
to obtain an estimation of how advantageous this state is.

The last important implementation detail is bias — instead of leaving all scoring heuristics
to the `scoreStateHeuristic` function, some bonuses and penalties to the score
are applied directly withing the `scoreActionRec` function.
There is one bonus and two penalties applied this way:
the bonus for playing a card, the penalty for a misfire
and the penalty from making some cards permanently unplayable
by discarding the last of a given card type.
This allows us not to store exactly which card was played/discarded within `AiGameState`.
This means all discards and all misfires will result in the exact same `AiGameState`,
which cuts the number of recursion branches down significantly.
