# Autohanabi

Autohanabi allows you to play a game of [Hanabi](https://en.wikipedia.org/wiki/Hanabi_(card_game))
against your computer.
It is implemented as a colorful CLI app for your terminal.

## Installation

You can install Autohanabi using [Cabal](https://www.haskell.org/cabal/).
You will first need to download a local copy of this repository
and run the following command in its root:

```sh
cabal install
```

You can also run the game without installing it with the following command:

```sh
cabal run
```

## How to play

If you don't know the [rules of Hanabi](https://en.wikipedia.org/wiki/Hanabi_(card_game)#Gameplay),
you should read them first.

### The game state

The UI looks like this: (Note: color may not work in some Markdown engines.)

<pre>
Played:   <span style="color:#cd0000;">0</span> <span style="color:#cdcd00;">0</span> <span style="color:#00cd00;">0</span> <span style="color:#0000ee;">0</span> <span style="color:#e5e5e5;">0</span>
Computer: <span style="color:#00cd00;">1</span> <span style="color:#cdcd00;">4</span> <span style="color:#cdcd00;">3</span> <span style="color:#cd0000;">2</span>
You:      ? ? ? ?
Infos:    <span style="color:#0000ee;">*</span> <span style="color:#0000ee;">*</span> <span style="color:#0000ee;">*</span> <span style="color:#0000ee;">*</span> <span style="color:#0000ee;">*</span> <span style="color:#0000ee;">*</span> <span style="color:#0000ee;">*</span> <span style="color:#0000ee;">*</span>
Fuses:    <span style="color:#cd0000;">*</span> <span style="color:#cd0000;">*</span> <span style="color:#cd0000;">*</span>
Deck:     42 cards

<span style="text-decoration:underline;">What do you want to do?</span>
<span style="font-weight:bold;">p</span>: Play a card
<span style="font-weight:bold;">d</span>: Discard a card
<span style="font-weight:bold;">h</span>: Give a hint
<span style="font-weight:bold;">Q</span>: Quit
</pre>

The first line shows what cards have been played so far.
For each color, the highest successfully played card is shown,
or `0` if no cards of that color have been played yet.
Equivalently, each digit represents the number of cards of that color that have been played so far.

The second line shows the cards hend in the hand by the computer.
Each card is represented by a digit in the correct color.
The third line shows a `?` for each card in your hand and can mostly be ignored.

The fourth and fifth lines show how many tokens of each type are available.
Every blue `*` on the fourth line represents an information token ready to be used.
Every red `*` on the fifth line represents a remaining fuse token.
Used tokens are represented by a gray `*`.

Finally, the sixth line shows the number of cards remaining in the draw pile.

### Picking a move

You can select a move by navigating the menu bellow the game state.
Each option has an assigned letter or number which you need to type to select it.
You first need to choose the type of your move, either playing a card, discarding a card,
or giving a hint to the computer.

If you choose to play or discard a card, you will then need to pick the card to play/discard.
Cards are numbered from left to right.
A new card will be drawn at the end of your turn, which will be added on the right side of your hand.
Any cards to the right of the played/discarded card will shift one position to the left.

If you choose to give a hint, you will first need to select the type of attribute
(either the color or the number) and then the color/number.
You cannot give a hint that doesn't match any card,
nor can you give a hint if you're out of information tokens.

### Viewing the result

After you pick a move, you will be shown a message telling you how it went,
e.g. the color and number of the card you played or discarded.

After you press any key, the computer will make its turn,
and the result of that turn will also be shown,
e.g. what hint he gave you and which of your cards match it.

If the game ends, the amount of points you got will be displayed.
The program will then exit.
