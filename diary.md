# Day 41 (We, 2025-09-17)

I'd like to tackle yesterday's issue by re-implementing `buildTreeAB`, for which I have to replace the list comprehension by a recursive definition. The problem with list comprehensions is that they cannot be stopped prematurely. But this is exactly what I need for Alpha-Beta pruning: stop building the tree.

So I give it a naive try: show the type information in GHCi (`:t buildTreeAB`) and re-implement the entire thing based on my current understanding.

Having started implementing this, I noticed that I have absolutely zero clarity of mind right now. I need to do something else now and will revisit the issue later on.

# Day 40 (Tu, 2025-09-16)

First, I got rid of the `Leaf` variant of my `Tree`. I had to remove it at three or four places, and the game still works.

Then I started implementing the `buildTreeAB` function, which worked rather well, until it stopped evaluating and got stuck. I had to write my bespoke `foldl1` function that stops recursion upon alpha-beta-range violation. But this function just never finishes, as far as I understand the situation. I have to think about that.

# Day 39 (Mo, 2025-09-15)

No code today, just some notes on Alpha-Beta pruning:

Consider a game situation in which the maximizing player X has three valid moves, to which the minimizing player O can react with three different moves each, leading to the following outcomes:

1. 3, 4, 3
2. 1, x, y
3. 5, 2, 8

The outcomes x and y are not known yet. If both players pick the optimal move, the tree evaluates as follows:

```
max(min(3,4,3),min(1,x,y),min(5,2,8))
= max(3,min(1,x,y),2)
```

The values of x and y are _irrelevant_. No matter how good a move O will pick, X won't send the game down that branch. Therefore, the current _at least_ and _at most_ values shall be tracked:

- alpha: _at least_ outcome
- beta: _at most_ outcome

The search can be stopped once an outcome exceeds those bounds, which are to be updated as better outcomes are encountered by the respective player:

- minimizing(alpha, beta)
    - update beta as a _lower_ outcome is detected
    - stop if outcome < alpha is found: maximizing won't move down that branch
- maximizing(alpha, beta)
    - update alpha as a _higher_ outcome is detected
    - stop if outcome > beta is found: minimizing won't move down that branch

With this understanding, I hopefully can implement Alpha-Beta-Pruning… tomorrow, when I have more time.

# Day 38 (Su, 2025-09-14)

I managed to implement tree building with evaluation in the same process. I extended the `Tree` definition, which now distinguishes between `Node` and `Leaf`, which leads to slightly more verbose code, but also brings additional clarity to it. This is a good trade-off for me as a beginner. I also optimized the delay time for the computer opponent. However, I should replace the linear factor by an exponential one.

I think something with the Minimax algorithm wasn't working properly, because the computer player was way too fast evenon the highest level, where a tree of up to nine levels is built up. Now I have to wait considerably longer, and the AI is also stronger.

So, I'll come back to Alpha-Beta pruning next week, hopefully tomorrow.

# Day 37 (Sa, 2025-09-13)

Little time today; I read about alpha-beta pruning and designed the API, but I wasn't able to implement it completely yet. I wonder if I need to merge the logic to build up and to validate the tree. This requires an addition to the tree data type: a `Maybe Int` field for the sub-tree's value. It's a good exercise, so I have to try that tomorrow.

# Day 36 (Fr, 2025-09-12)

Only little time this morning: I started reading about the Minimax algorithm in the Russell/Norvig book and then implemented the function to build up the tree. However, upon evaluating the tree, I noticed a flaw in my data structure, which I was able to fix. Building the complete tree up to five steps is certainly feasible for interactive game play, even for the initial move. I also wrote a score function to maximize for player X and minimize for player O by subtracting O's points from X's points. Like that, X can pick the maximum and O the minimum successor nodes.

During the day, I had the odd 5 minutes now and then to implement the Minimax algorithm, and it now seems to work fine. The human player can also pick the opponent's level from 1 to 9, corresponding to the levels the AI player computes for the tree.

In the evening, I optimized the output by displaying the coordinates on both sides of the grid, i.e. left and right; top and bottom. I also changed the `bestMove` function to a `bestMoves` (plural) function, which returns equally strong moves as a list, from which the AI player then picks a random move. This makes the initial phase a bit more interesting.

So I'm quite happy with the game now. Tomorrow, I first read about alpha-beta pruning, then try to optimize the game.

# Day 35 (Th, 2025-09-11)

Yesterday late in the evening, I played another round, and figured out a flaw in the logic to skip the turn for a player that is unable to move: I only use the respective other input function (random move, prompt move), but don't switch the player. So I have to re-consider this logic. Fortunately, I saved yesterday's board as a text file, now I can re-produce the test data. (It would be a good exercise to read such a board in from a file for better debugging.) Before falling asleep, I also thought about the data structure for the game tree, which I just wrote down today.

I managed to fix the skipping logic in the waiting room. It's still verbose, but now it's moved down to a `where` block, so that it no longer obscures the actual logic. On towards Minimax, finally.

# Day 34 (We, 2025-09-10)

Today, I started implementing the game logic. I am able to play against a random computer, but wins, draws, and situations where a player is unable to move are neither detected nor handled. I leave that for tomorrow; this was quite a productive morning hour.

Later on the train, I _almost_ finished the game implementation. There's one thing left: skip a player if he has no valid moves left. But the finishing (win/draw) condition works.

I also managed to resolve that issue in the evening; now I have a playable version of Reversi with a random computer opponent. On towards implementing Minimax!

# Day 33 (Tu, 2025-09-09)

I continued with Reversi. First, I wrote a function `display` (replacing the old one) that shows the grid with row and column indicators from a to h and from 1 to 8, respectively. Then I wrote a function `parseMove` that takes an input string and returns its coordinates, e.g. `"a5"` is parsed to `(0, 4)`. Parsing can fail, therefore it's wrapped in a `Maybe`. Finally, I wrote the `promptMove` function that prompts a player for a given grid for a move. The function is called recursively until the player entered a valid move, which is then returned as a `IO Pos`.

I still have some issues writing pure code within impure code `do` blocks, but I managed to write useful code again. Unfortunately, I can't spend more than an hour per day on Haskell, but it's still better then nothing.

Tomorrow, I'd like to write the game logic. The human player can pick either `X` or `O`; `X` always begins. The computer player will just do a random move, which of course must be a valid one. Then comes the Minimax algorithm. And then maybe Alpha-Beta Pruning.

# Day 32 (Mo, 2025-09-08)

Now with a working `affectedCoordinates` under my belt, applying moves was very easy. One major lesson with Haskell so far is that you absolutely need to dissect the problem top-down and build up the solution bottom-up. I'm also glad that I worked through a good deal of SICP since my first serious encounter with Haskell five years ago. Sometimes, tail-recursive functions with accumulators are very useful, even if not necessary that often in Haskell. Unfortunately, pattern matching has no option for _pinning_, i.e. match against the values of existing bindings, at least not to my knowledge; this was my major misunderstanding when first implementing `affectedCoordinates`. But it's possible to work around that.

Busy day today, I'll be thinking of what to implement next for Reversi. Probably it's interactive gameplay.

# Day 31 (Su, 2025-09-07)

Having been unable to figure out the issue with my `affectedCoordinates` implementation, I [reached out](https://discourse.haskell.org/t/reversi-applying-moves-with-case-of-and-pattern-matching/12922) to the Haskell community. As I expected, the compiler warnings pointed me into the right direction: I simply had a misconception about pattern matching. Having resolved that issue, the code works as expected. So I now only found a working solution for one of the hardest problems of the program, I only learned something about pattern matching. And my confidence towards compiler warnings also grew: Those messages can be really useful!

Having resolved this issue, I now can write the code to actually apply the move. But first, I need to reshape my shifts table from `(x,y)` coordinates to `(r,c)` coordinates. This is just more consistent.

# Day 30 (Sa, 2025-09-06)

I was thinking about Reversi yesterday, and once more thought that the logic to validate and apply moves are quite similar; figuring out which stones the application logic has to turn is exactly what is already done in the validation logic.

So instead of just returning a boolean, indicating whether or not the move is legal, it could return a list of affected coordinates. However, the validation logic is way easier to implement without the notion of coordinates by dissecting the grid into what I call "paths".

Writing the logic to apply the moves requires me to rethink the approach of the validation logic: I'll need those coordinates! But first, I'd like to figure out how to apply them.

I managed to write a function that applies the changes to a grid given a set of coordinates. Now I just need to figure out the coordinates, which I'll do later or maybe tomorrow (little time today).

Later on, I worked on a function `affectedCoordinates`. I figured out a tail-recursive logic using an accumulator. This is not for the sake of tail recursion, but to distinguish two cases: How to deal with a particular field value depends on if there was already an affected field found.

# Day 29 (Fr, 2025-09-05)

I continued working on my Reversi implementation. I managed to validate moves, which wasn't too hard.

Now comes the hard part: applying the moves. Using the validation logic, I can figure out which fields are affected on a grid. But I have no idea how to apply them. Given a grid, a list of affected coordinates, and a new value, a new grid shall be returned with the given value at the affected coordinates. I have no idea yet how to do this. So that's what I'll be thinking about today.

# Day 28 (Th, 2025-09-04)

I started with my Reversi implementation today. I figured a way to trace the path along all eight directions on the grid using operator sections.

I learned that `(-1)` is a syntactic special case: it's just the value negative one, not a function. One has to write `(subtract 1)` instead.

Having done that, I wrote a function to return all the horizontal, vertical, and diagonal paths on the grid from a starting point, which is done in a relatively elegant way using a list comprehension.

Having laid that groundwork, I can now write the next building block to validate moves.

I also gave chapter 12 a quick read in the afternoon. I'll have to work through that particular chapter very slowly, especially the example on the _State Monad_ left me puzzling.

# Day 27 (We, 2025-09-03)

I worked through exercises 3 and 4a. Then I decided to skip 4b, 4c, 4d for the moment. I'd like to come back to those, but first I'd like to learn more about the language. I'd also like to implement Reversi in Haskell, for which I'd also like to write the minimax algorithm again from scratch for better understanding it. After that, I'll tackle those exercises.

On towards the core of the second part: Chapter 12, titled _Monads and more_!

Having written this, I decided to adjust my plans slightly: In order to digest everything I've learned in chapter 11, I'll take a break with _Programming in Haskell_. Instead of continuing with chapter 12, I'll implement the game of Reversi in Haskell. This is the ideal candidate for minimax and alpha-beta pruning, since every board yields an objective score among the two players. So this will take up a week at least, for I'd rather struggle through it than looking stuff up.

# Day 26 (Tu, 2025-09-02)

Little time today, since I had to prepare today's lessons in the morning after a heavy headache today after teaching.

However, I managed to finished the instruction part of the chapter. There must be something wrong with the minimax implementation, because the bot is absolutely beatable. I need to figure that out. (Probably it's just a typo in the code I copied from the book.)

In the evening, I managed to get exercises 1 and 2 done. I also figured out the issue I had with the bot: I used the `wins` function instead of `won`, which I need to fix in my notes. After the fix, the bot is really unbeatable.

# Day 25 (Mo, 2025-09-01)

I continued working through chapter 11 and managed to finish the part about the minimax algorithm. My decision to use `Maybe` instead of empty or singleton lists fired a bit back, but I managed to find a workaround for it.

I'll need another day for the rest of the chapter, and probably quite some time more for the exercises, among which alpha-beta pruning has to be implemented. But this is all very good stuff, so I'll take my time. Chapter 12 on Functors, Applicatives, and Monads will be the most important one for me, to which I'm looking forward.

# Day 24 (Su, 2025-08-31)

I didn't manage to finish chapter 11 yet, I just was able to implement the interactive game. I again deviated from the book by using a `Maybe Grid` with `Just grid` and `Nothing` instead of `[Grid]` with a singleton `[grid]` and empty list `[]`. I leave game trees for later—or for tomorrow, because I have a lot of lessons to prepare for tomorrow.

# Day 23 (Sa, 2025-08-30)

I started working through chapter 11, which I first read once completely, and then summarized the first of three parts of it. Later on, I also implemented the printing of a grid, but decided to simplify considerabely compared with the book's implementation.

# Day 22 (Fr, 2025-08-29)

I finished the examples of chapter 10 and managed to do 5/6 exercises in the morning. The last one I have to leave for later, unfortunately, because today is a teaching day (10 lessons).

Having finished the 10 lessons, I managed to solve exercise 6 in the evening. I don't like the solution, but the task itself is a messy one, so I don't mind. I wrote a recursive function that applies the backspaces after the fact by processing the string in reverse. It was a good exercise, after all. No on towards chapter 11 (tomorrow).

# Day 21 (Th, 2025-08-28)

I already gave chapter 10 a quick read yesterday and then seriously worked through the theory part this morning. Having summarized that, I stopped after the first extended example and leave the rest for later or tomorrow.

# Day 20 (We, 2025-08-27)

I worked through the remaining exercises of chapter 9, which turned out to not be as terribly hard as I was afraid of. However, I had completely different results for exercises 4 and 5 as stated in the book, but I decided to move ahead nonetheless. I also stumbled upon some practical issues, for example how to use `sortBy`, and that one needs to `import Data.List` in order to use it. And on Arch Linux: the `ghc -dynamic` flag has to be used for compilation. (I already read that Arch's native Haskell deployment is not a good fit for Haskell programmers, but rather for users of Haskell programs such as `pandoc`.)

# Day 19 (Tu, 2025-08-26)

I worked through the optimisations of the countdown problem. Then I started with the exercises, where I got stuck at exercise 2. I'd like to do them thoroughly, which will take some time. So the second part of the book probably has to wait until Thursday.

# Day 18 (Mo, 2025-08-25)

I worked through the initial version of the countdown problem, but I didn't have time for the optimizations yet, and neither for the exercises.

# Day 17 (Su, 2025-08-24)

I worked through the exercises of chapter 8. Especially the last one was quite hard, so I looked up a solution from earlier. It's exactly the initial idea that I had: dumb but effective. Anyway, onwards to chaper 9 (tomorrow)!

# Day 16 (Sa, 2025-08-23)

Little time today; I just managed to finish up the tautology checker, which required some modifications to the existing `find` function. In the evening, I also worked through the abstract machien example. I leave the exercises for tomorrow.

# Day 15 (Fr, 2025-08-22)

This morning, my laptop refused to boot for the first time in almost ten years due to a Kernel Panic, which I need to investigate later. So I started working on the tautology checker example from chapter 8, but have some issues with functions that were defined earlier on. Unfortunately, I have little time today, because I have ten lessons to teach.

# Day 14 (Th, 2025-08-21)

I worked through the theory part of chapter 8 on types and type classes. I leave the extended examples for tomorrow and will tackle the exercises on Saturday.

# Day 13 (We, 2025-08-20)

No teaching duties today, so I had time to finish chapter 7 by working through the second half of its exercises. Some of those were challenging, but I consider it to be quite the appropriate difficulty level for me at the moment. Chapter 8 is about the type system, and chapter 9 will be an extended example. I must not get frustrated by them and make sure to finally tackle the second part of the book, which finally introduces me to effectful programming in Haskell. That will be some weekend material.

# Day 12 (Tu, 2025-08-19)

I worked through the first five exercises of chapter 7. I still don't intuitively grasp the semantics of `foldl` and `foldr`. The problem is not that I get their application wrong, but rather that I don't understand why my solution works. Both functions process a list from the left to right, but the accumulator value is handed from left to right in `foldl` and the other way around for `foldr`.

When writing the combine function for `foldr`, the accumulator contains the solution built up from the right; for `foldl`, the accumulator contains the solution built up from the left. In other words: Iteration always moves from left to right, but the accumulator is either moved from the left to right (`foldl`) or from the right to left (`foldr`).

A good hint is the type: `foldl` accepts the accumulator as the first (or _left_) parameter, `foldl` as the second (or _right_) argument.

It's also interesting from the didactic view: The ability to write down the types of `foldl` and `foldr` correctly without looking it up originates from either rote learning—or from understanding the semantics!

# Day 11 (Mo, 2025-08-18)

Only little time in the morning: I worked through the two extended examples in chapter 7, but haven't done the exercises yet, which will have to wait until tomorrow.

# Day 10 (Su, 2025-08-17)

I worked through the first part of chapter 7. Now I understand that `foldl` can be thought of in terms of forwarding an accumulator, while `foldr` builds up a chain of recursive function calls.

# Day 9 (Sa, 2025-08-16)

Having read chapter 6 quickly yesterday evening, I summarized it today and worked through the exercises, which were rather easy. (The nine months of working through the first three chapters of SICP really paid off!) Now comes the most interesting chapter for me of the first part: higher-order functions.

# Day 8 (Fr, 2025-08-15)

I worked through the extended Caesar cipher example and worked through all of the exercises, which were rather easy.

# Day 7 (Th, 2025-08-14)

I continued with chapter 5 and managed so summarize the first part. The second part is an extended example, which I save for later (or tomorrow).

# Day 6 (We, 2025-08-13)

Little time today; I started working through chapter 5 (on list comprehensions) in the morning.

# Day 5 (Tu, 2025-08-12)

I finished working through chapter 4, for which I wrote a rather terse summary. I'm not getting disciplined in leaving things away—the kind of discipline that _saves_ effort. As usual, I was not interested in the exercises dealing with formal definitions; I'm rather interested in how to express my thoughts in Haskell, for which this chapter had a couple of great exercises, forcing the reader to solve the same problem using various approaches. That's what I expect from a text book.

I now remember how much I struggled with the book five years ago. Functional programming really became second nature to me in the time since then. Re-visiting hard stuff is the best learning strategy for me. It takes time, but little effort, or rather effort distributed over time.

# Day 4 (Mo, 2025-08-11)

Today I received the book _Learn You a Haskell for Greater Good_. I only tried working through it on my commutes roughly ten years ago, but must have given up on it. Having read the introduction and the first chapter, the book seems dense, informal, and fun, but never to the point of being daft. I like it as an informal companion to _Programming Haskell_. Unfortunately, the book has no exercises.

In the evening, I started working through chapter 4 in _Programming Haskell_.

# Day 3 (Su, 2025-08-10)

I worked through chapter 3 in _Programming Haskell_.

# Day 2 (Sa, 2025-08-09)

Having been out all day long, I found time to work through chapter 2 of _Programming Haskell_.

# Day 1 (Fr, 2025-08-08)

First commit today after finishing chapter 1 of _Programming Haskell_. I got my setup to run and did all the exercises, which were rather easy.

# Day 0 (Th, 2025-08-07)

I started reading chapter 1 of _Programming Haskell_ (Second Edition) by Graham Hutton. I already worked throuh half of the book five years ago, but now I'd like to get through.
