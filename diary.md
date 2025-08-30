# Day 23 (Sa, 2025-08-30)

I started working through chapter 11, which I first read once completely, and then summarized the first of three parts of it.

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
