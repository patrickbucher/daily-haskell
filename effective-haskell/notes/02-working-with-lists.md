# Working with Lists

Strings are lists:

```ghci
λ ['h','e','l','l','o'] == "hello"
```

Thus, the concatenation operator `<>` can be used for any kind of list:

```ghci
λ [1,2,3] <> [4..9]
[1,2,3,4,5,6,7,8,9]
```

The n-th element of a list can be accessed using the `!!` operator:

```ghci
λ "hello" !! 4
'o'
λ [10,20..100] !! 5
60
```

Prepend ("cons") an element to a list:

```ghci
λ 0 : [1..3]
[0,1,2,3]
λ 0 : 1 : 2 : 3 : [4..9]
[0,1,2,3,4,5,6,7,8,9]
```

Access the _head_ (first element) and _tail_ (remainder) of a list:

```ghci
λ head [0..4]
0
λ tail [0..4]
[1,2,3,4]
λ tail [1]
[]
λ head []
ERROR
λ tail []
ERROR
```

Both `head` and `tail` are _partial_ functions, i.e. they are not defined for every input, e.g. `head` and `tail` on an empty list.

Check if a list is empty:

```ghci
λ null [1,2,3]
False
λ null []
True
```

Build up a list recursively:

```ghci
λ countdown n = if n == 0 then [] else n : countdown (n - 1)
λ countdown 10
[10,9,8,7,6,5,4,3,2,1]
```

Re-implement `foldl` and `foldr`:

```haskell
foldLeft f acc xs =
  if null xs
  then acc
  else foldLeft f (f acc (head xs)) (tail xs)

foldRight f acc xs =
  if null xs
  then acc
  else f (head xs) $ foldRight f acc (tail xs)
```

- A left fold calculates the next carry value (`acc`) and passes it on to the recursive call.
- A right fold passes the original carry value (`acc`) to the recursive call.

`foldLeft` is tail-recursive, `foldRight` builds up a recursive chain of computation.

Subtract using `foldl` and `foldr`:

```ghci
λ foldl (-) 10 [7,3] -- ((10 - 7) - 3)
0
λ foldr (-) 3 [7,10] -- (7 - (10 - 3))
0
λ foldl (-) 0 [7,3,1] -- (((0 - 7) - 3) - 1)
-11
λ foldr (-) 0 [7,3,1] -- (7 - (3 - (1 - 0)))
5
```

TODO: p 59
