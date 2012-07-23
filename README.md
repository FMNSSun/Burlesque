Burlesque
=========

It's part of our culture. Get over it!

# Builtins


```
-- | .+
-- 
-- Int Int -> Regular integer addition
-- Str Str -> String concatenation
-- Int Str -> Take first n characters of a string

-- | .-
--
-- Int Int -> Regular integer subtraction
-- Int Str -> Drop first n characters of a string
-- Str Str -> Opposite of string concatenation

-- | <-
--
-- Int -> Reverse digit
-- Str -> Reverse string

-- | ln
--
-- Str -> Returns a list of lines
-- Int -> Number of digits

-- | ri
--
-- Int -> Identity
-- Str -> Convert to Int

-- | ps
--
-- Str -> Parses a string as a BlsqExp

-- | ++
--
-- Block -> Sum of all (Int) elements

-- | [~
--
-- Block -> Last element
-- Str -> Last character

-- | ~]
--
-- Block -> All except last elements
-- Str -> All except last character

-- | \[
--
-- Block -> Concatenates Blocks in Block or Strings in Block

-- | m[
--
-- Block Block -> Map

-- | \/
--
-- StackManip -> Swap

-- | ^^
--
-- StackManip -> Duplicate

-- | vv
--
-- StackManip -> Pop

-- | if
--
-- Block Int -> If and only if

-- | e!
--
-- Block -> Eval

-- | w!
--
-- Block f, Block g -> while g do f
```