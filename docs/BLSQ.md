% Burleseque - Moonpage
% Roman Müntener

# ABOUT

An interpreter for the esoteric programming language *The Burlesque Programming Language*. 

**Author:** Roman Müntener, 2012-?

Useful Weblinks:

* [Burlesque on RosettaCode](http://rosettacode.org/wiki/Category:Burlesque)
* [Source code](http://github.com/FMNSSun/Burlesque)
* [Language Reference](http://mroman.ch/burlesque/lref.html)

Until this moonpage is complete, please consult the Language Reference. Once compelete, the moonpage will
superseed the Language Reference.

# SYNOPSIS

```cmdline
blsq <options>
```

# LANGUAGE

## SYNTAX

## BUILT-INS

### Add ```.+```

```Int a, Int b: ``` Integer addition.

```shell
blsq ) 5 5.+
10
```

```Double a, Double b: ``` Double addition.

```shell
blsq ) 5.1 0.9.+
6.0
```

```String a, String b: ``` Concatenates two strings.

```shell
blsq ) "ab" "cd" .+
"abcd"
```

```Int a, String b: ``` Returns the first `a` characters of `b` as a String.

```shell
blsq ) 3 "abcdef" .+
"abc"
```

```Block a, Block b: ``` Concatenates two blocks.

```shell
blsq ) {1 2}{3 4}.+
{1 2 3 4}
```

```Char a, Char b: ``` Creates a string with the two characters `a` and `b` in it (in that exact order).

```shell
blsq ) 'a'b.+
"ab"
```

```String a, Char b: ``` Append `b` to `a`.

```shell
blsq ) "ab"'c.+
"abc"
```

```Int a, Block b: ``` Returns the first `a` elements of `b`.

```shell
blsq ) 2{1 2 3}.+
{1 2}
```

```Block a, Int b: ``` Returns the first `b` elements of `a`.

```shell
blsq ) {1 2 3}2.+
{1 2}
```

```String a, Int b: ``` Returns the first `b` characters of `a` as a String.

```shell
blsq ) "abc"2.+
"ab"
```

```Double a, Int b: ``` Convert `b` to Double, then perform addition.

```shell
blsq ) 1.0 2.+
3.0
```

```Int a, Double b: ``` Convert `a` to Double, then perform addition.

```shell
blsq ) 2 1.0.+
3.0
```

### AddX ```_+```

```Int a, Int b: ``` Creates a Block with the two Integers `a` and `b` as elements (in this exact order).

```shell
blsq ) 1 2_+
{1 2}
```

```Double a, Double b: ``` Creates a Block with the two Doubles `a` and `b` as elements (in this exact order).

```shell
blsq ) 1.0 2.0_+
{1.0 2.0}
```

```String a, String b: ``` Concatenates the two Strings.

```shell
blsq ) "ab""cd"_+
"abcd"
```

```Block a, Block b: ``` Concatenates the two Blocks.

```shell
blsq ) {1}{2}_+
{1 2}
```

```Char a, Char b: ``` Converts both arguments two string and concatenates them.

```shell
blsq ) 'a'b_+
"ab"
```

```String a, Char b: ``` Converts `b` to String, then concatenates.

```shell
blsq ) "a"'b_+
"ab"
```

```Char a, String b: ``` Converts `a` to String, then appends it to `b`.

```shell
blsq ) 'a"b"_+
"ba"
```

```Int a, String b: ``` Converts `a` to String, then appends it to `b`.

```shell
blsq ) 1"b"_+
"b1"
```


```String a, Int b: ``` Converts `b` to String, then concatenates.

```shell
blsq ) "b"1_+
"b1"
```

### Div ```./```

```Int a, Int b: ``` Integer division.

```shell
blsq ) 10 3./
3
```


```Double a, Double b: ``` Double division.

```shell
blsq ) 10.0 3.0./
3.3333333333333335
```

```String a, String b: ``` Removes `b` from the beginning of `a` iff `b` is a prefix of `a`.

```shell
blsq ) "README.md" "README" ./
".md"
blsq ) "README.md" "REDME" ./
"README.md"
```

```Block a, Block b: ``` Removes `b` from the beginning of `a` iff `b` is a prefix of `a`.

```shell
blsq ) {1 2 3}{1 2}./
{3}
blsq ) {1 2 3}{2 2}./
{1 2 3}
```

```Int a, Double b: ``` Converts `a` to Double, then divides.

```shell
blsq ) 10 3.0./
3.3333333333333335
```

```Double a, Int b: ``` Converts `b` to Double, then divides.

```shell
blsq ) 10.0 3./
3.3333333333333335
```

### Duplicate ```J``` ```^^```

Duplicates the top most element.

```shell
blsq ) 5
5
blsq ) 5J
5
5
```

### Mul ```.*```

```Int a, Int b: ``` Integer multiplication.

```shell
blsq ) 2 3.*
6
```

```Double a, Double b: ``` Double multiplication.

```shell
blsq ) 2.0 3.0.*
6.0
```

```String a, Int b: ``` Creates a Block containing `a` exactly `b` times.

```shell
blsq ) "ab"3.*
{"ab" "ab" "ab"}
```

```Char a, Int b: ``` Creates a String containing `a` exactly `b` times.

```shell
blsq ) 'a 3.*
"aaa"
```

```Block a, Int b: ``` Creates a Block containing `a` exactly `b` times.

```shell
blsq ) {1 2}3.*
{{1 2} {1 2} {1 2}}
```

```String a, String b: ``` Appends `a` to `b` then reverses.

```shell
blsq ) "123""456".*
"321654"
```

```Int a, Double b: ``` Converts `a` to Double, then multiplies.

```shell
blsq ) 2 3.0.*
6.0
```

```Double a, Int b: ``` Converts 'b' to Double, then multiplies.

```shell
blsq ) 2.0 3.*
6.0
```

### Sub ```.-```

```Int a, Int b: ``` Integer subtraction.

```shell
blsq ) 1 5.-
-4
```

```Double a, Double b: ``` Double subtraction.

```shell
blsq ) 1.0 4.0.-
-3.0
```

```String a, String b: ``` Removes `b` from the end of `a` iff `b` is a suffix of `a`.

```shell
blsq ) "README.md" ".md".-
"README"
blsq ) "README.md" ".txt".-
"README.md"
```

```Int a, Block b: ``` Removes the first `a` elements from `b`.

```shell
blsq ) 3{1 2 3 4}.-
{4}
```

```String a, Int b: ``` Removes the first `b` characters from `a`.

```shell
blsq ) "abcd"2.-
"cd"
```

```Int a, String b: ``` Removes the first `a` characters from `b`.

```shell
blsq ) 2"abcd".-
"cd"
```

```Block a, Int b: ``` Removes the first `b` elements from `a`.

```shell
blsq ) {1 2 3 4}2.-
{3 4}
```

```Int a, Double b: ``` Converts `a` to Double, then subtracts.

```shell
blsq ) 4 3.0.-
1.0
```

```Double a, Int b: ``` Converts `b` to Double, then subtracts.

```shell
blsq ) 4.0 3.-
1.0
```

```Block a, Block b: ``` Removes `b` from the end of `a` iff `b` is a suffix of `a`.

```shell
blsq ) {1 2 3 4}{3 4}.-
{1 2}
blsq ) {1 2 3 4}{3 4 5}.-
{1 2 3 4}
```

### Swap ```j``` ```\/```

Swaps the top two elements.

```shell
blsq ) 1 2
2
1
blsq ) 1 2j
1
2
```
