% Burleseque - Moonpage
% Roman Müntener

# ABOUT

An interpreter for the esoteric programming language *The Burlesque Programming Language*. Actually,  Burlesque is less of a programming language
than it is a tool. The actual language behind it is very simple and the only thing that makes Burlesque notable is the amount of built-ins
it has. The syntax can be learnt within a few minutes (there are basically only Numbers, Strings and Blocks) and the concepts can be learnt
quickly as well. People familiar with functional programming languages will already know these concepts so Burlesque is especially easy to learn
if you already know the terms *map*, *filter*, *reduce*, *zip* and others. This moonpage tries to be as accurate, complete and easy to understand
as possible. If you encounter an error in the documentation please report it on [github](http://github.com/FMNSSun/Burlesque). 
**Author:** Roman Müntener, 2012-?

Useful Weblinks:

* [Burlesque on RosettaCode](http://rosettacode.org/wiki/Category:Burlesque)
* [Source code](http://github.com/FMNSSun/Burlesque)
* [Language Reference](http://mroman.ch/burlesque/lref.html)

**Until this moonpage is complete please consult the Language Reference. Once compelete, the moonpage will
superseed the Language Reference and this warning will disappear.**

## HISTORY

Burlesque has been under development since 2012 and is still being improved on a regular basis. It was built as a tool for me (mroman) to use
as a helper for my computer science studies. 

# SYNOPSIS

```cmdline
blsq 
  --file <path>           Read code from file (incl. STDIN)
  --file-no-stdin <path>  Read code from file (excl. STDIN)
  --no-stdin <code>       Read code from argv (excl. STDIN)
  --shell                 Start in shell mode
  --version               Print version info
  --compile <path>        Pseudo-compile file to haskell code
  --stdin <code>          Read code from argv (incl. STDIN)
```

* *path* - Path to a file
* *code* - Burlesque code

*STDIN* will be pushed as a String to the stack. On exit all elements are printed in order from top to bottom. No output will be produced before the Burlesque code terminates. 

## EXAMPLE USAGES

```cmdline
$ blsq --file-no-stdin hw.blsq 
Hello, world!
$ echo -n "hello" | blsq --file revstdin.blsq 
olleh
$ echo -n "hello" | blsq --stdin "<-Q"
olleh
$ blsq --no-stdin "2 64**"
18446744073709551616
```

```cmdline
$ echo -n `ls *.blsq` | blsq --stdin "wdzisp"
0 hw.blsq
1 index.blsq
2 prog.blsq
3 revstdin.blsq
4 test.blsq
```

```cmdline
$ echo -n `ls` | blsq --stdin 'wd{".blsq"!~}{".blsq".-}FMuN'
hw
index
prog
revstdin
test
```

```cmdline
$ df | blsq --stdin "ln[-{WD-]}muQ"
rootfs
udev
tmpfs
/dev/disk/by-uuid/2e7e48d9-b728-48f7-95db-a58db91f4769
tmpfs
tmpfs
```

# LANGUAGE

## GLOSSARY

 * Block: A collection of values.
 * Identifier: An identifier. Refers to a Command. 
 * Command: A unit of execution. A Command does something.
 * Built-in: A Command that is built-in. It's not a user defined Command.
 * Map: Other languages call this asociative array or dictionary.
 * Special: A special identifier. 
 * Modifier: A Modifier is not a Command. A Modifier may modify the behaviour of the next Command
   or may alter evaluation. 
   
## TYPES

## SYNTAX

The syntax of Burlesque is rather easy on one hand but incredibly complicated once one dives
deeper into the rabbit hole. The basic syntax of Burlesque is rather easy.

### Identifier

An Identifier used to be exactly two characters long but this rule no longer applies. A built-in
can have pretty much any number of characters. The rules for parsing Identifiers are incredibly
complicated. Burlesque tries to parse Identifiers as a last option
meaning if any other parsing rule matches it is not parsed as an Identifier. It's thus vital
to know all the other parsing rules to be able to know when something is parsed as an identifier or not. 

A two character Identifier must start with one of `1234567890{}'," ()yYV` and the second character
can be anything. A single character built-in must start with one of `jJQ`. 

Two backticks can be used to parse an arbitrary length built-in. The two backticks aren't part of the parsed
built-in. It parses until it sees a space or newline. 

### Specials

A special is not a Command nor is it an Identifier. It is a Modifier that is evaluated at runtime. Currently the following specials exist:
`,`, `)`, `@`, `:`, and `%`. `#Q`, `#q`, `#J`, `#j` are technically modifiers as well but are parsed as
Identifiers and have a runtime type of `Ident`. 

## Evaluation model

Burlesque is a lazy language. It only evaluates things when necessary. This enables one to use
Blocks that are infinitely long. Burlesque has a primary stack and a secondary stack and
a Map. These are passed from built-in to built-in. However, in sub-evaluations for example when
using the map or filter builtins the primary stack changes during the sub-evaluation. In other words
sub-evaluations run on their own primary stack - sometimes - not always. It depends on the built-in. The secondary
stack and the Map remain accessible within sub-evaluations - sometimes - not always. It depends on
the built-in. Burlesque evaluates from left to right. When a value is encountered it is pushed to the
primary stack, when an Identifier is encountered the corresponding Command (built-in) will be executed. 
To push a Command to the stack use the quote syntax: `(++)` for example to push the Identifier `++` to the stack.

```shell
blsq ) (ab)to
"Ident"
blsq ) abto
"Error"
```  

The Map is where variables are stored (the DB command pushes the Map to the primary stack and allows
one to inspect it):

```shell
blsq ) %a=5 DB
<"a",5>
```

## BUILT-INS

### Abs ```ab```

```Int a: ``` Absolute value of `a`.

```shell
blsq ) -6ab
6
blsq ) 6ab
6
```

```Double a: ``` Absolute value of `a`.

```shell
blsq ) -6.0ab
6.0
blsq ) 6.0ab
6.0
```

```Char a: ``` Returns `1` if `a` is a control else returns `0`. 

```shell
blsq ) 0L[ab
1
blsq ) 32L[ab
0
```

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

### And ```&&```

This built-in auto-zips if an argument provided is a Block.

```Int a, Int b: ``` Bitwise AND.

```shell
blsq ) 2 4&&
0
blsq ) 1 7&&
1
```

### AndLs ```r&```

```Block {}```: If the top of the stack is an empty Block then this built-in will push a zero integer. 

```shell
blsq ) {}r&
0
```

```Otherwise```: *Defined as*: *```{&&}r[```*.

```shell
blsq ) {3 3 3}r&
3
```

### Append ```[+```

```Block a, Any b: ``` Append `b` to `a`.

```shell
blsq ) {1 2}9[+
{1 2 9}
```

```String a, Char b: ``` Append `b` to `a`.

```shell
blsq ) "ab"'c[+
"abc"
```

```Int a, Int b: ``` Concatenates Integers.

```shell
blsq ) 12 23[+
1223
```

**Authors' Notes:** This built-in is rather superfluous because most of its use-cases can be covered by either using `++` or `_+`. Yet, there may be some rare use-cases where you might want to use it for example in
`{[+}r[` or the like. 

### Average ```av```

```Block a:``` *Defined as:* *```J++jL[pd./```*. Calculates average.

```shell
blsq ) {1 2 3 4}J++jL[pd./
2.5
blsq ) {1 2 3 4}av
2.5
```

**Authors' Notes:** The `pd` is there to ensure that the result is always a Double. 

```Block a:``` Floor of `a` as Integer.

```shell
blsq ) 5.9av
5
blsq ) 5.1av
5
```

**Authors' Notes:** If you want the floor of `a` as Double use `fo`. 

### Average2 ```AV```

*Defined as:* *```PDav```*. Calculates average. 

```shell
blsq ) {1 2 3.2 4}AV
2.75
blsq ) {1 2 3.2 4}av
2.55
```

**Authors' Notes:** This built-in is a relic from earlier versions of Burlesque where `./` only worked on ```Int, Int``` or ```Double, Double``` but not with ```Double, Int``` or ```Int, Double```. 
This meant that ```av``` could only be used on Blocks that contained Doubles (because `++` would produce an Integer otherwise and the `L[pd./` would fail because an Integer could not be divided by
a Double). In such cases `AV` had to be used. With newer versions the functionality of `./` was extended and `av` can now be used on Blocks that contain Integers as well. However,
if you use `AV` on a Block that contains Doubles it will convert all these Doubles to ceiling(a) which is not what you want in most cases. Thus: The use of `av` is recommended as it is safer. 

### BlockAccess ```!!```

```Block a, Int b```: Returns the `b`-th element of `a`.

```shell
blsq ) {1 2 3 4}0!!
1
blsq ) {1 2 3 4}2!!
3
```

```String a, Int b```: Returns the `b`-th character of `a`.

```shell
blsq ) {1 2 3 4}0!!
1
blsq ) {1 2 3 4}2!!
3
```

### Box ```bx```

```Any a: ``` Puts `a` into a Block.

```shell
blsq ) 5bx
{5}
blsq ) 'abx
{'a}
```

### Concat ```\[```

```Block {}: ``` *Defined as:* *```{}```*. Empty Block becomes empty Block.

```shell
blsq ) {}\[
{}
```

```Block {Block (Char a)}: ``` Return a single character string consisting of `a`. 

```shell
blsq ) {'a}\[
"a"
```

```Block a: ``` *Defined as:* *```{_+}r[```*. Concatenates elements in a Block.

```shell
blsq ) {{1 1} {2 1} {3}}{_+}r[
{1 1 2 1 3}
blsq ) {{1 1} {2 1} {3}}\[
{1 1 2 1 3}
blsq ) {'a 'b 'c}\[
"abc"
```

**Authors' Notes:** There is an additional special case when ```{_+}r[``` does not return a Block the return value will be boxed. Why this special case exist remains unknown.

### ConcatMap ```\m```

*Defined as:* *```m[\[```*. 

```shell
blsq ) {1 2 3}{ro}m[\[
{1 1 2 1 2 3}
blsq ) {1 2 3}{ro}\m
{1 1 2 1 2 3}
blsq ) {1 2 3}{ro}m[
{{1} {1 2} {1 2 3}}
blsq ) "abc"{'ajr@}\m
"aababc"
```

**Authors' Notes:** The Map built-in detects if the input argument is a String and will concat automatically. Compare these examples:

```shell
blsq ) "abc"{'ajr@}m[
{'a 'a 'b 'a 'b 'c}
blsq ) "abc"XX{'ajr@}m[
{{'a} {'a 'b} {'a 'b 'c}}
blsq ) "abc"XX{'ajr@}\m
{'a 'a 'b 'a 'b 'c}
blsq ) "abc"XX{'ajr@}\m\[
"aababc"
blsq ) "abc"{'ajr@}\m
"aababc"
```

### Contains ```~[```

```Block a, Any b: ``` Returns 1 if `a` contains `b` otherwise returns 0.

```shell
blsq ) {1 2 3}4~[
0
blsq ) {1 2 3}2~[
1
```

```String a, Char b: ``` Returns 1 if `a` contains `b` otherwise returns 0.

```shell
blsq ) "abc"'b~[
1
blsq ) "abc"'z~[
0
```

```Int a, Int b: ``` Returns 1 if `a` contains `b` otherwise returns 0 (works on absolute values).

```shell
blsq ) 1223 22~[
1
blsq ) 1223 21~[
0
```

```String a, String b: ``` Returns 1 if `a` contains `b` otherwise returns 0.

```shell
blsq ) "hello" "ell"~[
1
blsq ) "hello" "elo"~[
0
```

### Continuation ```c!```

Generally speaking a *Continuation* refers to executing code on a snapshot of the stack and then pushing the result back to the actual stack. 
This means that this built-in lets you run code without destroying data on the stack.

```Block a: ``` Run `a` as a Continuation.

```shell
blsq ) 5 4.+
9
blsq ) 5 4{.+}c!
9
4
5
blsq ) 5 4{.+J}c!
9
4
5
```

### Decrement ```-.```

```Int a: ``` Decrements `a`.

```shell
blsq ) 5-.
4
```

```Char a: ``` Returns the previous character (unicode point - 1)

```shell
blsq ) 'c-.
'b
```

```String a: ``` Prepend first character of `a` to `a`.

```shell
blsq ) "abc"-.
"aabc"
```

```Block a: ``` Prepend first element of `a` to `a`.

```shell
blsq ) {1 2 3}-.
{1 1 2 3}
```

### Difference ```\\```

**Notes:** If left argument contains duplicates as many of them will be removed as are in the right argument. Order is preserved. 

```Block a, Block b: ``` Difference of `a` and `b`.

```shell
blsq ) {1 1 1 2 3}{1 1}\\
{1 2 3}
```

```String a, String b: ``` Difference of `a` and `b`.

```shell
blsq ) "abcde""ce"\\
"abd"
```

```Int a, Int b: ``` Difference of `a` and `b`.

```shell
blsq ) 1232 22\\
13
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

```Char a: ``` Returns `1` if `a` is a number else returns `0`. 

```shell
blsq ) 1634L[./
1
blsq ) '9./
1
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

### DupSwap ```^/```

*Defined as:* *```^^\/```*. 

**Authors' Notes:** Nobody has yet figured out what this is actually useful for. Some say it's useless, other's say... 'Fuck you'.

### Equal ```==```

```Any a, Any b: ``` Returns 1 if `a == b` else returns 0.

```shell
blsq ) 5 5==
1
blsq ) 5.0 5==
0
blsq ) 3 2==
0
blsq ) {1 23}{1 23}==
1
```

### Eval ```e!```

```Block a: ``` Evaluates (executes) `a`.

```shell
blsq ) {5 5.+}e!
10
```

**Authors' Notes:** If you want to eval a String use `pe`. 

### EvalMany ```E!```

*Defined as:* *```.*\[e!```*. This built-in can be used to evaluate a Block a number of times.

```shell
blsq ) 1{J.+}1E!
2
blsq ) 1{J.+}2E!
4
blsq ) 1{J.+}3E!
8
blsq ) 1{J.+}4E!
16
blsq ) 1{J.+}4.*\[e!
16
```

### Explode ```XX```

```String a: ``` Converts `a` to a Block of characters. 

```shell
blsq ) "abc"XX
{'a 'b 'c}
```

```Int a: ``` Converts `a` to a Block of digits (works on absolute value.

```shell
blsq ) 971XX
{9 7 1}
```

```Double a: ``` Converts `a` to a Block containing floor(`a`) and ceiling(`a`).

```shell
blsq ) 5.3XX
{5 6}
```

```Char a: ``` Converts to String.

```shell
blsq ) 'aXX
"a"
```

```Block a: ``` No operation.

```shell
blsq ) {1 2}XX
{1 2}
```

**Authors' Notes:** Sometimes this built-in is also referred to as *Xplode*. 

### Filter ```f[```

```String a, Block f: ``` *Somewhat defined as:* *```\/XX\/f[```*. Explodes the String to a Block of Chars
before doing ```f[```. This has a lot of special casing to simulate proper behaviour. The ```f[``` actually
returns a Block of Chars which is then concatenated to form a proper String again.  

```shell
blsq ) "abcdehge"{'e==}f[
"ee"
```

```Block v, Block f: ``` Filters `f` using `v` as the condition. Filter removes all elements of the Block
for which `v` returns false. 

```shell
blsq ) {1 2 3 4 5 6}{2dv}f[
{2 4 6}
blsq ) {1 2 3 4 5 6}{3.>}f[
{4 5 6}
```

### FindIndex ```fi```

```Block a, Block p```: Scans through the list (l2r) and returns the index of the first element where
`p` evaluates to true. 

```shell
blsq ) {1 2 3 4 5 6}{3==}fi
2
```

```String a, Block p```: *Defined as*: *```\/XX\/fi```*. 

```shell
blsq ) "abcdef"{'a!=}fi
1
```

### FindIndexEq ```Fi```

```Block a, Any x```: Scans through the list (l2r) and returs the index of the first element equal
to `x`. 

```shell
blsq ) {1 3 9 10 2}10Fi
3
```

```String a, Char x```: Scans through the list (l2r) and returns the index of the first element equal
to `x`. 

```shell
blsq ) "abcdef"'aFi
0
```

### FindIndices ```fI```

```Block a, Block p```: Scans through the list (l2r) and returns all indices of elements where
`p` evaluates to true. 

```shell
blsq ) {1 2 3 4 5 6 7 8}{2.%0==}fI
{1 3 5 7}
```

```String a, Block p```: *Defined as*: *```\/XX\/fI```*. 

```shell
blsq ) "hello"{'l==}fI
{2 3}
```

### Format ```FF```

```Pretty a, Int format: ``` Change format of `a` to `format`.

Formats are:

* 0 - Normal
* 1 - No spaces
* 2 - With spaces
* 3 - Raw

```shell
blsq ) {1 2 {3 4 "5"}}sh0FF
[1, 2, [3, 4, "5"]]
blsq ) {1 2 {3 4 "5"}}sh1FF
[1,2,[3,4,"5"]]
blsq ) {1 2 {3 4 "5"}}sh2FF
[1 2 [3 4 "5"]]
blsq ) {1 2 {3 4 "5"}}sh3FF
{1 2 {3 4 "5"}}
```

### FormatFromFormat ```Ff```

*Defined as:* *```FFff```*. Generally just a shortcut for `FFff`.

```shell
blsq ) {1 2 {3 4}}sh2Ff
"[1 2 [3 4]]"
```

**Authors' Notes:** In most cases you want to use `SH` directly. 

### FromFormat ```ff```

```Pretty a: ``` Converts the `a` to String.

```shell
blsq ) {1 2 {3 4}}shff
"[1, 2, [3, 4]]"
blsq ) {1 2 {3 4}}sh2FFff
"[1 2 [3 4]]"
```

### Greater ```.>```

```Any a, Any b: ``` Returns 1 if `a > b` else returns 0.

```shell
blsq ) 3.0 2.9 .>
1
blsq ) 2.0 2.9 .>
0
blsq ) 10 5 .>
1
blsq ) 10 5.0 .>
0
blsq ) 'a 1 .>
1
blsq ) 'a 9.0 .>
1
blsq ) 'a {} .>
0
blsq ) {} 9.0 .>
1
blsq ) {} 9 .>
1
```

**Note:** Comparing values with different types may result in unexpected (but determinstic, thus not undefined) behaviour. 

### Group ```=[```

```Block a: ``` Groups together elements next to each other that are equal.

```shell
blsq ) {1 2 2 3 4 4 5 6}=[
{{1} {2 2} {3} {4 4} {5} {6}}
blsq ) {1 2 2 3 4 4 4 6}=[
{{1} {2 2} {3} {4 4 4} {6}}
```

```String a: ``` Groups together characters next to each other that are equal.

```shell
blsq ) "abbbbbc"=[
{"a" "bbbbb" "c"}
```

**See also:** *GroupBy*.

### Head ```-]```

```Block a: ``` Returns the first element of `a`.

```shell
blsq ) {2 4 0}-]
2
```

```String a: ``` Returns the first character of `a`.

```shell
blsq ) "abc"-]
'a
```

**Authors' Notes:** If you need the first character of `a` as a String use `-~`. 

```Int a: ``` Returns the first digit of `a` (works on absolute value).

```shell
blsq ) -451-]
4
```

### HeadTail ```-~```

*Defined as:* *```-][-```*. 

```shell
blsq ) "abcd"-][-
"a"
blsq ) {{1 2 3} {4 5 6}}-~
{2 3}
blsq ) "abcd"-~
"a"
```

**Authors' Notes:** Useful to get the first character of a String as a String. 

### IfElse ```ie```

```Block a, Block b, Int a: ``` Executes `a` if b is not zero, otherwise executes `b`.

```shell
blsq ) 5{3.*}{2.*}1ie
15
blsq ) 5{3.*}{2.*}0ie
10
```

**Authors' Notes:** This built-in is terrible because in most real-world cases it requires at least two additional swaps to get the result of a predicate to the top of the stack.

### Iff ```if```

```Int a, Block b: ``` Executes `b` only iff `a` is not zero.

```shell
blsq ) 5 1{3.*}if
15
blsq ) 5 0{3.*}if
5
```

```Block a, Int b: ``` Executes `a` only iff `b` is not zero.

```shell
blsq ) 5{3.*}0if
5
blsq ) 5{3.*}1if
15
```

### Increment ```+.```

```Int a: ``` Increments a.

```shell
blsq ) 5+.
6
```

```Char a: ``` Returns the next character (unicode point + 1).

```shell
blsq ) 'a+.
'b
```

```String a: ``` Appends the last character of `a` to `a`.

```shell
blsq ) "abc"+.
"abcc"
```

```Block a: ``` Appends the last element of `a` to `a`.

```shell
blsq ) {1 2 3}+.
{1 2 3 3}
```

### InfixOf ```~~```

```Block a, Block b: ``` Returns 1 if `b` is an infix of `a` otherwise returns 0.

```shell
blsq ) {1 2 3 4}{2 3}~~
1
blsq ) {1 2 3 4}{3 3}~~
0
```

**Authors' Notes:** For Strings use `~[`. 

### Init ```~]```

```Block a: ``` Returns all but the last elements of `a`.

```shell
blsq ) {1 2 3}~]
{1 2}
```

```String a: ``` Returns all but the last character of `a`.

```shell
blsq ) "12a"~]
"12"
```

```Int a: ``` Returns all but the last digit of `a` (as Integer) (works on absolute value).

```shell
blsq ) 451~]
45
```

### InitTail ```~-```

*Defined as:* *```~][-```*. Can be used to remove the first and last element of a String/Block.

```shell
blsq ) "abcd"~][-
"bc"
blsq ) "abcd"~-
"bc"
```

### Intersection ```IN```

**Notes:** Duplicates in the left argument are preserved. Order is preserved. 

```Block a, Block b: ``` Intersection of `a` and `b`.

```shell
blsq ) {5 1 1 2 2 2 2} {1 2 2 2 3 4 5}IN
{5 1 1 2 2 2 2}
```

```String a, String b: ``` Intersection of `a` and `b`.

```shell
blsq ) "abc""dce"IN
"c"
```

```Int a, Int b: ``` Intersection of `a` and `b`.

```shell
blsq ) 512 721IN
12
```

### Intersperse ```[[```

```Any a, Block b: ``` Inserts `a` between elements in `b`.

```shell
blsq ) 0{1 2 3}[[
{1 0 2 0 3}
```

```Char a, String b: ``` Inserts `a` between characters in `b`.

```shell
blsq ) 'x"abc"[[
"axbxc"
```

**See also:** *Intercalate*. 

### Last ```[~```

```String a: ``` Returns the last character of `a`.

```shell
blsq ) "abc"[~
'c
```

```Block a: ``` Returns the last element of `a`.

```shell
blsq ) {1 2 3}[~
3
```

```Int a: ``` Returns the last digit of `a` (works on absolute value).

```shell
blsq ) 451[~
1
```

### Length ```L[```

```String a: ``` Number of characters in a String.

```shell
blsq ) "abc"L[
3
```

```Block a: ``` Number of elements in a Block.

```shell
blsq ) {1 2 3}L[
3
```

```Int a: ``` Converts to character based on unicode code point.

```shell
blsq ) 69L[
'E
blsq ) 98L[
'b
```

```Char a: ``` Returns case as either `'A` or `'a`.

```shell
blsq ) 'BL[
'A
blsq ) 'bL[
'a
```

### Lines ```ln```

```String a: ``` Split `a` into lines. 

```shell
blsq ) "abc\ndef\ngeh"ln
{"abc" "def" "geh"}
```

```Int a: ``` Number of digits in `a` (works on absolute value).

```shell
blsq ) 123ln
3
blsq ) -123ln
3
```

```Block a, Block b: ``` Returns whichever is longer. If both are equal in length `b` is returned.

```shell
blsq ) {1 2}{1 2 3}ln
{1 2 3}
blsq ) {1 2 4}{1 2 3}ln
{1 2 3}
```

```Char a: ``` Returns 1 if `a` is a printable character otherwise returns 0.

```shell
blsq ) 1L[ln
0
blsq ) 'aln
1
```

**See also:** *WithLines*.

### Map ```m[```

```String a, Block f: ``` *Defined as:* *```jXXjm[\[```*. Applies `f` to every character in `a`.

```shell
blsq ) "aBc"{<-}jXXjm[\[
"AbC"
blsq ) "aBc"{<-}m[
"AbC"
```

```Block a, Block f: ``` Applies `f` to every element in `a`.

```shell
blsq ) {1 2 3 4 5}{J.*}m[
{1 4 9 16 25}
```

**See also:** *ConcatMap* and there are many other different versions and shortcuts for *Map*. 

### MapDup ```[m```

*Defined as:* *```(^^)+]m[```*.

```shell
blsq ) {1 2 3}{?*}[m
{1 4 9}
```

### MapParse ```[M```

*Defined as:* *```(ps)+]m[```*. Can be used as a shortcut if the first step of the function that is
being mapped over the Block is parsing. 

```shell
blsq ) "5 6;7 8"';;;{++}[M
{11 15}
```

### MapPretty ```M[```

*Defined as:* *```m[sh```*. Can be used as a shortcut if the result of mapping should
be pretty printed. 

```shell
blsq ) "hi there"{L[}m[
"aaaaaaaa"
blsq ) "hi there"{L[}M[
aaaaaaaa
```

### MapString ```]m```

*Defined as:* *```(Sh)[+m[```*. Can be used when one wants to apply a function to every
element of a Block and each result should be converted to a String.

```shell
blsq ) {{1 2 3}{4 5 6}}{<-}]m
{"[3, 2, 1]" "[6, 5, 4]"}
```

### MapToPretty ```M]```

*Defined as:* *```{sh}m[```*. Can be used as a shortcut to convert every element of a 
Block to Pretty. 

```shell
blsq ) "hi there"wd
{"hi" "there"}
blsq ) "hi there"wdM]
{hi there}
```

### MapToPrettyFromFormat ```m]```

*Defined as:* *```{shff}m[```*. 

```shell
blsq ) {{1 2 3}{4 5 6}}m]
{"[1, 2, 3]" "[4, 5, 6]"}
```

### Matches ```~=```

```String str, String regex: ``` Returns 1 if `regex` matches `str` otherwise returns 0.

```shell
blsq ) "123""[0-3]{3}"~=
1
blsq ) "123""[1-3]{3}"~=
1
blsq ) "123""[1-3]{4}"~=
0
```

### MatchesList ```=~```

```String str, String regex: ``` Returns the capturing groups as a list. Empty block if no matches or no capture groups were used in the regular expression. 

```shell
blsq ) "123abc""([0-3]{3}).(b.)"=~
{"123" "bc"}
```

### Max ```>.```

```Any a, Any b: ``` Returns whichever is greatest.

```shell
blsq ) 5 6>.
6
blsq ) 6 5>.
6
blsq ) {12}12>.
{12}
```

### Maximum ```>]```

```Block a: ``` Returns the maximum of `a`.

```shell
blsq ) {1 2 3 2 1}>]
3
```

```String a: ``` Returns the maximum of `a`.

```shell
blsq ) "debca">]
'e
```

```Int a: ``` Returns the largest digit as an Integer.

```shell
blsq ) 1971>]
9
blsq ) 1671>]
7
```

### Min ```<.```

```Any a, Any b: ``` Returns whichever is smallest.

```shell
blsq ) 5 4<.
4
blsq ) 5 4<.
4
blsq ) 10 10.0<.
10
```

### Minimum ```<]```

```Block a: ``` Returns the minimum of `a`.

```shell
blsq ) {1 2 0 3}<]
0
```

```String a: ``` Returns the minimum of `a`.

```shell
blsq ) "bac"<]
'a
```

```Int a: ``` Returns the smallest digit as an Integer.

```shell
blsq ) 109<]
0
```

### Mod ```.%```

This is an auto-zip and auto-map built-in.

```Int a, Int b: ``` Integer modulo.

```shell
blsq ) 10 3.%
1
```

```Char a: ``` Returns `1` if `a` is punctuation otherwise returns `0`. 

```shell
blsq ) '..%
1
blsq ) 'a.%
0
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

```Double a, Int b: ``` Converts `b` to Double, then multiplies.

```shell
blsq ) 2.0 3.*
6.0
```

```Char a: ``` Returns `1` if `a` is a symbol else returns `0`. 

```shell
blsq ) '9.*
0
blsq ) '+.*
1
```

### NotEqual ```!=```

*Defined as:* *```==n!```*.

```shell
blsq ) 4 4==n!
0
blsq ) 4 3==n!
1
blsq ) 3 4==n!
1
blsq ) 3 4!=
1
blsq ) 4 4!=
0
```

### Nub ```NB```

Nub means *removing duplicates*. Order is preserved. 

```Block a: ``` Nub `a`.

```shell
blsq ) {5 1 1 2 2 2 2}NB
{5 1 2}
```

```String a: ``` Nub `a`.

```shell
blsq ) "abccd"NB
"abcd"
```

```Int a: ``` Nub `a`.

```shell
blsq ) 101010011NB
10
```

```Char a: ``` Returns `1` if `a` is lower case else returns `0`. 

```shell
blsq ) 'BNB
0
blsq ) 'bNB
1
```

### Or ```||```

This built-in auto-zips if an argument provided is a Block.

```Int a, Int b: ``` Bitwise OR.

```shell
blsq ) 2 4||
6
blsq ) 2 {4 8}||
{6 10}
```

### OrLs ```r|```

```Block {}```: If the top of the stack is an empty Block then this built-in will push a zero integer. 

```shell
blsq ) {}r|
0
```

```Otherwise```: *Defined as*: *```{||}r[```*.

```shell
blsq ) {1 2 3}r|
3
```

### PadLeft ```P[```


```Block a, Int b, Any c: ``` Pad `a` to length `b` by inserting `c` on the left (or removing elements from the right).

```shell
blsq ) {3 4 5}4 1P[
{1 3 4 5}
blsq ) {3 4 5 6 7}4 1P[
{3 4 5 6}
```

```String a, Int b, Char c: ``` Pad `a` to length `b` by inserting `c` on the left (or removing characters from the right).

```shell
blsq ) "12"4' P[
"  12"
blsq ) "12345"4' P[
"1234"
```

### PadRight ```[P```


```Block a, Int b, Any c: ``` Pad `a` to length `b` by inserting `c` on the right (or removing elements from the right).

```shell
blsq ) {3 4 5}4 1[P
{3 4 5 1}
blsq ) {3 4 5 6 7}4 1[P
{3 4 5 6}
```

```String a, Int b, Char c: ``` Pad `a` to length `b` by inserting `c` on the right (or removing characters from the right).

```shell
blsq ) "12345"4' [P
"1234"
blsq ) "12"4' [P
"12  "
```

### Parse ```ps```

This built-in auto-maps if the argument given is a Block.

```String a: ``` Tries to parse `a` with the Burlesque parser. (Tries to parse `a` as Burlesque code). Returns a Block.

```shell
blsq ) "5"ps
{5}
blsq ) "5 3.0.+"ps
{5 3.0 .+}
blsq ) "{5 3.0.+}m["ps
{{5 3.0 .+} m[}
```

**Authors' Notes:** This built-in is handy. Instead of doing something like:

```shell
blsq ) "5 3 6 7"wdri++
21
```

you can just do:

```shell
blsq ) "5 3 6 7"ps++
21
```

### Pop ```vv```

Removes the element on top of the stack.

```shell
blsq ) 1 2
2
1
blsq ) 1 2vv
1
```

**Authors' Notes:** If there only is one element on top of the stack using `,` is shorter. 

### PopSwap ```v/```

*Defined as:* *```vv\/```*. 

```shell
blsq ) 1 2 3 4v/
2
3
1
blsq ) 1 2 3 4vv\/
2
3
1
```

### Pow ```**```

```Int a, Int b: ``` Returns `a` to the power of `b` (`a ^ b`).

```shell
blsq ) 2 3**
8
```

```Double a, Double b: ``` Returns `a` to the power of `b` (`a ^ b`).

```shell
blsq ) 4.0 3.0**
64.0
```

```Block a, Block b: ``` Merges `a` and `b`. ```c = a_1, b_1, a_2, b_2```

```shell
blsq ) {1 2 3}{4 5 6}**
{1 4 2 5 3 6}
```

```String a, String b: ``` Merges `a` and `b`.

```shell
blsq ) "123""456"**
"142536"
```

```Char a: ``` Returns the unicode codepoint of `a` as an Integer.

```shell
blsq ) 'A**
65
blsq ) 'a**
97
```

### PrefixOf ```~!```

```Block a, Block b: ``` Returns 1 if `b` is a prefix of `a` otherwise returns 0.

```shell
blsq ) {1 4 3 2}{1 4}~!
1
blsq ) {1 4 3 2}{4 3}~!
0
```

```String a, String b: ``` Returns 1 if `b` is a prefix of `a` otherwise returns 0.

```shell
blsq ) "http://mroman.ch" "http://"~!
1
blsq ) "http://mroman.ch" "https://"~!
0
```

```Int a, Int b: ``` Returns 1 if `b` is a prefix of `a` otherwise returns 0 (works on absolute values).

```shell
blsq ) 1991 91~!
0
blsq ) 1991 199~!
1
```

### Prepend ```+]```

```Block a, Any b: ``` Prepend `b` to `a`.

```shell
blsq ) {1 2}3+]
{3 1 2}
```

```String a, Char b: ``` Prepend `b` to `a`.

```shell
blsq ) "ab"'c+]
"cab"
```

```Int a, Int b: ``` Prepends `b` to `a` (result is an Integer).

```shell
blsq ) 12 23+]
2312
```

### Pretty ```sh```

```Any a: ``` Convert to a Pretty with format *Normal*.

```shell
blsq ) "abc\ndef"sh
abc
def
blsq ) {1 2 3}
{1 2 3}
blsq ) {1 2 3}sh
[1, 2, 3]
blsq ) 5.0
5.0
blsq ) 5.0sh
5.0
```

### PrettyFormatFromFormat ```SH```

*Defined as:* *```jshjFf```*. Can be used to convert something to a String with a specified format. 

```shell
blsq ) {1 2 {3 4}}2SH
"[1 2 [3 4]]"
blsq ) {1 2 {3 4}}1SH
"[1,2,[3,4]]"
```

### PrettyFromFormat ```Sh```

*Defined as:* *```shff```*. Can be used to convert something to a String with format `Normal`.

```shell
blsq ) {1 2 3}Sh
"[1, 2, 3]"
```

### PrettyPretty ```sH```

*Defined as:* *```SHsh```*. Can be used to convert something for display with a specified format.

```shell
blsq ) {1 2 {3 4}}1sH
[1,2,[3,4]]
```

### Product ```pd```

```Block {}: ``` *Defined as:* *```1```*. The product of an empty block is one.

```Block a: ``` *Defined as:* *```{.*}r[```*. Calculates the product of a Block.

```shell
blsq ) {1 2 3 4}{.*}r[
24
blsq ) {1 2 3 4}pd
24
blsq ) {1 2 3.0 4}pd
24.0
```

```Int a: ``` *Defined as:* *```Shrd```*. Converts to double. 

```shell
blsq ) 5Shrd
5.0
blsq ) 5pd
5.0
blsq ) 5rd
5.0
```

```Double a: ``` Ceiling of `a` as Integer.

```shell
blsq ) 1.1pd
2
```

**Authors' Notes:** If you want ceiling of `a` as Double use `cl`. 

### ProductMany ```PD```

*Defined as:* *```{pd}m[```*. Just maps `pd` over a Block.

```shell
blsq ) {{1 2 3} 5 {2 4}}{pd}m[
{6 5.0 8}
blsq ) {{1 2 3} 5 {2 4}}PD
{6 5.0 8}
```

**Authors' Notes:** Can be used as a shortcut for `)pd`. Otherwise this built-in doesn't offer too much over `rd` as `rd` auto-maps. 

### PushMany ```^p```

```Block a: ``` Pushes every element in `a` to the stack.

```shell
blsq ) 'a{1 2 3}^p
3
2
1
'a
```

### PushManyReverse ```p^```

```Block a: ``` Pushes every element in `a` to the stack in reversed order.

```shell
blsq ) 'a{1 2 3}p^
1
2
3
'a
```

### Range ```r@```

```Int a, Int b: ``` Generates a Block containing the numbers `a` through `b`.

```shell
blsq ) 1 10r@
{1 2 3 4 5 6 7 8 9 10}
```

```Char a, Char b: ``` Generates a Block containing the characters `a` through `b`.

```shell
blsq ) 'a'zr@
{'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z}
```

**Authors' Notes:** Use *RangeConcat* if you need a String. 

```Double a: ``` Square root of `a`.

```shell
blsq ) 64.0r@
8.0
```

```String a: ``` Returns a Block with all permutations of `a`.

```shell
blsq ) "abc"r@
{"abc" "bac" "cba" "bca" "cab" "acb"}
```

```Block a: ``` Returns a Block with all permutations of `a`.

```shell
blsq ) {1 0 9}r@
{{1 0 9} {0 1 9} {9 0 1} {0 9 1} {9 1 0} {1 9 0}}
```

### RangeInfinity ```R@```

```Str a: ``` All subsequences of `a`.

```shell
blsq ) "abc"R@
{"" "a" "b" "ab" "c" "ac" "bc" "abc"}
```

```Block a: ``` All subsequences of `a`.

```shell
blsq ) {1 2 3}R@
{{} {1} {2} {1 2} {3} {1 3} {2 3} {1 2 3}}
```

```Int a: ``` Generates a Block containing the numbers `a` to Infinity. 

```shell
blsq ) 5R@10.+
{5 6 7 8 9 10 11 12 13 14}
```

### ReadArray ```ra```

This built-in auto-maps if the argument given is a Block.

```String a: ``` Parses an array in `[,]`-notation.

```shell
blsq ) "[1,2,3]"ra
{1 2 3}
blsq ) "[1,[2,4],3]"ra
{1 {2 4} 3}
blsq ) "[1,[2 4],3]"ra
{1 {2 4} 3}
blsq ) "[1,[2 4],,,,3]"ra
{1 {2 4} 3}
```

It should be noted that `,` are optional and multiple `,` will be skipped as well. Nesting is supported. 

```Char a: ``` Returns 1 iff `a` is space, else returns 0.

```shell
blsq ) " \t\ra0"{ra}m[
{1 1 1 0 0}
```

### ReadDouble ```rd```

This built-in auto-maps if the argument given is a Block.

```String a: ``` Converts `a` to Double.

```shell
blsq ) "3.0"rd
3.0
```

```Int a: ``` *Defined as:* *```pd```*. 

```Double a: ``` No operation.

```shell
blsq ) 3.1rd
3.1
```

```Char a: ``` Returns 1 iff `a` is alpha, else returns 0.

```shell
blsq ) 'ard
1
blsq ) '1rd
0
```

**Authors' Notes:** This built-in is useful to convert every element in a Block to a Double:

```shell
blsq ) {3.0 5 "3.14"}rd
{3.0 5.0 3.14}
```

### ReadInt ```ri```

This built-in auto-maps if the argument given is a Block. 

```String a: ``` Converts `a` to Int.

```shell
blsq ) "100"ri
100
blsq ) "-101"ri
-101
```

```Int a: ``` No operation.

```shell
blsq ) 5ri
5
blsq ) -5ri
-5
```

```Double a: ``` *Defined as:* *```av```*. 

```Char a: ``` Returns 1 iff `a` is alpha numeric, else returns 0.

```shell
blsq ) 'ari
1
blsq ) '1ri
1
blsq ) '.ri
0
```

**Authors' Notes:** This built-in is useful to convert every element in a Block to an Integer:

```shell
blsq ) {"12" 12.0 13 12.7}ri
{12 12 13 12}
```

However, Doubles are not rounded to the nearest Integer but are truncated. Rounding everything to the nearest Integer can be done with for example ```rd)R_```. 


### Reduce ```r[```

```Block a, Block f: ``` Takes the first element of `a` and the second element of `a`, applies `f`, takes the next element of `a` and applies `f` again and continues like that. More symbolically speaking
`{1 2 3 4}{.+}r[` becomes `1 2 .+ 3 .+ 4 .+`, `{1 2 3 5}{?-}r[` becomes `1 2 ?- 3 ?- 4?-` and so forth. 

```shell
blsq ) {1 2 3 4}{.+}r[
10
blsq ) {1 2 3 4}{.*}r[
24
```

### Replace ```r~```

```Block a, Any b, Any c: ``` Replaces every occurence of `b` in `a` with `c`.

```shell
blsq ) {1 2 3 1 4}1 9r~
{9 2 3 9 4}
```

```String a, Char b, Char c: ``` Replaces every occurence of `b` in `a` with `c`.

```shell
blsq ) "hello"'l'!r~
"he!!o"
```

```String a, String b, String c: ``` Replaces every occurence of `b` in `a` with `c`.

```shell
blsq ) "hi there hi go""hi""bye"r~
"bye there bye go"
```

```Int a, Int b, Int c: ``` Replaces every occurence of `b` in `a` with `c` (works on absolute values).

```shell
blsq ) -1334336 33 10r~
1104106
```

### ReplaceRegex ```R~```

```String str, String repl, String regex: ``` Replaces every match of `regex` with `repl`. 

```shell
blsq ) "Year 2014.""X""[[:digit:]]"R~
"Year XXXX."
blsq ) "Year 2014.""X""[0-3]"R~
"Year XXX4."
blsq ) "Year 2014.""__""[a-z]|[0-3]{2}"R~
"Y______ __14."
```

### Reverse ```<-```

```String a: ``` Reverses `a`.

```shell
blsq ) "123"<-
"321"
```

```Block a: ``` Reverses `a`.

```shell
blsq ) {4 5 6}<-
{6 5 4}
```

```Int a: ``` Reverses the digits of an Integer. (Works on absolute value).

```shell
blsq ) -123<-
321
```

```Char a: ``` Inverts case.

```shell
blsq ) 'a<-
'A
blsq ) 'B<-
'b
```

### Round ```r_```

This built-in accepts a Block as first argument, in which case an auto-map is performed. 

```Double a, Int b: ``` Rounds `a` to `b` decimal points.

```shell
blsq ) 3.12 2r_
3.12
blsq ) 3.19 2r_
3.19
blsq ) 3.5 0r_
4.0
blsq ) {3.5 3.4}0r_
{4.0 3.0}
```

### Round2 ```R_```

*Defined as:* *```0r_pd```*.

```shell
blsq ) {3.5 3.4}0r_pd
12.0
blsq ) {3.5 3.4}R_
12.0
blsq ) 5.5R_
6
blsq ) 5.3R_
5
blsq ) 5.3 0r_pd
5
```

**Authors' Notes:** Even though `r_` can auto-map this built-in won't do the same *expected* job because `pd` will calculate the product of a Block. You may however use this fact
as a shortcut for example for `{0r_}m[pd`. If you want to round every Double to the nearest Integer in a Block use `)R_`. 

### Signum ```sn```

```Int a: ``` Signum of `a`. (-1 for negative, 1 for positive, 0 for zero).

```shell
blsq ) -6sn6sn0sn
0
1
-1
```

```Double a: ``` Signum of `a`. (-1.0 for negative, 1.0 for positive, 0.0 for zero).

```shell
blsq ) -6.0sn6.0sn0.0sn
0.0
1.0
-1.0
```

```Char a: ``` Returns `1` if `a` is upper case else returns `0`. 

```shell
blsq ) 'Bsn
1
blsq ) 'bsn
0
```

### Smaller ```.<```

```Any a, Any b: ``` Returns 1 if `a < b` else returns 0.

```shell
blsq ) 2 3.<
1
blsq ) 4 3.<
0
blsq ) {1 2 3}{2 2 3}.<
1
```

**Note:** Comparing values with different types may result in unexpected (but determinstic, thus not undefined) behaviour. 

### Sort ```><```

```Block a: ``` Sorts `a` in ascending order.

```shell
blsq ) {5 3 4}><
{3 4 5}
```

```Str a: ``` Sorts `a` in ascending order.

```shell
blsq ) "there"><
"eehrt"
```

```Int a: ``` Sorts `a` in ascending order.

```shell
blsq ) 3241><
1234
blsq ) 32401><
1234
```

**Notes:** Please be aware that this will remove zeroes as numbers don't have leading zeroes.

```Char a: ``` Returns `1` if `a` is a digit, `0` otherwise.

```shell
blsq ) "ab10c")><
{0 0 1 1 0}
```

### SortReverse ```<>```

*Defined as:* *```><<-```*

```shell
blsq ) {5 3 0 9 7 8}<>
{9 8 7 5 3 0}
blsq ) {5 3 0 9 7 8}><<-
{9 8 7 5 3 0}
```

### Split ```;;```

```Block a, Block b: ``` Split `a` on `b`.

```shell
blsq ) {1 2 3 4 2 3 5 7 2 3 8}{2 3};;
{{1} {4} {5 7} {8}}
```

```String a, String b: ``` Split `a` on `b`.

```shell
blsq ) "Hello, world, is,"", ";;
{"Hello" "world" "is,"}
```

```String a, Char b: ``` Split `a` on `b`.

```shell
blsq ) "Hello"'l;;
{"He" "" "o"}
```

```Char a, String b: ``` Split `b` on `a`.

```shell
blsq ) 'e"Hello";;
{"H" "llo"}
```

```Int a, Int b: ``` Split `a` on `b`.

```shell
blsq ) 1234256238 23;;
{1 4256 8}
```

### StripLeft ```S[```

```Block a, Any b: ``` Removes any leading `b`s from `a`.

```shell
blsq ) {1 1 2 5}1S[
{2 5}
```

```String a, Char b: ``` Removes any leading `b`s from `a`.

```shell
blsq ) "QQabQ"'QS[
"abQ"
```

```Int a: ``` Returns (`a` * `a`) (squares).

```shell
blsq ) 5S[
25
```

**Authors' Notes:** For Doubles use `fC`. 

### StripRight ```[S```

```Block a, Any b: ``` Removes any trailing `b`s from `a`.

```shell
blsq ) {1 0 0 0}0[S
{1}
```

```String a, Char b: ``` Removes any trailing `b`s from `a`.

```shell
blsq ) "abccc"'c[S
"ab"
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

### SuffixOf ```!~```

```Block a, Block b: ``` Returns 1 if `b` is a suffix of `a` otherwise returns 0.

```shell
blsq ) {1 2 3}{2 3}!~
1
blsq ) {1 2 3}{1 2}!~
0
```

```String a, String b: ``` Returns 1 if `b` is a suffix of `a` otherwise returns 0.

```shell
blsq ) "this.txt" ".txt"!~
1
blsq ) "this.txt" ".pdf"!~
0
```

```Int a, Int b: ``` Returns 1 if `b` is a suffix of `a` otherwise returns 0 (works on absolute values).

```shell
blsq ) 123 -23!~
1
blsq ) 123 -24!~
0
```

### Sum ```++```

```Block {}: ``` *Defined as:* *```0```*. The sum of an empty Block is zero.

```Block a: ``` *Defined as:* *```{.+}r[```*. Calculates the sum of a Block.

```shell
blsq ) {1 2 3 4}{.+}r[
10
blsq ) {1 2 3 4}++
10
blsq ) {1 2 3 4.0}++
10.0
```

```Int a, Int b: ``` Concatenates Integers (works on absolute values). 

```shell
blsq ) 12 34++
1234
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

### SwapDup ```/^```

*Defined as:* *```\/^^```.

```shell
blsq ) 1 2 3/^
2
2
3
1
blsq ) 1 2 3\/^^
2
2
3
1
blsq ) 1 2 3/^
2
2
3
1
blsq ) 1 2 3jJ
2
2
3
1
```

**Author's Notes**: You might as well use ```jJ```. 

### SwapPop ```/v```

*Defined as:* *```\/vv```*. This builtin essentially pops the element under the top of the stack.

```shell
blsq ) 1 2
2
1
blsq ) 1 2 /v
2
blsq ) 1 2 \/vv
2
blsq ) 1 2 3 \/vv
3
1
```

### Tail ```[-```

```Block a: ``` Returns all but the first element of `a`.

```shell
blsq ) {1 2 3}[-
{2 3}
```

```String a: ``` Returns all but the first character of `a`.

```shell
blsq ) "hello"[-
"ello"
```

```Int a: ``` Returns all but the last digit of `a` (as Integer) (works on absolute value).

```shell
blsq ) 451[-
51
```

```Char a: ``` Convert to string.

```shell
blsq ) 'a[-
"a"
```

### ToLower ```zz```

This built-in auto-maps if an argument provided is a block.

```Char a```: Convert `a` to lower case.

```shell
blsq ) 'Qzz
'q
```

```String a```: Convert `a` to lower case.

```shell
blsq ) "aBc."zz
"abc."
```

### ToUpper ```ZZ```

This built-in auto-maps if an argument provided is a block.

```Char a```: Convert `a` to upper case.

```shell
blsq ) 'aZZ
'A
```

```String a```: Convert `a` to upper case.

```shell
blsq ) "aBc."ZZ
"ABC."
```

### Union ```UN```

**Notes:** If left argument contains duplicates these will be preserved. Duplicates in the right argument will be removed. Order is preserved. 

```Block a, Block b: ``` Union of `a` and `b`. 

```shell
blsq ) {1 1} {1 2 2 2 3}UN
{1 1 2 3}
```

```String a, String b: ``` Union of `a` and `b`.

```shell
blsq ) "zabc""cde"UN
"zabcde"
```

```Int a, Int b: ``` Union of `a` and `b`.

```shell
blsq ) 12 14UN
124
```

### Unlines ```un```

```Block {}: ``` If given an empty block returns an empty string.

```shell
blsq ) {}un
""
```

```Otherwise: ``` *Defined as:* *```"\n"j[[\[```*. This is the *inverse* of `ln` and inserts newlines between elements. 

```shell
blsq ) {"abc" "def" "ghe"}"\n"j[[\[
"abc\ndef\nghe"
blsq ) {"abc" "def" "ghe"}un
"abc\ndef\nghe"
```

**Authors' Notes:** Due to its definition this built-in will only work as expected when the Block contains Strings.
If you want to *unlines* a Block containing other types use `Su`. 

```shell
blsq ) {1 2 3}un
"\n12\n3"
blsq ) {1 2 3}Su
"1\n2\n3"
```

### UnlinesPretty ```uN```

*Defined as:* *```unsh```*.

```shell
blsq ) {"12" "23"}un
"12\n23"
blsq ) {"12" "23"}uN
12
23
blsq ) {"12" "23"}unsh
12
23
```

### Unparse ```up```

```Any a: ``` Converts a to display string. This is somewhat the *inverse* of `ps`.

```shell
blsq ) {1 2++}up
"{1 2 ++}"
```

**Authors' Note:** This built-in is somewhat equivalent to using `3SH`. 

### While ```w!```

A while-loop

```Block f, Block p: ``` Executes `f` while `p` is not zero. `p` will be tested each time against the top of the stack.

```shell
blsq ) 5{+.}{10.<}w!
10
```

```Block f: ``` Executes `f` as long as the top of the stack is not zero. Same thing as doing ```{code}{}w!```.

```shell
blsq ) 0 10{j+.+.j-.}w!
0
20
```

### WithLines ```wl```

*Defined as:* *```jlnjm[un```*. This built-in allows to map over the lines in a String. 

```shell
blsq ) "abc\ndef"{<-}jlnjm[un
"cba\nfed"
blsq ) "abc\ndef"{<-}wl
"cba\nfed"
```


### WithLinesParsePretty ```wL``` 

*Defined as:* *```(ps)+]WL```*. This built-in allows to map over the lines in a String while calling *Parse* on each line automatically.

```shell
blsq ) "11 22\n5 6"{++Sh}(ps)+]WL
33
11
blsq ) "11 22\n5 6"{++Sh}wL
33
11
```

### WithLinesPretty ```WL```

*Defined as:* *```wlsh```*. 

```shell
blsq ) "abc\ndef"{<-}wlsh
cba
fed
blsq ) "abc\ndef"{<-}WL
cba
fed
```

### WithWords ```ww```

*Defined as*: *```jWDjm[wd```*. This built-in allows to map over the words in a String.

```shell
blsq ) "hello world"{<-}jWDjm[wd
"olleh dlrow"
blsq ) "hello world"{<-}ww
"olleh dlrow"
```

### WithWordsPretty ```WW```

*Defined as*: *```wwsh```*.

```shell
blsq ) "hello world"{<-}wwsh
olleh dlrow
blsq ) "hello world"{<-}WW
olleh dlrow
```

### Words ```wd```

```String _: ``` *Defined as:* *```" ";;```*. Splits by space. 

```shell
blsq ) "hi there"wd
{"hi" "there"}
```

```Block {}: ``` Pushes an empty string to the stack.

```shell
blsq ) {}wd
""
```

```Block a: ``` *Defined as*: *```' \/[[\[```*. An *unwords*. It converts a Block of words
into a String.

```shell
blsq ) {"hi" "there"}wd
"hi there"
blsq ) {"hi" "there"}' \/[[\[
"hi there"
```

```Otherwise: ``` *Defined as*: *```\/[[```*. 

```shell
blsq ) {"hi" "there" "you"}'.wd
{"hi" '. "there" '. "you"}
```

**Authors' Notes:** This built-in only splits by a regular space. If you want to split by
whitespaces you need to use ```WD``` (Words2).

```shell
blsq ) "a\nb"wd
{"a\nb"}
blsq ) "a\nb"WD
{"a" "b"}
```

### Words3 ```wD```

*Defined as:* *```wdsh```*. 

```shell
blsq ) {"abc" "def"}wD
abc def
```

### Xor ```$$```

This built-in auto-zips if an argument provided is a Block.

```Int a, Int b: ``` Bitwise XOR.

```shell
blsq ) 66 34$$
96
blsq ) 96 66 $$
34
blsq ) 96 34 $$
66
```

### Zip ```z[```

```Block a, Block b: ``` Zips `a` and `b`. 

```shell
blsq ) {1 2 3}{4 5 6}z[
{{1 4} {2 5} {3 6}}
```

```String a, String b: ``` Zips `a` and `b`.

```shell
blsq ) "abc""def"z[
{{'a 'd} {'b 'e} {'c 'f}}
```

```String a, Block b: ``` Zips `a` and `b`.

```shell
blsq ) "abc"{1 2 3}z[
{{'a 1} {'b 2} {'c 3}}
```

```Block a, String b: ``` Zips `a` and `b`.

```shell
blsq ) {1 2 3}"abc"z[
{{1 'a} {2 'b} {3 'c}}
```

```Int a, Int b```: *Defined as*: *```XX\/XX\/z[```*. (It'll explode the integers).

```shell
blsq ) 123 456z[
{{1 4} {2 5} {3 6}}
```

```Block a, Any b```: *Defined as*: *```\/XX\/z[```*. (It'll explode argument `b`).

```Any a, Block b:```: *Defined as*: *```XXz[```*. (It'll explode argument `a`).

### ZipWith ```Z[```

```Any a, Any b, Block f```: Performs a *Zip* followed by a *Map*. 

### ZipWithPush ```Z]```

*Defined as:* *```(^p+])Z[```*. 

**Authors' Notes:** This is was introduced to the fact that with the usual *ZipWith* the arguments are as a Block on the
stack which is kinda inconvenient in most cases. 