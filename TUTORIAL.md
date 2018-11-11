# Tutorial

Welcome to the re-newed tutorial for *The Burlesque Programming Language*.
This tutorial assumes that you have the Burlesque-Shell installed locally (you can get it at
http://github.com/FMNSSun/Burlesque). If you don't have it installed, don't want to install it or
can't install it there's also an online version available which works in your browser at http://104.167.104.168/~burlesque/burlesque.cgi?q=10ro. 

## What is Burlesque

Burlesque is a stack-based, concatenative, lazy and esoteric programming language. A stack works like a pile of books. You can put a book on top of the pile (this is called a push) or you can take a book away from the pile (this is called a pop). You can also swap the two books at the top (this is called a swap) or even
rotate the entire pile (obviously called a rotation).

## The stack

The interpreter prints out the remaining contents of the stack after it is done executing your program. The order in which it prints out the elements is from top to bottom. If we push the numbers ```1, 2``` and ```3``` the number ```1``` will be at the bottom and the number ```3``` at the top meaning the ```3``` will be printed first.

```
blsq ) 1 2 3
3
2
1
```

Let's look at what happens if we duplicate, pop or swap numbers. The pop instruction is ```vv```, the swap instruction is `j` and the duplicate instruction is ```J```:

```
blsq ) 1 2 3vv
2
1
blsq ) 1 2 3j
2
3
1
blsq ) 1 2 3J
3
3
2
1
```

These are the most basic instructions and be sure to understand completely what they do. ```v``` removes the element at the top, ```j``` swaps the top two elements and ```J``` duplicates the element at the top. 
The more advanced instructions for stack manipulation are ```x/```, ```#r``` and ```#R```. ```x/``` rotates the top three elements and ```#r``` (rotates right) and ```#R``` (rotates left) rotate the whole stack:

```
blsq ) 1 2 3 4x/
2
4
3
1
blsq ) 1 2 3 4#r
3
2
1
4
blsq ) 1 2 3 4#R
1
4
3
2
```

## Literals

A literal is a fixed value in the source code. Burlesque supports the data types Integer, Double (floating point), String, Block and Identifier. 

An Integer is for example ```5```, a Double is for example ```3.14159```, a String is for example ```"Hello, world!"```, a Block is just a list of elements for example ```{1 2.0 {1 2 3}}``` and ```(vv)``` is an Identifier. If you write ```vv``` the interpreter will execute the instruction, if you write ```(vv)``` it will push the instruction to the stack rather than interpreting it.

A note about terminology: Block and List as well as Instruction and Built-In are used interchangeably. 

```
blsq ) vv
ERROR: Burlesque: (vv) Stack size error!
blsq ) (vv)
vv
```

## Arithmetic

We can of course do arithmetic. The most basic arithmetic instructions are ```?+``` (addition), ```?-``` (subtraction), ```?/``` (division) and ```?*``` multiplication:

```
blsq ) 1.0 3?+
4.0
blsq ) 1.5 3?*
4.5
blsq ) 5 3?-
2
blsq ) 10 3?/
3
blsq ) 10 3.0?/
3.3333333333333335
```

The result of an arithmetic instruction on two Integers will be an Integer (that's why ```10 3?/``` yields ```3```). If one operand is a Double, the other will be promoted to a Double.

If you are familiar with other programming languages you will notice that the ```?+``` is at the end and not between two numbers. In concatenative/stack-based languages postfix notation is used. This means you write the operands first then followed by the instruction. Internally instructions will just pop the arguments (operands) from the stack. To calculate ```3*(2-5)``` we have to write ```3 2 5 ?- ?*```.

## Comparisons

Burlesque treats ```0``` as false end everything else as true. To test if something is smaller we use ```.<```, to test if something is larger we use ```.>``` and to test equality we use ```==```:

```
blsq ) 5 3.>
1
blsq ) 5 3.<
0
blsq ) 5 3==
0
blsq ) 5 5==
1
```

## Blocks

Blocks are very important in Burlesque. A Block is just a list of elements. You can also nest Blocks.
Some instructions will just magically work on Blocks as well:

```
blsq ) {1 2 3}{4 5 6}?*
{4 10 18}
blsq ) {1 2 3}1?+
{2 3 4}
```

Additionally we can do a lot with Blocks. We can reverse them (```<-```), remove duplicates (```NB```),
sort them (```<>```), group elements together (```=[```) and much more:

```
blsq ) {1 2 3 3 4}<-
{4 3 3 2 1}
blsq ) {1 2 3 3 4}NB
{1 2 3 4}
blsq ) {1 2 3 9 3 4}<>
{9 4 3 3 2 1}
blsq ) {1 2 3 3 4}=[
{{1} {2} {3 3} {4}}
```

There are probably hundreds of instructions that work on Blocks, so be sure to read the language reference and memorize them :).

## Codeblocks

Blocks are also code. 

```
blsq ) {1 2?+}
{1 2 ?+}
```

We can evaluate a block with the eval instruction (```e!```):

```
blsq ) {1 2?+}e!
3
```

Now we are going to introduce four very important concepts: Mapping, Zipping, Reducing and Filtering.

### Mapping

Mapping means applying a function (a Block) to every element in a Block. The map instruction is ```m[```:

```
blsq ) {1 2 3 4 5}{3.>}m[
{0 0 0 1 1}
blsq ) {1 2 3 4 5}{J?*}m[
{1 4 9 16 25}
```

The first example applies ```3.>``` to every element in the Block and the second one ```J?*```. ```J?*``` takes a number, duplicates it and performs a multiplication thus calculating the square of a number.

### Zipping

Zipping (```z[```) combines two Blocks into one (this is done pairwise):

```
blsq ) {1 2 3}{4 5 6}z[
{{1 4} {2 5} {3 6}}
```

The combination of mapping and zipping is called ZipWith (```Z]```):

```
blsq ) {1 2 3}{4 5 6}{?*}Z]
{4 10 18}
```

This code multiplicates the elements in the Blocks pairwise. However, some instructions can do that automatically:

```
blsq ) {1 2 3}{4 5 6}?*
{4 10 18}
```

### Reducing

Reducing is probably the most complicated of the magic four. The Reduce instruction is ```r[```:

```
blsq ) {1 2 3 4 5}{?*}r[
120
blsq ) {1 2 3 4 5}{?+}r[
15
```

The first example computes the product of the Block, the second example the sum of the Block.
I will try to explain through Psudo-Code to you what Reduce does. Let's say we have a Block with
the elements ```{a b c d e}``` then Reduce with ```?+``` will compute ```a b?+c?+d?+e?+```.
A Reduce with ```?*``` will compute ```a b?*c?*d?*e```. 

### Filtering

Filtering is easy. Filtering removes every element from a Block that doesn't meet a certain condition.

```
blsq ) {1 2 3 4 5 6}{3.>}f[
{4 5 6}
```

This returns all numbers greater than ```3``` in the given Block.

## If

If we want to execute a Block based on a condition we can use ```if```. Let's say we want to pop the top element if and only if it is a ```5```.
This can be done with ```J5=={vv}if```. We need to duplicate the number with ```J``` because ```=``` will pop two elements from the stack so our original number
would be lost if we didn't duplicate it:


```
blsq ) 4 5==
0
blsq ) 4 J5==
0
4
blsq ) 5 J5==
1
5


blsq ) 5J5=={vv}if
blsq ) 4J5=={vv}if
4
```

## Strings

Strings will be printed with quotes by default. If you don't want that you need to use the ```Q``` instruction:

```
blsq ) "Hello, world!"
"Hello, world!"
blsq ) "Hello, world!"Q
Hello, world!
```

Strings can contain raw bytes as well. If you want to use a ```"``` within a String you need to escape it with ```\'```, also if you want a ```\``` in your String you need to escape it with
``` \\ ```:


```
blsq ) "Hi \' there"Q
Hi " there
```

```
blsq ) "Hi \\' there"Q
Hi \' there
```


