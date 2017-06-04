# Building a Brainfuck Interpreter in Haskell

## Purpose

This document describes how to build an interpreter for the Brainfuck language using Haskell. We shall call the interpreter _Brainfunc_. This document describes the components of the interpreter and also provides examples of those components. The goals of this document are to 1) assist readers to build a small program in Haskell and 2) expose readers to design considerations in functional programming.

## Contents

* [How to Use this Material](#how-to-use-this-material)
* [Brainfuck and Brainfunc](#brainfuck-and-brainfunc)
* [Preliminaries](#preliminaries)
* [Building Brainfunc](#building-brainfunc)
    * [Building a Zipper](#building-a-zipper)
    * [Representing Brainfuck Programs](#representing-brainfuck-programs)
    * [Representing an Infinite Tape](#representing-an-infinite-tape)
    * [Building a P'' Interpreter](#building-a-p-interpreter)
    * [Building a Brainfuck Interpreter](#building-a-brainfuck-interpreter)
    * [Completing Brainfunc](#completing-brainfunc)
    * [Extending Brainfunc](#extending-brainfunc)
    * [Wrap-up](#wrap-up)
* [Worked Examples](#worked-examples)
    * [`Zipper` Functions](#zipper-functions)
    * [`Code` Functions](#code-functions)
    * [`Tape` Functions](#tape-functions)
    * [P'' Functions](#p-functions)
    * [Brainfuck Functions](#brainfuck-functions)
    * [Completion Functions](#completion-functions)

## How to Use this Material

This material consists of two parts: descriptions of the functions that comprise Brainfunc, and example implementations of those functions. Each section in the first part of the document provides a brief overview of the concepts that will be covered in that section, then describes the functionality required to implement those concepts. The functions are presented in approximate order of their complexity, therefore it is suggested that you work through them in the presented order. In each case, first write down the type of the function, then address the base cases (where certain input values directly correspond to certain output values), then complete the implementation. Ensure your function has a defined result for all input values.

Although worked examples are presented in the second part of this document, it is recommended to defer looking at them for as long as possible (ideally until after you have completed your interpreter). If stuck, move on to the next function and come back to the skipped function before moving to the next section (no function's implementation depends on another function in the same section). Also discuss the functions and their implementation with fellow Haskellers as an alternative to referring to the worked examples. The motivation for this suggestion is that the process of writing Haskell code involves exploring the structure of types and the relationships between them, which is somewhat undermined by referring to complete examples. Often by 'trying everything' you will not only improve your understanding of types and functions but also stumble across the answer and can work back from there.

If you find that you have no use for the worked examples, it may still be worth viewing them since each is accompanied by a short discussion about how the component was designed and what implications that design has for the structure of the program.

## Brainfuck and Brainfunc

In 1936, Alan Turing described a model of effective computability that has since become known as the _Turing Machine_. This work was part of a larger research effort that demonstrated that a function is computable if there exists an algorithm that produces the same output as the function for all of the function's inputs. The Turing Machine is a model for constructing algorithms, which it achieves using the following:

* An infinite _tape_ that consists of discrete _cells_ that contain symbols.
* A _head_ that can move between cells and read from or write symbols to them.
* A _state register_ that stores the state of the Machine.
* A _table_ that consists of a finite number of instructions.

The Turing Machine informed the development of a conceptual architecture for computing machines, now known as the _von Neumann Architecture_ (after John von Neumann), on which all modern general purpose computers are based. Broadly, the Machine's table corresponds to stored programs, the state register to the program runtime, the tape to computer memory and the head to the central processing unit.

In 1964, Corrado Böhm developed the P'' programming language to describe a subset of the variants of the Turing Machine model. In 1992, Urban Müller adapted and extended P'' to incorporate input and output operations and named the resultant language _Brainfuck_. The Brainfuck language defines a set of instructions that can be performed on a conceptual tape, in which each cell contains an integer value, by a conceptual head:

* `+`: Increments the value in the cell under the head.
* `-`: Decrements the value in the cell under the head.
* `>`: Moves the head to the next cell.
* `<`: Moves the head to the previous cell.
* `[`: Denotes the beginning of a loop. If the value in the cell under the head is zero, then the instructions within the loop are skipped. The instruction does not affect the tape.
* `]`: Denotes the end of a loop. If the value in the cell under the head is non-zero, then the instructions within the loop it closes are repeated. The instruction does not affect the tape.
* `.`: Reads a value from the cell under the head and prints it to the terminal.
* `,`: Writes a value entered by the user at the terminal to the cell under the head.

Interpreters of the Brainfuck language may vary in terms of the number of cells on the tape and the maximum and minimum values that a cell can contain. In the most liberal interpretation, the tape is infinite (finite only in memory) and each cell can contain any integer. Each cell initially contains a value of zero.

In 1932, prior to the discovery of the Turing Machine, Alonso Church discovered the _Lambda Calculus_. Lambda Calculus consists of three components: terms, abstractions, and expressions in which an abstraction is applied to a term. When first describing the Turing Machine, Turing demonstrated that the Machine and Lambda Calculus are equivalent. However, the two models begat very different paradigms of programming: the instruction-oriented Turing Machine influenced the creation of languages that consist of instructions that modify shared state - the imperative paradigm - whereas the expression-oriented Lambda Calculus enabled the creation of languages that define programs as expressions - the functional paradigm. The purpose of this material is to guide you to create an interpreter for Brainfuck, an imperative language, in Haskell, a functional language. It is a brain<i>functional</i> interpreter. _Brainfunc_, if you will.

## Preliminaries

### Knowledge Required to Create Brainfunc

Brainfunc depends on Haskell's `List`, `Maybe`, `Tuple` and `IO` types, and also requires limited use of Haskell's `error` operation. The IO aspects of Brainfunc will be explained as they appear. In this section, the required knowledge of `List`, `Maybe`, `Tuple` and `error` is detailed.

###  `List`

The `List` type has the following structure:

```
data [a] = [] | a : [a]
```

Thus a `List` value is either `[]`, meaning that it is empty, or it has a 'head' element `a` followed by a 'tail' `[a]`, which is another list. In the latter case, we say `a` is 'cons'ed onto `[a]` by the 'cons' operator `:`. The type variable `a` indicates that a list can be created for any type, with the constraint that the list only contains values of that type. The following are all `List` values:

```
2:1:[]         :: [Integer] -- 2:1:[] can be expressed as [1,2]
'c':'a':'t':[] :: [Char]    -- 'c':'a':'t':[] can be expressed as "cat"
[]             :: [a]       -- The type of a is determined from the
                            -- context in which the value is used
```

We can exploit the structure of `List` to traverse a sequence of values. For example:

```
length :: [a] -> Integer
length []     = 0               -- Case: []
length (x:xs) = 1 + (length xs) -- Case: a : [a]
```

### `Maybe`

The `Maybe` type has the following structure:

```
data Maybe a = Nothing | Just a
```

Thus a `Maybe` value is either `Just a`, representing that a value of type `a` is present, or `Nothing`, indicating that no value of that type is available. `Maybe` is used to represent the result of operations that may fail. The following are all `Maybe` values:

```
Just 5     :: Maybe Integer
Just "cat" :: Maybe [Char]
Nothing    :: Maybe a       -- The type of a is determined from the
                            -- context in which the value is used
```

We can exploit the structure of `Maybe` to conditionally perform actions. For example:

```
isJust :: Maybe a -> Bool
isJust Nothing  = False -- Case: Nothing
isJust (Just _) = True  -- Case: Just a
```

### `Tuple`

The type `Tuple` has the following structure:

```
data (,) a b = (,) a b
```

`Tuple` is used to pair values together, which is useful for both indicating that two values are related to each other and for returning more than one value from a function. The type variables `a` and `b` in the definition of `Tuple` indicate that the two values in a `Tuple` can be of different types. The following are all `Tuple` values:

```
(1, 2)            :: (Integer, Integer)
("cat", 5)        :: ([Char], Integer)
(Just 5, [1,2,3]) :: (Maybe Integer, [Integer])
```

We can exploit the structure of `Tuple` to obtain the first and second values. For example:

```
fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y
```

### `error`

Haskell contains an `error` function that enables the execution of a program to be aborted and a textual message to be printed for the user. `error` is intended to be used in only two circumstances: 1) when truly exceptional cases occur from which it is impossible to recover and 2) when the program enters an invalid state due to programmer error. In the latter case, `error` can indicate to other developers that, should the program encounter the `error` case, a logical error has occurred in the construction of the program. There is at least one case in Brainfunc in which `error` is useful for signalling that a case that was intended to be unreachable has been reached. Keep this in mind as you build Brainfunc and use `error` (only) where appropriate.

## Building Brainfunc

### Building a Zipper

#### Defining `Zipper`

`List` is defined as a value followed by a `List` of values. We can exploit this structure to move rightwards through a `List` value using recursion. However, the structure of `List` is too limited to allow us to move leftwards through its elements. We require a `List` that has a 'tail' in both the left and right directions. This is the motivation for the `Zipper` type, which has the following structure:

```
data Zipper a = Zip | Zipper [a] a [a]
```

A value of type `Zipper` is either a `Zip`, which is a `Zipper` that contains no elements, or it is a cursor element of type `a` with `List`s of elements, also of type `a`, to both its left and right. The following are all `Zipper` values:

```
Zipper [3,2,1] 4 [5,6,7]                   :: Zipper Integer
Zipper "ac" 't' "nip"                      :: Zipper Char
Zipper ["dog", "bird"] "cat" ["fish"]      :: Zipper [Char]
Zipper [Nothing] (Just 5) [Just 3, Just 1] :: Zipper (Just Integer)
```

If there are elements in the lists to the left and right of the cursor, then we can shift the cursor left and right by swapping the cursor with the heads of those `List`s:

```
next (Zipper [3,2,1] 4 [5,6,7]) = Zipper [4,3,2,1] 5 [6,7]
prev (Zipper [3,2,1] 4 [5,6,7]) = Zipper [2,1] 3 [4,5,6,7]
```

Thus `Zipper` enables us to move backwards and forwards through a sequence of values.

#### Required Functions

With a definition of `Zipper`, we can define various functions that allow us to modify values of that type. There are many functions that are useful for manipulating `Zipper` values, but only the following functions are required to create Brainfunc:

* `zipper`: Allows us to create a `Zipper` from a single value.
* `zipperFromList`: Allows us to create a `Zipper` from a `List`.
* `getCursor`: Allows us to safely retrieve the cursor element of a `Zipper`.
* `setCursor`: Allows us to set a `Zipper`'s cursor element.
* `next`: Allows us to shift a `Zipper`'s cursor one position to the right.
* `prev`: Allows us to shift a `Zipper`'s cursor one position to the left.
* `insertNext`: Allows us to insert a value one position to the right of a `Zipper`'s cursor and shift its cursor to that value.
* `insertPrev`: Allows us to insert a value one position to the left of a `Zipper`'s cursor and shift its cursor to that value.
* `nextCursor`: Allows us to obtain a `Zipper` shifted one position to the right and the cursor of that shifted `Zipper` in one operation.
* `prevCursor`: Allows us to obtain a `Zipper` shifted one position to the left and the cursor of that shifted `Zipper` in one operation.

#### `zipper`

Implement the `zipper` function, which returns a `Zipper` in which the cursor is the argument value. Examples:

```
zipper 5          == Zipper [] 5 []
zipper 'c'        == Zipper "" 'c' ""
zipper (Just [1]) == Zipper [] (Just [1]) []
```

#### `zipperFromList`

Implement the `zipperFromList` function, which returns a `Zipper` representation of the argument `List`. Examples:

```
zipperFromList [1,2,3,4,5] == Zipper [] 1 [2,3,4,5]
zipperFromList "catnip"    == Zipper "" 'c' "atnip"
zipperFromList []          == Zip
```

#### `getCursor`

Implement the `getCursor` function, which returns the cursor of the given `Zipper` if the `Zipper` has a cursor. Examples:

```
getCursor (Zipper [3,2,1] 4 [5,6,7]) == Just 4
getCursor (Zipper "ac" 't' "nip")    == Just 't'
getCursor Zip                        == Nothing
```

#### `setCursor`

Implement the `setCursor` function, which replaces the cursor of a given `Zipper` with a given value. Examples:

```
setCursor 9 (Zipper [3,2,1] 4 [5,6,7]) == Zipper [3,2,1] 9 [5,6,7]
setCursor '?' (Zipper "ac" 't' "nip")  == Zipper "ac" '?' "nip"
setCursor 1 Zip                        == Zipper [] 1 []
```

#### `next`

Implement the `next` function, which returns the given `Zipper` shifted one position to the right if the shift is possible. Examples:

```
next (Zipper [3,2,1] 4 [5,6,7]) == Just (Zipper [4,3,2,1] 5 [6,7])
next (Zipper "ac" 't' "nip")    == Just (Zipper "tac" 'n' "ip")
next Zip                        == Nothing
```

#### `prev`

Implement the `prev` function, which returns the given `Zipper` shifted one position to the left if the shift is possible. Examples:

```
prev (Zipper [3,2,1] 4 [5,6,7]) == Just (Zipper [2,1] 3 [4,5,6,7])
prev (Zipper "ac" 't' "nip")    == Just (Zipper "c" 'a' "tnip")
prev Zip                        == Nothing
```

#### `insertNext`

Implement the `insertNext` function, which inserts a given value to the right of a given `Zipper`'s cursor and returns that `Zipper` shifted so that the inserted value is the cursor. Examples:

```
insertNext 9 (Zipper [3,2,1] 4 [5,6,7]) == Zipper [4,3,2,1] 9 [5,6,7]
insertNext '?' (Zipper "ac" 't' "nip")  == Zipper "tac" '?' "nip"
insertNext 9 Zip                        == Zipper [] 9 []
```

#### `insertPrev`

Implement the `insertPrev` function, which inserts a given value to the left of a given `Zipper`'s cursor and returns that `Zipper` shifted so that the inserted value is the cursor. Examples:

```
insertPrev 9 (Zipper [3,2,1] 4 [5,6,7]) == Zipper [3,2,1] 9 [4,5,6,7]
insertPrev '?' (Zipper "ac" 't' "nip")  == Zipper "ac" '?' "tnip"
insertPrev 9 Zip                        == Zipper [] 9 []
```

#### `nextCursor`

Implement the `nextCursor` function, which returns a cursor of the given `Zipper` shifted one element to the right paired with the shifted `Zipper`, if shifting is possible. Examples:

```
nextCursor (Zipper [3,2,1] 4 [5,6,7])  == Just (5, Zipper [4,3,2,1] 5 [6,7])
nextCursor (Zipper [6,5,4,3,2,1] 7 []) == Nothing
nextCursor Zip                         == Nothing
```

#### `prevCursor`

Implement the `prevCursor` function, which returns a cursor of the given `Zipper` shifted one element to the left paired with the shifted `Zipper`, if shifting is possible. Examples:

```
prevCursor (Zipper [3,2,1] 4 [5,6,7])  == Just (3, Zipper [2,1] 3 [4,5,6,7])
prevCursor (Zipper [] 1 [2,3,4,5,6,7]) == Nothing
prevCursor Zip                         == Nothing
```

### Representing Brainfuck Programs

#### Encoding Brainfuck Instructions

To represent a Brainfuck program, it is necessary to represent the program's instructions. This can be achieved using a sum type:

```
data Instruction = Incr -- Increment (+)
                 | Decr -- Decrement (-)
                 | Next -- Next cell (>)
                 | Prev -- Previous cell (<)
                 | Open -- Open loop ([)
                 | Loop -- Close loop (])
                 | Read -- Read cell (.)
                 | Wrte -- Write to cell (,)
```

Observe that each of the data constructors of `Instruction` corresponds to one of the instructions of Brainfuck. (For the sake of brevity, `Instruction` values will occasionally be represented using only their first letter.) A Brainfuck program is a sequence of `Instruction`s. To implement loops, the structure that represents the sequence must enable forward and backward iteration. The `Zipper` is one such structure and allows us to define Brainfuck programs as follows:

```
type Code = Zipper Instruction
```

This makes `Code` an alias of `Zipper Instruction`, increasing the readability of our Haskell program while also allowing us apply any function that takes a `Zipper a` to values of type `Code`. The cursor of a value of type `Code` is the next instruction to be executed in the Brainfuck program.

#### Required Functions

After each `Instruction` is processed, the program's sequence of instructions, `Code`, has to be shifted one position to the right. However, the `Open` and `Loop` instructions may require the instruction sequence to be shifted to the corresponding instruction, shifting from `Open` to `Loop` or `Loop` to `Open` depending on whether the loop is to be entered or repeated, respectively. Therefore we require three functions to process values of the `Code` type:

* `nextInstruction`: Allows us to shift to the next Brainfuck instruction.
* `findMatchingLoop`: Allows us to skip the instructions within a loop.
* `findMatchingOpen`: Allows us to repeat the instructions within a loop.

#### `nextInstruction`

Implement the `nextInstruction` function, which returns a cursor of the given `Code` shifted one element to the right paired with the shifted `Code`, if shifting is possible. Examples:

```
nextInstruction (Zipper [Incr,Next] Incr [Prev])
    == Just (Prev, Zipper [Incr,Incr,Next] Prev [])
nextInstruction (Zipper [Incr,Incr,Next] Prev [])
    == Nothing
nextInstruction Zip
    == Nothing
```

#### `findMatchingLoop`

Implement the `findMatchingLoop` function, which takes a `Code` and returns that `Code` shifted so that the cursor element is the corresponding `Loop` instruction, if one exists. Examples:

```
findMatchingLoop (Zipper [] Open [Open,Loop,Loop])
    == Just (Zipper [Loop,Open,Open] Loop [])
findMatchingLoop (Zipper [] Open [Loop])
    == Just (Zipper [Open] Loop [])
findMatchingLoop (Zipper [Open] Loop [])
    == Nothing
findMatchingLoop (Zipper [] Open [])
    == Nothing
findMatchingLoop (Zipper [] Read [])
    == Nothing
```

**Suggestion:** First implement `findMatchingLoop` for non-nested loops. After completing the interpreter, revise `findMatchingLoop` so that it can find the matching `Loop` instruction for `Open` regardless of loop nesting.

#### `findMatchingOpen`

Implement the `findMatchingOpen` function, which takes a `Code` and returns that `Code` shifted so that the cursor element is the corresponding `Open` instruction, if one exists. Example:

```
findMatchingOpen (Zipper [Loop,Open,Open] Loop [])
    == Just (Zipper [] Open [Open,Loop,Loop])
findMatchingOpen (Zipper [Open] Loop [])
    == Just (Zipper [] Open [Loop])
findMatchingOpen (Zipper [] Open [Loop])
    == Nothing
findMatchingOpen (Zipper [] Loop [])
    == Nothing
findMatchingOpen (Zipper [] Incr [])
    == Nothing
```

**Suggestion:** First implement `findMatchingOpen` for non-nested loops. After completing the interpreter, revise `findMatchingOpen` so that it can find the matching `Open` instruction for `Loop` regardless of loop nesting.

### Representing an Infinite Tape

#### Simulating Infinite Cells

To implement our interpreter, we must simulate an infinite tape and support the actions that may be performed on that tape by Brainfuck programs. The `Zipper` type closely resembles the conceptual tape of Turing's Machine. Its cursor can be used to represent the cell under the head of the Turing Machine, and the cursor can have an indefinite number of elements to its left and right. However, Brainfuck constrains the way in which we can modify the value in a cell, and our `Zipper` values are not infinite. We must create a collection of functions that operate on `Zipper` values and modify them in such a way that: 1) the bounded nature of the `Zipper` type is not apparent, and 2) that the instructions of Brainfuck programs are supported. To assist the readability of our code, `Zipper Integer` will be aliased thusly:

```
type Tape = Zipper Integer
```

Note that this allows us to apply any function that has a `Zipper a` argument to any value of type `Tape`.

#### Required Functions

Six functions are required to simulate an infinite tape for the purposes of Brainfunc. They correspond to the actions that may be performed on a tape by the instructions of the Brainfuck language:

* `incrCell`: Allows us to increase the value in the current cell by one.
* `decrCell`: Allows us to decrease the value in the current cell by one.
* `nextCell`: Allows us to move to the next cell on the tape.
* `prevCell`: Allows us to move to the previous cell on the tape.
* `readCell`: Allows us to read the value at the current cell.
* `wrteCell`: Allows us to write a value to the current cell.

#### `incrCell`

Implement the `incrCell` function, which returns a `Tape` that is identical to the given `Tape` except that the cursor's value is incremented by one. Example:

```
incrCell (Zipper [3,2,1] 5 [7,8,9]) == Zipper [3,2,1] 6 [7,8,9]
```

#### `decrCell`

Implement the `decrCell` function, which returns a `Tape` that is identical to the given `Tape` except that the cursor's value is decremented by one. Example:

```
decrCell (Zipper [3,2,1] 5 [7,8,9]) == Zipper [3,2,1] 4 [7,8,9]
```

#### `nextCell`

Implement the `nextCell` function, which returns a `Tape` identical to the given `Tape` except that the cursor is shifted one position to the right. Examples:

```
nextCell (Zipper [3,2,1] 4 [5,6,7]) == Zipper [4,3,2,1] 5 [6,7]
nextCell (Zipper [3,2,1] 4 [])      == Zipper [4,3,2,1] 0 []
nextCell Zip                        == Zipper [] 0 []
```

#### `prevCell`

Implement the `nextCell` function, which returns a `Tape` identical to the given `Tape` except that the cursor is shifted one position to the left. Examples:

```
prevCell (Zipper [3,2,1] 4 [5,6,7]) == Zipper [2,1] 3 [4,5,6,7]
prevCell (Zipper [] 4 [5,6,7])      == Zipper [] 0 [4,5,6,7]
prevCell Zip                        == Zipper [] 0 []
```

#### `readCell`

Implement the `readCell` function, which returns the cursor element of the given `Tape`. Examples:

```
readCell (Zipper [3,2,1] 4 [5,6,7]) == 4
```

#### `wrteCell`

Implement the `wrteCell` function, which replaces the cursor element of the given `Tape` with the given `Integer` value. Example:

```
wrteCell 4 (Zipper [3,2,1] 9 [5,6,7]) == Zipper [3,2,1] 4 [5,6,7]
```

### Building a P'' Interpreter

#### From `Zipper` to Interpreter

The functions developed thus far enable us to interact with `Zipper` values and treat `Zipper`s like a sequence of Brainfuck instructions or as an seemingly infinite sequence of cells. The next step is to implement the functionality that modifies our code and tape values according to the Brainfuck instruction being processed, and then repeats that functionality for every instruction in the code. This is the stage where we encode the behaviour of the non-IO aspects of Brainfuck into our Haskell program, creating an interpreter for a language equivalent to P''.

#### Required Functions

To implement our Brainfuck interpreter, we require two groups of functions, 1) functions that apply the effects of a Brainfuck instruction to the code and tape, and 2) functions that extract instructions from the code to determine which 'instruction functions' to apply:

* `executeIncr`: Allows us to apply the `+` Brainfuck instruction to a code and tape pair.
* `executeDecr`: Allows us to apply the `-` Brainfuck instruction to a code and tape pair.
* `executeNext`: Allows us to apply the `>` Brainfuck instruction to a code and tape pair.
* `executePrev`: Allows us to apply the `<` Brainfuck instruction to a code and tape pair.
* `executeOpen`: Allows us to apply the `[` Brainfuck instruction to a code and tape pair.
* `executeLoop`: Allows us to apply the `]` Brainfuck instruction to a code and tape pair.
* `executeInstruction`: Allows us to execute a single non-IO instruction on the tape.
* `executeCode`: Allows us to execute a sequence of non-IO instructions on the tape.

In the next section, we will extend this collection of functions to support Brainfuck's IO instructions.

#### `executeIncr`

Implement the `executeIncr` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Incr` instruction. Example:

```
executeIncr (Zipper [Incr] Incr [Next,Wrte], Zipper [] 1 []) ==
    (Zipper [Incr] Incr [Next,Wrte], Zipper [] 2 [])
```

#### `executeDecr`

Implement the `executeDecr` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Decr` instruction. Example:

```
executeDecr (Zipper [Incr] Decr [Decr,Decr], Zipper [] 1 []) ==
    (Zipper [Inrc] Decr [Decr,Decr], Zipper [] 0 [])
```

#### `executeNext`

Implement the `executeNext` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Next` instruction. Example:

```
executeNext (Zipper [Incr] Next [Decr,Read], Zipper [] 1 []) ==
    (Zipper [Incr] Next [Decr,Read], Zipper [1] 0 [])
```

#### `executePrev`

Implement the `executePrev` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Prev` instruction. Example:

```
executePrev (Zipper [Incr] Prev [Decr,Read], Zipper [] 1 []) ==
    (Zipper [Incr] Prev [Decr,Read], Zipper [] 0 [1])
```

#### `executeOpen`

Implement the `executeOpen` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Open` instruction. Examples:

```
executeOpen (Zipper [Incr] Open [Loop], Zipper [] 1 []) ==
    (Zipper [Incr] Open [Loop], Zipper [] 1 [])
executeOpen (Zipper [] Open [Decr,Loop], Zipper [] 0 []) ==
    (Zipper [Decr,Open] Loop [], Zipper [] 0 [])
```

#### `executeLoop`

Implement the `executeLoop` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Loop` instruction. Examples:

```
executeLoop (Zipper [Open,Incr] Loop [], Zipper [] 1 []) ==
    (Zipper [Incr] Open [Loop], Zipper [] 1 [])
executeLoop (Zipper [Open] Loop [], Zipper [] 0 []) ==
    (Zipper [Open] Loop [], Zipper [] 0 [])
```

#### `executeInstruction`

Implement the `executeInstruction` function, which receives an `Instruction` and paired `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Instruction`. If the provided `Instruction` is an IO instruction (`Read` or `Wrte`) the function should error. Examples:

```
executeInstruction Incr (Zipper [Next,Incr] Incr [Prev], Zipper [1] 0 []) ==
    (Zipper [Next,Incr] Incr [Prev], Zipper [1] 1 [])
executeInstruction Open (Zipper [] Open [Incr,Incr,Loop], Zipper [] 0 []) ==
    (Zipper [Incr,Incr,Open] Loop [], Zipper [] 0 [])
executeInstruction Open (Zipper [Incr] Open [Incr,Loop], Zipper [] 1 []) ==
    (Zipper [Incr] Open [Incr,Loop], Zipper [] 1 [])
executeInstruction Loop (Zipper [Decr,Open,Incr] Loop [], Zipper [] 0 []) ==
    (Zipper [Decr,Open,Incr] Loop [], Zipper [] 0 [])
executeInstruction Loop (Zipper [Open,Incr] Loop [Incr], Zipper [] 1 []) ==
    (Zipper [Incr] Open [Loop,Incr], Zipper [] 1 [])
executeInstruction Read (Zipper [] Read [], Zipper [] 0 []) ==
    error "Unsupported instruction!"
executeInstruction Wrte (Zipper [Incr] Wrte [], Zipper [] 1 []) ==
    error "Unsupported instruction!"
```

#### `executeCode`

Implement the `executeCode` function, which returns the `Tape` created by executing each `Instruction` in the given `Code` on the given `Tape`. Examples:

```
executeCode (Zipper [] Next [Incr,Incr,Next,Decr]) (Zipper [] 0 []) ==
    Zipper [2,0] (-1) []
executeCode Zip (Zipper [] 0 [])
    == Zipper [] 0 []
executeCode Zip Zip
    == Zipper [] 0 []
```

### Building a Brainfuck Interpreter

#### IO in Brainfunc

To implement the `Read` and `Wrte` instructions of the Brainfuck language it is necessary to perform IO actions. In Haskell, all IO operations occur within the `IO` context. The intention of this section is to provide an intuition for understanding this notion of 'context' and how values that are within a context can be modified using a syntactical structure called `do` notation. This is required knowledge for performing IO actions in Brainfunc.

#### A Notion of Context

Types like `List` and `Maybe` can be interpreted as types that define a context for other types, changing how values of that type are manipulated. `List` takes `a` and represents a context where many values of `a` may exist. `Maybe` takes `a` and represents a context that a value of type `a` may or may not be present. Often a collection of functions must be used and each returns a value that lies within a context. Handling the context while manipulating the result values can be cumbersome. Consider, for example, a function `unSquare` that returns the integer square root of its argument (if any such number exists) and a function `naturalLog` that returns the natural logarithm of an integer:

```
unSquare :: Integer -> Maybe Integer
naturalLog :: Integer -> Maybe Double
```

Note that for certain inputs both functions do not define a result, therefore the return values of both must exist in the `Maybe` context. To apply `naturalLog` to the result of `unSquare` it is necessary to first pattern match on the result of `unSquare` to handle the `Nothing` case:

```
unSquareAndLogn :: Integer -> Maybe Double
unSquareAndLogn x = case unSquare x of
    Just x' -> naturalLog x'
    Nothing -> Nothing
```

The pattern matching syntax scales poorly as the number of expressions that evaluate to a value within a context increases.

#### Using `do` Notation

Haskell provides a special syntax, called `do` notation, that enables values that are in a context to be extracted from the context without requiring pattern matching. For example, `do` notation allows us to implement `unSquareAndLogn` as follows:

```
unSquareAndLogn :: Integer -> Maybe Double
unSquareAndLogn x = do
    x' <- unSquare x -- 1. Extracts x' from the Maybe Integer that is returned
                     --    by unSquare
    naturalLog x'    -- 2. Applies the naturalLog function to the extracted
                     --    Integer x', resulting in a Maybe Double
```

The `<-` syntax extracts the value from the context to its right, giving it the name of the term on its left (see 1.). The `<name> <-` syntax is optional and only required if the contextual value returned by the expression on the right is needed (see 2.). All expressions in a `do` block must evaluate to a type in the same context - e.g. `unSquare` and `naturalLog` both return `Maybe`s - but that the 'wrapped' type can vary - e.g. `unSquare` returns `Maybe Integer` whereas `naturalLog` returns `Maybe Double`. The result of a `do` block is the result of its last expression. Although the context represented by the `List` type is very different from the context of the `Maybe` type, the same abstraction can be applied. Consider the following functions that take a `String` that represents the state of a Tic Tac Toe board and return a `List` of `String`s of the next possible board states:

```
xMove :: String -> [String]
-- xMove "........." == ["X........", ".X.......", ..., "........X"]
-- xMove "XOXOXOXOX" == []
oMove :: String -> [String]
-- oMove ".X......." == ["OX.......", ".XO......", ..., ".X......O"]
-- oMove "XOOXXOOXX" == []
```

We can observe the possible states of a Tic Tac Toe board for the 'O' player using the following function:

```
oMoves :: String -> [String]
oMoves board = do
    board' <- xMove board -- 1. Extracts each String value from the List
                          --    returned by xMove
    oMove board'          -- 2. Applies oMove to each of the values extracted
                          --    in the previous action and joins the results
                          --    to make a single List
```

#### Managing IO Actions

The relevance of this abstraction to Brainfunc (other than the simplification of `nextCursor` made possible by `do` notation) is that all data in Haskell that is associated with IO operations exists in the `IO` context, and that pattern matching on values in the `IO` context is not possible. As such, `do` notation not only provides a concise syntax for manipulating values that exist in an `IO` context but also enables that manipulation to occur. For example, the following functions permit obtaining input from and providing output to the terminal:

```
getLine :: IO String        -- Returns a String obtained from terminal input
putStrLn :: String -> IO () -- Prints the given String to the terminal
```

Using `do` notation, we can extract a `String` obtained using `getLine` and use it to print a customised message using `putStrLn`:

```
main = do
    putStrLn "Enter your name: "  -- 1. Evaluates to IO (), indicating that
                                  --    putStrLn performed a side effect
                                  --    (printing to the terminal)
    input <- getLine              -- 2. Extracts the String from the result
                                  --    of getLine, which is in the IO
                                  --    context, and names it 'input'
    putStrLn ("Hello, " ++ input) -- 3. Applies putStrLn to the modified
                                  --    String and evaluates to IO ()
```

V value that lies outside of the context of the `do` block can be placed inside the context by applying the `pure` function to it. The appropriate context is determined from the usage of the expression in which `pure` appears. For example:

```
main :: IO ()
main = do
    putStrLn "Print greeting?"   -- 1. Evaluates to IO (), indicating that
                                 --    putStrLn performed a side effect
                                 --    (printing to the terminal)
    input <- getLine             -- 2. Extracts the String from the IO context
                                 --    that contains the result of getLine and
                                 --    names it 'input'
    case input of                -- Note: expression must evaluate to IO ()
                                 -- since that's the return type of main
        "y" -> putStrLn "Hello!" -- 3a. Applies putStrLn to the given String
                                 --     resulting in a value of type IO ()
        _   -> pure ()           -- 3b. Evaluates to a value of type IO ()
```

The functions `putStrLn` and `getLine` provide the IO functionality required to implement the `Read` and `Wrte` IO instructions of Brainfuck in Haskell, and `do` notation provides the syntax that enables the use of these functions.

#### Obtaining Integer Input

The function `getLine` returns a `String`, but for the purposes of our Brainfuck interpreter we require `Integer`s from the user. It is for this reason that the `readMaybe` function exists. It has the following type:

```
readMaybe :: (Read a) => String -> Maybe a
```

This type indicates that `readMaybe` takes a `String` and returns a `Just a` if a value of type `a` could be read from the `String`, oftherwise it returns `Nothing`. Although it is not relevant to Brainfunc, know that the type constraint `(Read a) => ...` states that `readMaybe` is only defined for values of type `a` that implement the `Read` typeclass (on which `readMaybe` depends for 'reading' values). The type of `a` can normally be determined by the context in which the result of `readMaybe` is used. For example, the following use of `readMaybe` means that the type `a` is unambiguously `Int`:

```
main = do
    putStrLn "Enter number of worlds to greet:"
    numWorlds <- getLine
    case readMaybe numWorlds of
        Just 0  -> putStrLn "Goodbye"
        Just 1  -> putStrLn "Hello, world!"
        Just _  -> putStrLn "Hello, worlds!"
        Nothing -> putStrLn "Expected integer"
```

Use `readMaybe` to ensure that the input provided by a user when executing the `Wrte` instruction is in the expected format.

#### Required Functions

To implement the IO version of the interpreter we need to implement functions that perform Brainfuck's IO instructions and modify the existing `instruction` and `process` functions to account for the `IO` context. The following functions are required:

* `executeRead`: Allows us to apply the `.` Brainfuck instruction to a code and tape pair.
* `executeWrte`: Allows us to apply the `,` Brainfuck instruction to a code and tape pair.
* `executeInstructionIO`: Extends `executeInstruction` to support IO instructions.
* `executeCodeIO`: Allows us to execute a sequence of instructions, including IO, on the tape.

#### `executeRead`

Implement the `executeRead` function, which receives a `Code` and `Tape`, writes the value in the current cell of the `Tape` to the terminal, then returns the resultant `Code` and `Tape`. Example:

```
executeRead (Zipper [Incr,Incr] Read [], Zipper [] 2 []) ==
    (Zipper [Incr,Incr] Read [], Zipper [] 2 [])
    -- '2' is printed to the terminal
```

#### `executeWrte`

Implement the `executeWrte` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Wrte` instruction, which replaces the `Tape`'s cursor element with the `Integer` value input by the user at the terminal. Example:

```
executeWrte (Zipper [Incr,Incr] Wrte [], Zipper [] 2 []) ==
    (Zipper [Incr,Incr] Wrte [], Zipper [] 5 [])
    -- '5' was entered at the terminal
```

**Suggestion:** First implement `executeWrte` so that it calls `error` when a `String` that cannot be converted into an `Integer` is provided by the user. After completing the interpreter, extend `executeWrte` so that it ignores 'unreadable' input and simply prompts the user for input until they provide a value in the expected format.

#### `executeInstructionIO`

Implement the `executeInstructionIO` function, which receives an `Instruction` and a `Code` and `Tape` pair and returns the resultant `Code` and `Tape` in the context of `IO` after 'executing' the `Instruction`. Examples:

```
executeInstructionIO Read (Zipper [Incr,Incr] Read [Next], Zipper [] 2 []) ==
    IO (Zipper [Incr,Incr] Read [Next], Zipper [] 2 [])
    -- '2' is printed to terminal
executeInstructionIO Wrte (Zipper [Decr,Decr] Wrte [], Zipper [] (-2) []) ==
    IO (Zipper [Decr,Decr] Wrte [], Zipper [] 5 [])
    -- '5' was entered at terminal
executeInstructionIO Incr (Zipper [Next,Incr] Incr [], Zipper [1] 0 []) ==
    IO (Zipper [Next,Incr] Incr [], Zipper [1] 1 [])
executeInstructionIO Open (Zipper [Incr] Open [Incr,Loop], Zipper [] 1 []) ==
    IO (Zipper [Incr] Open [Incr,Loop], Zipper [] 1 [])
executeInstructionIO Loop (Zipper [Open,Incr] Loop [Incr], Zipper [] 1 []) ==
    IO (Zipper [Incr] Open [Loop,Incr], Zipper [] 1 [])
```

#### `executeCodeIO`

Implement the `executeCodeIO` function, which takes a `Code` and a `Tape`, 'executes' every instruction in the `Code` on the `Tape`, and returns `IO ()`. Examples:

```
code = zipperFromList [Incr,Incr,Read,Next,Wrte,Prev,Read,Next,Read]
executeCodeIO code (Zipper [] 0 []) == IO ()
    -- Prints '2' to the terminal, waits for input from the user,
    -- prints '2' to the terminal again, then prints the input value
executeCodeIO Zip Zip               == IO ()
    -- Performs no side-effects
```

### Completing Brainfunc

#### Interpreting Source Code

Although `processIO` executes our interpreter, it requires Brainfuck programs to be encoded as a series of `Instruction`s. To complete Brainfunc we need to enable loading Brainfuck source code - the common representation of Brainfuck programs - and convert it into our instruction sequence. We also need to support the user providing that source code to our interpreter in some manner. These final tasks will turn our interpreter into a complete program that can be run from the command prompt.

#### Required Functions

To complete Brainfunc, we must create a trivial parser and functionality that executes the parsed input. We require the following functions:

* `charToInstruction`: Allows us to map a Brainfuck term to a Brainfuck instruction.
* `stringToCode`: Allows us to convert Brainfuck source code into a sequence of Brainfuck instructions.
* `process`: Allows us to execute Brainfuck programs without needing to provide the initial state of the tape.
* `main` (top-level function): Allows us to prompt the user to enter Brainfuck source code that is then executed.

#### `charToInstruction`

Implement the `charToInstruction` function, which takes a `Char` and returns the `Instruction` that the `Char` corresponds to, if any. Examples:

```
charToInstruction '.' == Just Read
charToInstruction 'c' == Nothing
```

#### `stringToCode`

Implement the `stringToCode` function, which takes a `String` representing Brainfuck source code and returns the `[Instruction]` that the `String` defines. Examples:

```
stringToCode "++." == [Incr, Incr, Read]
stringToCode "+h." == [Incr, Read]
stringToCode "cat" == []
```

#### `process`

Implement the `process` function, which takes a `String` representing Brainfuck source code and generates the corresponding `Code`, then applies `executeCodeIO` to that `Code` and a `Tape`. Examples:

```
process "+.>,<.>." == IO ()
    -- Prints '1' to the terminal, waits for input from the user,
    -- prints '1' to the terminal again, then prints the input value
process [] == IO ()
    -- Performs no side-effects
```

#### `main`

Implement the `main` function, which asks a user to enter Brainfuck source code directly into the terminal, converts the source code to a value of type `Code`, then executes that code. Note that the return type of `main` is always `IO ()` (at the top level, all Haskell programs perform some side-effect and return a value of a type that represents nothing).

### Extending Brainfunc

Consider extending your Brainfunc program in the following ways:

* Modify the `main` function so that it executes a Brainfuck source file at a location specified by a command-line argument. Note that there is a `getArgs :: IO [String]` function in the `System.Environment` module that returns a list of the arguments provided to the program at the command line.
* Modify `instructionIO` so that it supports reading and writing `Integer` values that are less than 256 as `ASCII`-formatted characters. Note that the functions `ord` and `chr` in the `Data.Char` module can convert `Char` to `Int` and `Int` to `Char`, respectively. There is also a function `toInteger` that can convert `Int` to `Integer` in the base module.
* Support a command line flag that causes the interpreter to run in P'' mode in which only the non-IO Brainfuck instructions are supported. Print the state of the tape after the code is executed to the terminal. Note that by suffixing a type definition with `deriving (Show)`, an implementation of the `Show` typeclass is automatically generated for the type, allowing the function `show :: Show a => a -> String` to be used to create a `String` representation of values of the type.
* Modify Brainfunc so that two `Instruction` types exist, one each for Brainfuck's non-IO and IO instructions. Make the non-IO instruction type a subset of the IO instruction type. Correct the `executeInstruction` function using the new types, and use `executeInstruction` to simplify `executeInstructionIO`.

### Wrap-up

Reflect on the code you have created. Consider its overall structure, and the correctness of each function. Consider how robust the design is to logical defects and assess the risk of defects being introduced during the maintenance phase. How could the code be reorganised to eliminate some of the risks to correctness? What alternative representations of `Zipper` exist, and what implications do they have? How could the exceptional cases be represented using types, and how would the code need to be changed to manage values of those types? When is the use of `error` justified? Also consider the patterns that repeat over and over in the code. Where can `do` notation be used to simplify Brainfunc? How can we exploit the symmetry of `nextCursor`-`prevCursor`, `incrCell`-`decrCell` and `nextCell`-`prevCell` to simplify their implementations? What implications would such changes have on the correctness of the program? Hopefully Brainfunc has provided some insight into threats to correctness and the power of Haskell in minimising the risk of software defects.

_____________________________________________

## Worked Examples

### `Zipper` Functions

#### `zipper`

```
zipper :: a -> Zipper a
zipper x = Zipper [] x []
```

The implementation of `zipper` can be determined by considering the possible results of the function, which are determined by the data constructors of `Zipper`. If the function returned `Zip`, then the argument would be discarded. To return a value using the data constructor `Zipper` we must provide a value of type `a` to serve as the `Zipper`'s cursor.

#### `zipperFromList`

```
zipperFromList :: [a] -> Zipper a
zipperFromList (x:xs) = Zipper [] x xs
zipperFromList []     = Zip
```

Just as `Zipper` can be considered to be `List` with 'two tails', `List` can be considered to be the cursor and 'right side' of `Zipper`. If the argument value is the base case of `List` then the result is the base case of `Zipper`.

#### `getCursor`

```
getCursor :: Zipper a -> Maybe a
getCursor (Zipper _ c _) = Just c
getCursor Zip            = Nothing
```

Retrieving the cursor of a `Zipper` requires extracting the cursor element from the argument `Zipper` using pattern matching and returning it as a `Maybe` value using the `Just` data constructor. The result of the function must have type `Maybe a` to address the `Zip` case of `Zipper` in which no cursor exists.

#### `setCursor`

```
setCursor :: a -> Zipper a -> Zipper a
setCursor x (Zipper ls _ rs) = Zipper ls x rs
setCursor x Zip              = Zipper [] x []
```

Setting the cursor element of a `Zipper` is achieved by obtaining the parts of the argument `Zipper` using pattern matching and substituting the cursor element when constructing the result `Zipper`. This implementation of `setCursor` sets the cursor if it does not exist, ensuring that the operation of setting a cursor will always succeed (e.g. calling `setCursor` doesn't result in a `Maybe` value that needs to be managed). This simplifies code that uses `setCursor` while also serving the needs of Brainfunc. Note that the behaviour of the `setCursor` in the `Zip` case is identical to the function `zipper`, thus the function could be implemented as:

```
setCursor :: a -> Zipper a -> Zipper a
setCursor x (Zipper ls _ rs) = Zipper ls x rs
setCursor x Zip              = zipper x
```

#### `next`

```
next :: Zipper a -> Maybe (Zipper a)
next (Zipper ls c (r:rs)) = Just $ Zipper (c:ls) r rs
next _                    = Nothing
```

Pattern matching on the structure of `Zipper` can provide the head of the `Zipper`'s right `List`, which can be made the new cursor element after consing the current cursor element to the head of the left `List`. This operation can fail if there are no elements in the `Zipper`'s right `List`, in which case there is no head to make the new cursor, or if the `Zipper` value is `Zip`, in which case there is no cursor to shift. Both cases are matched by `next _`.

#### `prev`

```
prev :: Zipper a -> Maybe (Zipper a)
prev (Zipper (l:ls) c rs) = Just $ Zipper ls l (c:rs)
prev _                    = Nothing
```

The implementation of `prev` is similar to `next` except that the cursor shift occurs in the left direction.

#### `insertNext`

```
insertNext :: a -> Zipper a -> Zipper a
insertNext x (Zipper ls c rs) = Zipper (c:ls) x rs
insertNext x Zip              = zipper x
```

The `insertNext` function replaces the cursor element but rather than 'overwrite' it, as in `setCursor`, it pushes it onto the `Zipper`'s right `List`, allowing the value to be later 'retrieved' by shifting the cursor one position left. Note that the resultant `Zipper` is shifted to the inserted value, which enables the `Zip` case to be elegantly handled (the caller is guaranteed to receive a `Zipper` with the inserted element as the cursor element).

#### `insertPrev`

```
insertPrev :: a -> Zipper a -> Zipper a
insertPrev x (Zipper ls c rs) = Zipper ls x (c:rs)
insertPrev x Zip              = zipper x
```

The implementation of `insertPrev` is similar to `insertNext` except that the former cursor element is consed onto the `Zipper`'s right `List`.

#### `nextCursor`

```
nextCursor :: Zipper a -> Maybe (a, Zipper a)
nextCursor Zipper ls c (r:rs) = Just $ (r, Zipper (c:ls) r rs)
nextCursor _                  = Nothing
```

The `nextCursor` function is a convenience function that combines shifting a `Zipper` and retrieving its cursor for cases in which both operations are required. This is only possible if the `Zipper`'s right `List` has a head element, which can be determined using pattern matching. The above implementation is arguably flawed because it does not make use of `next` and `getCursor` which implement the two operations on which it depends. The function can instead be written as:

```
nextCursor :: Zipper a -> Maybe (a, Zipper a)
nextCursor z = case next z of
    Just z' -> case getCursor z' of
        Just x  -> Just (x, z')
        Nothing -> Nothing
    Nothing -> Nothing
```

This is verbose because the possible failure of both `next` and `getCursor` must be managed. However, `do` notation can be used to extract the result of both functions in the `Just` cases:

```
nextCursor :: Zipper a -> Maybe (a, Zipper a)
nextCursor z = do
    z' <- next z
    c <- getCursor z'
    pure (c, z')
```

If either `next` or `getCursor` return `Nothing`, `nextCursor` evaluates to `Nothing`, otherwise a `Tuple` of the shifted `Zipper` and its cursor are returned inside a `Just` value. This implementation is preferable to the original because it does not duplicate the functionality of `next` and `getCursor` and because it reduces the burden of correctness on `nextCursor` (`nextCursor` need only correctly implement the interaction between `next` and `getCursor`).

#### `prevCursor`

```
prevCursor :: Zipper a -> Maybe (a, Zipper a)
prevCursor z = do
    z' <- prev z
    c <- getCursor z'
    pure $ (c, z')
```

The implementation of `prevCursor` is identical to `nextCursor` except that it makes use of `prev` in place of `next`. This symmetry can be exploited to create a higher-order function `shiftCursor` that abstracts over the behaviour of `prevCursor` and `nextCursor`. `shiftCursor` is identical to the two functions except that it takes as a parameter the function to use to shift the given `Zipper` value:

```
shiftCursor :: (Zipper a -> Maybe (Zipper a))
            -> Zipper a
            -> Maybe (a, Zipper a)
shiftCursor f z = do
    z' <- f z
    c <- getCursor z'
    pure (c, z')
```

This function enables `nextCursor` and `prevCursor` to be implemented as follows:

```
nextCursor :: Zipper a -> Maybe (a, Zipper a)
nextCursor z = shiftCursor next z
    -- or by eta reduction: nextCursor = shiftCursor next

prevCursor :: Zipper a -> Maybe (a, Zipper a)
prevCursor z = shiftCursor prev z
    -- or by eta reduction: prevCursor = shiftCursor prev
```

This further reduces the burden of correctness on `nextCursor` and `prevCursor` because they only need to be proven to provide the correct `Zipper`-shifting function to `shiftCursor` (the burden of ensuring interactions between a shifting function and `getCursor` are correct is now concentrated in `shiftCursor` instead of being distributed across both `nextCursor` and `prevCursor`).

### `Code` Functions

#### `nextInstruction`

```
nextInstruction :: Zipper Instruction
                -> Maybe (Instruction, Zipper Instruction)
nextInstruction Zipper ls c (r:rs) = Just $ (r, Zipper (c:ls) r rs)
nextInstruction _                  = Nothing
```

The case in which a `Zipper` has a 'next' element that can be made the cursor can be identified using pattern matching. However, the type of the above function is overspecialised: since its behaviour is not conditional on any values of the type `Instruction`, the type of the function is actually `Zipper a -> Maybe (a, Zipper a)`. This matches the type of `nextCursor`, which also has the same behaviour as `nextInstruction`. Therefore, `nextInstruction` can be implemented as follows:

```
nextInstruction :: Zipper Instruction
                -> Maybe (Instruction, Zipper Instruction)
nextInstruction = nextCursor
```

The motivation to redefine `nextCursor` specialised to `Instruction` is semantic: the type of `nextInstruction` makes it clear to readers of the code that the function is intended to manipulate `Code` values. Haskell's type polymorphism enables this to be expressed without requiring duplicate functionality.

#### `prevInstruction`

```
prevInstruction :: Zipper Instruction
                -> Maybe (Instruction, Zipper Instruction)
prevInstruction = prevCursor
```

Similar to the `nextInstruction` function, `prevInstruction` is an alias of a more general `Zipper` shifting function, in this case `prevCursor`.

#### `findMatchingLoop`

An intuition for the implementation of `findMatchingLoop` can be developed by first considering how to iterate through a `Code` value and stop on a `Loop` instruction:

```
findMatchingLoop :: Code -> Maybe Code
findMatchingLoop code = case nextInstruction code of
    Just (nextIns, nextCode) -> case nextIns of
        Loop -> Just nextCode             -- The Loop is found! Return the
                                          -- Code in which the found value
                                          -- is the cursor element
        _    -> findMatchingLoop nextCode -- The Loop has not been found so
                                          -- continue searching
    Nothing                  -> Nothing   -- The end of the initial Code
                                          -- has been reached without a
                                          -- Loop having been found
```

The complexity of `findMatchingLoop` arises when an `Open` value is the next element in `Code`, meaning that the next `Loop` value will correspond to that nested loop, not the loop that corresponds to our initial loop. The key to solving this problem is to recognise that if `findLoop` can find the `Loop` that matches an `Open` for a singly-nested loop, then the nested application of `findLoop` can find the matching `Loop`s for `Open`s in nested loops:

```
findMatchingLoop :: Code -> Maybe Code
findMatchingLoop code = case nextInstruction code of
    Just (nextIns, nextCode) -> case nextIns of
        Open -> case findMatchingLoop nextCode of
            Just code' -> findMatchingLoop code' -- Search continues from the
                                                 -- end of the inner loop
            Nothing     -> Nothing               -- The end of the Code was
                                                 -- reached without a Loop
                                                 -- having been found
        Loop -> Just code'
        _    -> findMatchingLoop code'
    Nothing            -> Nothing
```

Unfortunately, the implementation of `findMatchingLoop` can be called from outside of a loop - i.e. the cursor element of the `Code` value may not be `Open` - but still evaluate to a `Just Code` (e.g. `findMatchingLoop (Zipper [] Incr [Loop]) == Just (Zipper [Incr] Loop [])`). If the cursor element of the given `Code` is not `Open` then the result of the function can be immediately determined to be `Nothing`:

```
findMatchingLoop :: Code -> Maybe Code
findMatchingLoop code = case getCursor code of
    Just Open -> findMatchingLoop' code -- If the cursor element is Open then
                                        -- search for the matching Loop
    _         -> Nothing                -- If the cursor element is not Open
                                        -- then there is no matching Loop
    where
        findMatchingLoop' code = case nextInstruction code of
            Just (nextIns, nextCode) -> case nextIns of
                Open -> case findMatchingLoop' nextCode of
                    Just code'' -> findMatchingLoop' code''
                    Nothing     -> Nothing
                Loop -> Just nextCode
                _    -> findMatchingLoop' nextCode
```

With `do` notation, the excessive `case` expressions can be avoided:

```
findMatchingLoop :: Code -> Maybe Code
findMatchingLoop code = case getCursor code of
    Just Open -> findMatchingLoop' code
    _         -> Nothing
    where
        findMatchingLoop' code = do
            (nextIns, nextCode) <- nextInstruction code
            case nextIns of
                Open -> do
                    loopedCode <- findMatchingLoop' nextCode
                    findMatchingLoop' loopedCode
                Loop -> Just nextCode
                _    -> findMatchingLoop' nextCode
```

#### `findMatchingOpen`

```
findMatchingOpen :: Code -> Maybe Code
findMatchingOpen code = case getCursor code of
    Just Loop -> findMatchingOpen' code
    _         -> Nothing
    where
        findMatchingOpen' code = do
            (prevIns, prevCode) <- prevInstruction code
            case prevIns of
                Loop -> do
                    loopedCode <- findMatchingOpen' prevCode
                    findMatchingOpen' loopedCode
                Open -> Just prevCode
                _    -> findMatchingOpen' prevCode
```

The implementation of `findMatchingOpen` mirrors `findMatchingLoop`, switching the `Loop` and `Open` cases and using `prevInstruction` instead of `nextInstruction`.

### `Tape` Functions

#### `incrCell`

```
incrCell :: Tape -> Tape
incrCell (Zipper ls c rs) = Zipper ls (c + 1) rs
incrCell Zip              = zipper 1
```

To implement `incrCell` we can pattern match on `Zipper` to extract the cursor value that is to be incremented since we know that `Tape` is a `Zipper Integer`. Due to the (simulated) infinite nature of `Tape`, in the case that a `Tape` with no cells is encountered we can simply 'create' the cell by returning a `Zipper` in which the cursor element is 1 (an increment of the default cell value of zero). However, the above implementation of `incrCell` is brittle and depends heavily on the structure of `Zipper`. To increment a cell we need to read the cell's value and then write the increment of that value back to the cell, and the functions `readCell` and `wrteCell` exist for this purpose. Therefore, `incrCell` can be implemented as follows:

```
incrCell :: Tape -> Tape
incrCell z = wrteCell (readCell z + 1) z
```

This implementation enables `readCell` and `wrteCell` to handle `Zipper`'s base case and isolates `incrCell` from the underlying structure of `Tape`.

#### `decrCell`

```
decrCell :: Tape -> Tape
decrCell z = wrteCell (readCell z - 1) z
```

The implementation of `decrCell` mirrors the implementation of `incrCell` and the existence of `readCell` and `wrteCell` allows `decrCell` to be simplified in the same way. We can exploit this symmetry between `incrCell` and `decrCell` to create a higher-order function `applyCell` and define both `incrCell` and `decrCell` in terms of that function:

```
applyCell :: (Integer -> Integer) -> Tape -> Tape
applyCell f z = wrteCell (f $ readCell z) z

incrCell :: Tape -> Tape
incrCell z = applyCell (+1) z
    -- or by eta reduction: incrCell = applyCell (+1)

decrCell :: Tape -> Tape
decrCell z = applyCell (subtract 1) z
    -- or by eta reduction: decrCell = applyCell (subtract 1)
```

#### `nextCell`

```
nextCell :: Tape -> Tape
nextCell z = case next z of
    Just z' -> z'
    Nothing -> insertNext 0 z
```

Since `Tape` is actually a `Zipper Integer`, the `next` function can be used to shift a given `Tape` to the next cell. Since the `Tape` is simulated to be infinite, the failure case of `next` can be handled by simply inserting a new cell into the `Tape`, as performed by `insertNext`.

#### `prevCell`

```
prevCell :: Tape -> Tape
prevCell z = case prev z of
    Just z' -> z'
    Nothing -> insertPrev 0 z
```

As in other `next`-`prev` function pairs, `prevCell` mirrors the implementation of `nextCell` except that `prev` is used in place of `next`. Both functions can be simplified using a higher-order function `fromMaybe` to handle the result of `next`/`prev`, returning the value inside the `Maybe` result if one exists or else returning a default:

```
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe x Nothing  = x

nextCell :: Tape -> Tape
nextCell z = fromMaybe (insertNext 0 z) (next z)

prevCell :: Tape -> Tape
prevCell z = fromMaybe (insertPrev 0 z) (prev z)
```

#### `readCell`

```
readCell :: Tape -> Integer
readCell z = case getCursor z of
    Just c -> c
    Nothing -> 0
```

Recall that we consider our interpreter's tape to contain an infinite number of cells and that each cell initially contains the value zero. A `Tape` value of `Zip` represents an infinite tape that we have not actually constructed; as can be seen in `nextCell` and `prevCell`, we expand the `Tape` as required. Therefore `Zip` corresponds to our infinite, unmodified conceptual tape, in which we know the current cell must contain zero, which we return in the `Nothing` case. The structure of `readCell` is similar to `nextCell` and `prevCell`, and we can simplify it using `fromMaybe` in the same way:

```
readCell :: Tape -> Integer
readCell z = fromMaybe 0 $ getCursor z
```

This implementation can be simplified further by recognising that `getCursor` has type `Zipper a -> Maybe a` and `fromMaybe 0` has type `Maybe a -> a` - the functions can be composed together to create a function of type `Zipper a -> a`:

```
-- The compose function (present in the base module)
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = \x -> f (g x)

readCell :: Tape -> Integer
readCell = fromMaybe 0 . getCursor
```

#### `wrteCell`

```
wrteCell :: Integer -> Tape -> Tape
wrteCell = setCursor
```

The behaviour of `wrteCell` is identical to `setCursor`, it is simply specialised to the type of `Tape`.

### P'' Functions

#### `executeIncr`

```
executeIncr :: (Code, Tape) -> (Code, Tape)
executeIncr (code, tape) = (code, incrCell tape)
```

The `executeIncr` function implements Brainfuck's `+` instruction, incrementing the value in the tape's current cell but leaving the code unchanged. The reason for using a `Tuple` parameter is to indicate the intractable state of the `Code` and `Tape` values on which the function acts. This pattern is used for each of the '`execute<Instruction>`' functions.

#### `executeDecr`

```
executeDecr :: (Code, Tape) -> (Code, Tape)
executeDecr (code, tape) = (code, decrCell tape)
```

The `executeDecr` function implements Brainfuck's `-`, deferring to the `decrCell` function to decrement the given `Tape`'s cursor value but leaving the given `Code` unchanged.

#### `executeNext`

```
executeNext :: (Code, Tape) -> (Code, Tape)
executeNext (code, tape) = (code, nextCell tape)
```

The `executeNext` function implements Brainfuck's `>` instruction by using `nextCell` to modify the given `Tape`. The `>` instruction does not modify the sequence of instructions in a Brainfuck program, thus the `Code` value is returned unchanged.

#### `executePrev`

```
executePrev :: (Code, Tape) -> (Code, Tape)
executePrev (code, tape) = (code, prevCell tape)
```

The `executePrev` function mirrors `executeNext` except that it uses `prevCell` instead of `nextCell` to apply the action of Brainfuck's `<` instruction to the given `Tape`.

#### `executeOpen`

```
executeOpen :: (Code, Tape) -> (Code, Tape)
executeOpen (code, tape) = case readCell tape of
    0 -> case findMatchingLoop code of
        Just code' -> (code', tape)
        Nothing    -> error "Error: No matching ']' instruction"
    _ -> (code, tape)
```

The `executeOpen` function implements Brainfuck's `[` instruction, and therefore must determine whether the instructions in the loop that `[` opens should be skipped or executed. This check is performed using a `case` statement. If `executeOpen` evaluates `findMatchingLoop` and receives a result of `Nothing`, then the Brainfuck program provided to the interpreter is erroneous and Brainfunc terminates with an explanatory message.

#### `executeLoop`

```
executeLoop :: (Code, Tape) -> (Code, Tape)
executeLoop (code, tape) = case readCell tape of
    0 -> (code, tape)
    _ -> case findMatchingOpen code of
        Just code' -> (code', tape)
        Nothing    -> error "Error: No matching '[' instruction"
```

The `executeLoop` function implements Brainfuck's `]` instruction and thus mirrors `executeOpen`, facilitating repeating instructions instead of skipping them.

#### `executeInstruction`

```
executeInstruction :: Instruction -> (Code, Tape) -> (Code, Tape)
executeInstruction Incr = executeIncr
executeInstruction Decr = executeDecr
executeInstruction Next = executeNext
executeInstruction Prev = executePrev
executeInstruction Open = executeOpen
executeInstruction Loop = executeLoop
executeInstruction _    = error "Error: Instruction not supported!"
```

The `executeInstruction` function determines which `execute<Instruction>` function to apply to a `Code` and `Tape` according to the given `Instruction` using pattern matching. Each case is eta reduced for simplicity. For reasons explained in the following section, the `executeInstruction` function cannot process Brainfuck's IO instructions. This is indicated to users by the error case.

#### `executeCode`

The `executeCode` function's only task is to iterate through the given `Code` and execute its `Instruction`s on the given `Tape`. An intuition for how to design `executeCode` can be developed by considering the iteration condition (assume an `Instruction` to process exists):

```
executeCode' :: Instruction -> (Code, Tape) -> Tape
executeCode' ins state = case nextInstruction updatedCode of
    Just (nextIns, nextCode) -> executeCode' nextIns (nextCode, updatedTape)
    Nothing                  -> updatedTape
    where
        (updatedCode, updatedTape) = executeInstruction ins state
```

Execution of the 'current' `Instruction` occurs in the `where` clause, where the next state of `Code` and `Tape` is evaluated using `executeInstruction`. The condition for continuing to iterate is whether there exists a next `Instruction` to execute, which is determined by pattern matching on the result of applying `nextInstruction` to the updated `Code` value. The only special case of `executeCode` is when the provided `code` is `Zip`. This can be handled by pattern matching on the result of `getCursor`:

```
executeCode :: Code -> Tape -> Tape
executeCode code tape = case getCursor code of
    Just ins -> executeCode' ins (code, tape)
        where
            executeCode' :: Instruction -> (Code, Tape) -> Tape
            executeCode' ins state = case nextInstruction updatedCode of
                Just (nextIns, nextCode) -> executeCode'
                    nextIns (nextCode, updatedTape)
                Nothing                  -> updatedTape
                where
                    (updatedCode, updatedTape) = executeInstruction ins state
    Nothing  -> tape
```

### Brainfuck Functions

#### `executeRead`

```
executeRead :: (Code, Tape) -> IO (Code, Tape)
executeRead (code, tape) = do
    putStrLn (show $ readCell tape)
    pure (code, tape)
```

The `executeRead` function implements the IO aspect of the `readCell` function, printing the value read from the given `Tape` to the terminal. After reading from the `Tape` value, the function puts the unmodified `Code` and `Tape` values into the `IO` context using `pure`. The values are placed into the `IO` context even though they are unchanged to indicate to calling functions that the result of `executeRead` is intractable from whatever IO side-effects it may have performed. Similar to `readCell`, `executeRead` can be simplified by recognising that `show` has type `a -> String` and `putStrLn` has type `String -> IO ()`, thus `(.)` can compose them into a single function:

```
executeRead :: (Code, Tape) -> IO (Code, Tape)
executeRead (code, tape) = do
    putStrLn . show $ readCell tape
    pure (code, tape)
```

#### `executeWrte`

```
executeWrte :: (Code, Tape) -> IO (Code, Tape)
executeWrte (code, tape) = do
    x <- getLine
    case readMaybe x :: Maybe Integer of
        Just x' -> pure (code, wrteCell x' tape)
        Nothing -> error "Error: expected integer"
```

Similar to `executeRead` and `readCell`, the `executeWrte` function implements the IO aspect of the `wrteCell` function. The above implementation errors if the user provides non-integer input. The interpreter can be made more user-friendly by printing an instruction to the user in the `Nothing` case and then prompting for input again:

```
executeWrte :: (Code, Tape) -> IO (Code, Tape)
executeWrte (code, tape) = do
    x <- getLine
    case readMaybe x :: Maybe Integer of
        Just x' -> pure (code, wrteCell x' tape)
        Nothing -> do
            putStrLn "Expected integer value"
            executeWrte (code, tape)
```

#### `executeInstructionIO`

```
executeInstructionIO :: Instruction -> (Code, Tape) -> IO (Code, Tape)
executeInstructionIO Incr = pure . executeIncr
executeInstructionIO Decr = pure . executeDecr
executeInstructionIO Next = pure . executeNext
executeInstructionIO Prev = pure . executePrev
executeInstructionIO Open = pure . executeOpen
executeInstructionIO Loop = pure . executeLoop
executeInstructionIO Read = executeRead
executeInstructionIO Wrte = executeWrte
```

The implementation of `executeInstructionIO` is similar to `executeInstruction` except that its result involves `IO`, which enables it to support the `Read` and `Wrte` instructions. To support the non-IO `Instruction` values, `executeInstructionIO` must place their results into the `IO` context, which it does using `pure`. The `(.)` function is used to simplify these cases by exploiting the types of `pure` (specialised to `(Code, Tape) -> IO (Code, Tape)` in this case) and the `execute<non-IO Instruction>` functions (each has type `(Code, Tape) -> (Code, Tape)`). Without `(.)`, the first six cases of `executeInstructionIO` would have the form `executeInstructionIO <Instruction> (code, tape) = pure $ execute<Instruction> (code, tape)`.

#### `executeCodeIO`

```
executeCodeIO :: Code -> Tape -> IO ()
executeCodeIO code tape = case getCursor code of
    Just ins -> executeCodeIO' ins (code, tape)
        where
            executeCodeIO' :: Instruction -> (Code, Tape) -> IO ()
            executeCodeIO' ins state = do
                (updatedCode, updatedTape) <- executeInstructionIO ins state
                case nextInstruction updatedCode of
                    Just (nextIns, nextCode) -> executeCodeIO'
                        nextIns (nextCode, updatedTape)
                    Nothing                  -> pure ()
    Nothing  -> pure ()
```

The implementation of `executeCodeIO` is very similar to `executeCode` except that it must manage the `IO` context introduced by `executeInstructionIO`. This is performed by using `do` notation in the `executeCodeIO'` inner function. Note that `executeCodeIO` returns a value of type `IO ()` rather than `Tape`, indicating that it performs the side-effects of a Brainfuck program and discards the `Code` and `Tape` when the program is completed.

### Completion Functions

#### `charToInstruction`

```
charToInstruction :: Char -> Maybe Instruction
charToInstruction '+' = Just Incr
charToInstruction '-' = Just Decr
charToInstruction '>' = Just Next
charToInstruction '<' = Just Prev
charToInstruction '[' = Just Open
charToInstruction ']' = Just Loop
charToInstruction '.' = Just Read
charToInstruction ',' = Just Wrte
charToInstruction _   = Nothing
```

In Brainfuck, a character that does not correspond to `+`, `-`, `>`, `<`, `[`, `]`, `.` or `,` is considered to be part of a comment and ignored. This case is represented by `Maybe`'s `Nothing` value. If a character corresponds to a Brainfuck instruction, then the corresponding Brainfunc `Instruction` is returned in a `Just` value.

#### `stringToCode`

```
stringToCode :: String -> [Instruction]
stringToCode []     = []
stringToCode (x:xs) = case charToInstruction x of
    Just x' -> x' : stringToCode xs
    Nothing -> stringToCode xs
```

With `charToInstruction`, Brainfuck source code can be converted into a `List` of `Instruction`s by executing `charToInstruction` on each character and retaining only the contents of the `Just` values. Note that, other than `charToInstruction`, no part of `stringToCode` is specific to either `Char` or `Instruction`. The more general `mapMaybe` function can be created to handle all instances of modifying a `List` using a function that returns a `Maybe` result, and `stringToCode` can be defined as a specialisation of that function:

```
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f (x:xs) = case f x of
    Just x' -> x' : mapMaybe f xs
    Nothing -> mapMaybe f xs
mapMaybe _ []     = []

stringToCode :: String -> [Instruction]
stringToCode = mapMaybe charToInstruction
```

#### `process`

```
process :: String -> IO ()
process source = executeCodeIO code Zip
    where code = zipperFromList $ stringToCode source
```

The `process` function simply converts the `String` it receives to a `Code`, which can be performed using `zipperFromList` and `stringToCode`, and passes that `Code` and an empty `Tape` to `executeCodeIO`. The empty `Tape` is expanded by Brainfunc as required by the Brainfuck program (by the `nextCell` and `prevCell` functions).

#### `main`

```
main :: IO ()
main = do
    putStrLn "Enter Brainfuck code:"
    input <- getLine
    process input
```

The `main` function need only print a message to the user about the input it expects, obtain that input, then pass it to the `process` function.
