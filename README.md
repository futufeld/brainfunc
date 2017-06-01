# Building a Brainfuck Interpreter in Haskell

## Purpose

This document describes how to build an interpreter for the Brainfuck language, which we shall call _Brainfunc_, using Haskell. It describes the components of the interpreter and also provides examples of those components. The goals of this document are to 1) assist readers to build a small program in Haskell and 2) expose readers to design considerations in functional programming.

## Contents

* [How to Use this Material](#how-to-use-this-material)
* [Brainfuck and Brainfunc](#brainfuck-and-brainfunc)
* [Preliminaries](#preliminaries)
* [Building a Zipper](#building-a-zipper)
* [Representing Brainfuck Programs](#representing-brainfuck-programs)
* [Representing an Infinite Tape](#representing-an-infinite-tape)
* [Building a P'' Interpreter](#building-a-p-interpreter)
* [Building a Brainfuck Interpreter](#building-a-brainfuck-interpreter)
* [Completing Brainfunc](#completing-brainfunc)
* [Extending Brainfunc](#extending-brainfunc)
* [Conclusion](#conclusion)
* [Worked Examples](#worked-examples)

## How to Use this Material

This material consists of two parts: descriptions of the functions that comprise Brainfunc, and example implementations of those functions. Each section in the first part of the document provides a brief overview of the concepts that will be covered in that section, then describes the functionality required to implement those concepts. The functions are presented in an approximate order of their complexity, therefore it is suggested that you work through them in the presented order. In each case, first write down the type of the function, then address the base cases (where certain input values directly correspond to certain output values), then complete the implementation.

Although worked examples are presented in the second part of this document, it is recommended to defer looking at them for as long as possible (ideally until after you have completed your interpreter). If stuck, move on to the next function and come back to the skipped function before moving to the next section (no function's implementation depends on another function from the same section). Also discuss the functions and their implementation with fellow Haskellers as an alternative to referring to the worked examples. The motivation for this suggestion is that the process of writing Haskell code involves exploring the structure of types and the relationships between them, which is somewhat undermined by referring to complete examples. Often by 'trying everything' you will not only improve your understanding of types and functions but also stumble across the answer and can work back from there.

## Brainfuck and Brainfunc

In 1936 Alan Turing described a model of effective computability that has since become known as the _Turing Machine_. This work was part of a larger research effort that demonstrated that a function is computable if there exists an algorithm that produces the same output as the function if given the same input. The Turing Machine is a model for constructing algorithms, which it achieves using the following:

* An infinite _tape_ that consists of discrete _cells_ that contain symbols.
* A _head_ that can move between cells and read from or write symbols to them.
* A _state register_ that stores the state of the Machine.
* A _table_ that consists of a finite number of instructions.

The Turing Machine informed the development of a conceptual architecture for computing machines, now known as the _von Neumann Architecture_ (after John von Neumann), on which all modern general purpose computers are based. (Very) Broadly, the table corresponds to stored programs, the state register to the program's runtime, the tape to computer memory and the head to the central processing unit.

In 1964 Corrado Böhm developed the P'' programming language to describe a family of Turing Machine variants. In 1992 Urban Muller adapted and extended P'' to incorporate input and output operations and named the resulting language _Brainfuck_. The Brainfuck language defines a set of instructions that can be performed on a conceptual tape, in which each cell contains an integer value, by a conceptual head:

* `+`: Increments the value in the cell under the head.
* `-`: Decrements the value in the cell under the head.
* `>`: Moves the head to the next cell.
* `<`: Moves the head to the previous cell.
* `[`: Denotes the beginning of a loop. If the value under the head is zero, then the instructions within the loop are skipped. The instruction does not affect the tape.
* `]`: Denotes the end of a loop. If the value under the head is non-zero, then the instructions within the loop it closes are repeated. The instruction does not affect the tape.
* `.`: Reads a value from the cell under the head and prints it to the terminal.
* `,`: Writes a value entered by the user at the terminal to the cell under the head.

Interpreters of the Brainfuck language may vary in terms of the number of cells on the tape and the maximum and minimum values that a cell can contain. In the most liberal interpretation, the tape is infinite (finite only in memory) and each cell can contain any integer value.

In 1932, prior to the discovery of the Turing Machine, Alonso Church discovered the _Lambda Calculus_. Lambda Calculus consists of three components: terms, abstractions, and expressions in which an abstraction is applied to a term. When first describing the Turing Machine, Turing demonstrated that the Machine and Lambda Calculus are equivalent. However, the two models begat very different paradigms of programming: the instruction-oriented Turing Machine influenced the creation of languages that consist of instructions that modify shared state - the imperative paradigm - whereas the expression-oriented Lambda Calculus enabled the creation of languages that define programs as a single expression - the functional paradigm. The purpose of this material is to guide you to create an interpreter for Brainfuck, an imperative language, in Haskell, a functional language. It is a brain<i>functional</i> interpreter. _Brainfunc_, if you will.

## Preliminaries

### Knowledge Required to Create Brainfunc

Brainfunc depends on Haskell's `List`, `Maybe`, `Tuple` and `IO` types, and also requires limited use of Haskell's `error` operation. The IO aspects of Brainfunc will be explained as they appear. In this section, the required knowledge of `List`, `Maybe`, `Tuple` and `error` is detailed.

###  `List`

The `List` type has the following structure:

```
data [a] = [] | a : [a]
```

Thus a `List` value is either `[]`, meaning that it is empty, or it has a 'head' element `a` followed by a 'tail' `[a]`, which is another list. In the latter case, we say `a` is 'cons'ed onto `[a]`. The type variable `a` indicates that a list can be created for any type, with the constraint that the list only contains values of that type. The following are all `List` values:

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

## Building a Zipper

### Defining Zipper

`List` is defined as a value followed by a `List` of values. We can exploit this structure to move rightwards through a `List` value using recursion. However, the structure of `List` is too limited to allow us to move leftwards through its elements. We require a `List` that has a 'tail' in both the left and right directions. This is `Zipper`. The `Zipper` type has the following structure:

> data Zipper a = Zip | Zipper [a] a [a]

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

`Zipper` enables us to move backwards and forwards through a sequence of values.

### Required Functions

#### Overview

With a definition of `Zipper`, we can define various functions that allow us to modify a value of type `Zipper a`. There are many functions that are useful for manipulating `Zipper` values, but only the following functions are required to create Brainfunc:

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

Implement the `zipperFromList` function, which returns a `Zipper` representation of the argument `List`.

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

Implement the `setCursor` function, which replaces the cursor of a given `Zipper` with a given value. If no cursor exists, return a `Zipper` in which the cursor element is the given value. Examples:

```
setCursor 9 (Zipper [3,2,1] 4 [5,6,7]) == Zipper [3,2,1] 9 [5,6,7]
setCursor "?" (Zipper "ac" 't' "nip")  == Zipper "ac" '?' "nip"
setCursor 1 Zip                        == Zipper [] 1 []
```

#### `next`

Implement the `next` function, which shifts the cursor of the given `Zipper` one position to the right if it is possible to do so. Examples:

```
next (Zipper [3,2,1] 4 [5,6,7]) == Just (Zipper [4,3,2,1] 5 [6,7])
next (Zipper "ac" 't' "nip")    == Just (Zipper "tac" 'n' "ip")
next Zip                        == Nothing
```

#### `prev`

Implement the `prev` function, which shifts the cursor of the given `Zipper` one position to the left if it is possible to do so. Examples:

```
prev (Zipper [3,2,1] 4 [5,6,7]) == Just (Zipper [2,1] 3 [4,5,6,7])
prev (Zipper "ac" 't' "nip")    == Just (Zipper "c" 'a' "tnip")
prev Zip                        == Nothing
```

#### `insertNext`

Implement the `insertNext` function, which inserts a given value to the right of a given `Zipper`'s cursor and then shifts the cursor one position to the right. Examples:

```
insertNext 9 (Zipper [3,2,1] 4 [5,6,7]) == Zipper [4,3,2,1] 9 [5,6,7]
insertNext '?' (Zipper "ac" 't' "nip")  == Zipper "tac" '?' "nip"
insertNext 9 Zip                        == Zipper [] 9 []
```

#### `insertPrev`

Implement the `insertPrev` function, which inserts a given value to the left of a given `Zipper`'s cursor and then shifts the cursor one position to the left. Examples:

```
insertPrev 9 (Zipper [3,2,1] 4 [5,6,7]) == Zipper [3,2,1] 9 [4,5,6,7]
insertPrev '?' (Zipper "ac" 't' "nip")  == Zipper "ac" '?' "tnip"
insertPrev 9 Zip                        == Zipper [] 9 []
```

#### `nextCursor`

Implement the `nextCursor` function, which returns a cursor of the given `Zipper` shifted one element to the right as well as that shifted `Zipper`, if shifting is possible. Examples:

```
nextCursor (Zipper [3,2,1] 4 [5,6,7])  == Just (5, Zipper [4,3,2,1] 5 [6,7])
nextCursor (Zipper [6,5,4,3,2,1] 7 []) == Nothing
nextCursor Zip                         == Nothing
```

#### `prevCursor`

Implement the `prevCursor` function, which returns a cursor of the given `Zipper` shifted one element to the left as well as that shifted `Zipper`, if shifting is possible. Examples:

```
prevCursor (Zipper [3,2,1] 4 [5,6,7])  == Just (3, Zipper [2,1] 3 [4,5,6,7])
prevCursor (Zipper [] 1 [2,3,4,5,6,7]) == Nothing
prevCursor Zip                         == Nothing
```

## Representing Brainfuck Programs

### Encoding Brainfuck Instructions

To represent a Brainfuck program, it is necessary to represent the program's instructions. This can be achieved using a sum type:

> data Instruction = Incr | Decr | Next | Prev | Open | Loop | Read | Wrte

Observe that each of the data constructors of `Instruction` corresponds to one of the instructions of Brainfuck. (For the sake of brevity, `Instruction` values will occasionally be represented using only their first letter.) A Brainfuck program is a sequence of `Instruction`s. To implement loops, the structure that represents the sequence must enable forward and backward iteration. The `Zipper` is one such structure, allowing us to define Brainfuck programs as follows:

> type Code = Zipper Instruction

This makes `Code` an alias of `Zipper Instruction`, increasing the readability of our Haskell program while also allowing us apply any function that takes a `Zipper a` to values of type `Code`. The cursor of a `Code` represents the next instruction to be executed in the Brainfuck program.

### Required Functions

#### Overview

After each `Instruction` is processed, the program's sequence of instructions, `Code`, has to be shifted one position to the right. However, the `Open` and `Loop` instructions can require the instruction sequence to be shifted to the corresponding instruction, shifting from `Open` to `Loop` or `Loop` to `Open` depending on whether the loop is to be entered or repeated, respectively. Therefore we require three functions to process values of the `Code` type:

* `nextInstruction`: Allows us to shift to the next Brainfuck instruction.
* `findMatchingLoop`: Allows us to skip the instructions within a loop.
* `findMatchingOpen`: Allows us to repeat the instructions within a loop

#### `nextInstruction`

Implement the `nextInstruction` function, which returns a cursor of the given `Code` shifted one element to the right as well as that shifted `Code`, if it was possible to shift.

```
nextInstruction (Zipper [I, N] I [P])   == Just (P, Zipper [I, I, N] P [])
nextInstruction (Zipper [I, I, N] P []) == Nothing
nextInstruction Zip                     == Nothing
```

#### `findMatchingLoop`

Implement the `findMatchingLoop` function, which takes a `Code` in which the cursor element is `Open` and returns that `Code` shifted so that the cursor element is the corresponding `Loop` instruction, if one exists. Example:

```
findMatchingLoop (Zipper [] O [O, L, L]) == Just (Zipper [L, O, O] L [])
findMatchingLoop (Zipper [] O [L])       == Just (Zipper [O] L [])
findMatchingLoop (Zipper [O] L [])       == Nothing
findMatchingLoop (Zipper [] O [])        == Nothing
findMatchingLoop (Zipper [] R [])        == Nothing
```

**Suggestion:** First implement `findMatchingLoop` for non-nested loops. After completing the interpreter, revise `findMatchingLoop` so that it can find the matching `Loop` instruction for `Open` regardless of loop nesting.

#### `findMatchingOpen`

Implement the `findMatchingOpen` function, which takes a `Code` in which the cursor element is `Loop` and returns that `Code` shifted so that the cursor element is the corresponding `Open` instruction, if one exists. Example:

```
findMatchingOpen (Zipper [L, O, O] L []) == Just (Zipper [] O [O, L, L])
findMatchingOpen (Zipper [O] L [])       == Just (Zipper [] O [L])
findMatchingOpen (Zipper [] O [L])       == Nothing
findMatchingOpen (Zipper [] L [])        == Nothing
findMatchingOpen (Zipper [] I [])        == Nothing
```

**Suggestion:** First implement `findMatchingOpen` for non-nested loops. After completing the interpreter, revise `findMatchingOpen` so that it can find the matching `Open` instruction for `Loop` regardless of loop nesting.

## Representing an Infinite Tape

### Simulating Infinite Cells

To implement our interpreter we require a way to represent an infinite tape, and support the actions that may be performed on the tape in Brainfuck programs. The `Zipper` type closely resembles the conceptual tape of Turing's Machine. Its cursor can be used to represent the cell under the head of the Turing Machine, and the cursor can have an indefinite number of elements to its left and right. However, Brainfuck constrains the way in which we can modify the value in a cell, and our `Zipper` values are not infinite. We must create a collection of functions that operate on a `Zipper` value - this `Zipper` will represent our tape - and modify it in such a way that: 1) the bounded nature of the `Zipper` type is not apparent, and 2) that the instructions of Brainfuck programs are supported. To assist the readability of our code, `Zipper Integer` will be aliased thusly:

> type Tape = Zipper Integer

Note that this allows us to apply any function that has a `Zipper a` argument to any value of type `Tape`.

### Required Functions

#### Overview

Six functions are required to simulate an infinite tape for the purposes of Brainfunc. They correspond to the actions that may be performed on a tape according to the instructions of the Brainfuck program:

* `incrCell`: Allows us to increase the value in the current cell by one.
* `decrCell`: Allows us to decrease the value in the current cell by one.
* `nextCell`: Allows us to move to the next cell on the tape.
* `prevCell`: Allows us to move to the previous cell on the tape.
* `readCell`: Allows us to read the value at the current cell.
* `wrteCell`: Allows us to write a value to the current cell.

#### `incrCell`

Implement the `incrCell` function, which accepts a `Tape` and returns a `Tape` that is identical to the original except that the cursor's value is incremented by one. Example:

```
incrCell (Zipper [3,2,1] 5 [7,8,9]) == Zipper [3,2,1] 6 [7,8,9]
```

#### `decrCell`

Implement the `decrCell` function, which accepts a `Tape` and returns a `Tape` that is identical to the original except that the cursor's value is decremented by one. Example:

```
incrCell (Zipper [3,2,1] 5 [7,8,9]) == Zipper [3,2,1] 4 [7,8,9]
```

#### `nextCell`

Implement the `nextCell` function, which accepts a `Tape` and returns that `Tape` shifted one position to the right. Example:

```
nextCell (Zipper [3,2,1] 4 [5,6,7]) == Zipper [4,3,2,1] 5 [6,7]
```

#### `prevCell`

Implement the `prevCell` function, which accepts a `Tape` and returns that `Tape` shifted one position to the left. Example:

```
prevCell (Zipper [3,2,1] 4 [5,6,7]) == Zipper [2,1] 3 [4,5,6,7]
```

#### `readCell`

Implement the `readCell` function, which returns the cursor element of the given `Tape`. Example:

```
readCell (Zipper [3,2,1] 4 [5,6,7]) == 4
```

#### `wrteCell`

Implement the `wrteCell` function, which replaces the cursor element of the given `Tape` with the given `Integer` value. Example:

```
wrteCell 4 (Zipper [3,2,1] 9 [5,6,7]) == Zipper [3,2,1] 4 [5,6,7]
```

## Building a P'' Interpreter

### From `Zipper` to Interpreter

The functions developed thus far enable us to interact with `Zipper` values and treat `Zipper`s like a sequence of Brainfuck instructions or as an seemingly infinite sequence of cells. The next step is to implement the functionality that modifies our code or tape according to the Brainfuck instruction being processed, and then repeat that functionality for every instruction in the program. This is the stage where we encode the behaviour of the non-IO aspects of Brainfuck into our Haskell program.

### Required Functions

#### Overview

To implement our Brainfuck interpreter, we require two groups of functions, 1) functions that apply the changes of a Brainfuck instruction to the code and tape, and 2) functions that extract instructions from the code and apply them:

* `executeIncr`: Allows us to apply the `+` Brainfuck instruction to a code and tape.
* `executeDecr`: Allows us to apply the `-` Brainfuck instruction to a code and tape.
* `executeNext`: Allows us to apply the `>` Brainfuck instruction to a code and tape.
* `executePrev`: Allows us to apply the `<` Brainfuck instruction to a code and tape.
* `executeOpen`: Allows us to apply the `[` Brainfuck instruction to a code and tape.
* `executeLoop`: Allows us to apply the `]` Brainfuck instruction to a code and tape.
* `executeInstruction`: Allows us to execute a single non-IO instruction on the tape.
* `executeCode`: Allows us to execute a sequence of non-IO instructions on the tape.

In the next section, we will extend this collection of functions to support Brainfuck's IO instructions.

#### `executeIncr`

Implement the `executeIncr` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Incr` instruction. Example:

```
executeIncr (Zipper [I] I [N,W], Zipper [] 1 []) ==
    (Zipper [I] I [N,W], Zipper [] 2 [])
```

#### `executeDecr`

Implement the `executeDecr` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Decr` instruction. Example:

```
executeDecr (Zipper [I] D [D,D], Zipper [] 1 []) ==
    (Zipper [I] D [D,D], Zipper [] 0 [])
```

#### `executeNext`

Implement the `executeNext` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Next` instruction. Example:

```
executeNext (Zipper [I] N [D,R], Zipper [] 1 []) ==
    (Zipper [I] N [D,R], Zipper [1] 0 [])
```

#### `executePrev`

Implement the `executePrev` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Prev` instruction. Example:

```
executePrev (Zipper [I] P [D,R], Zipper [] 1 []) ==
    (Zipper [I] P [D,R], Zipper [] 0 [1])
```

#### `executeOpen`

Implement the `executeOpen` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Open` instruction. Examples:

```
executeOpen (Zipper [I] O [L], Zipper [] 1 []) ==
    (Zipper [I] O [L], Zipper [] 0 [])
executeOpen (Zipper [] O [D,L], Zipper [] 0 []) ==
    (Zipper [D,O] L [], Zipper [] 0 [])
```

#### `executeLoop`

Implement the `executeLoop` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Loop` instruction. Examples:

```
executeLoop (Zipper [O,I] L [], Zipper [] 1 []) ==
    (Zipper [I] O [L], Zipper [] 1 [])
executeLoop (Zipper [O] L [], Zipper [] 0 []) ==
    (Zipper [O] L [], Zipper [] 0 [])
```

#### `executeInstruction`

Implement the `executeInstruction` function, which receives an `Instruction` and a `Code` and `Tape` and returns the resultant `Code` and `Tape` after 'executing' the `Instruction`. If the provided `Instruction` is an IO instruction (`Read` or `Wrte`) the function should error. Examples:

```
executeInstruction Incr (Zipper [N,I,I] Incr [N,I], Zipper [2] 0 []) ==
    (Zipper [N,I,I] Incr [N,I], Zipper [2] 1 [])
executeInstruction Open (Zipper [] Open [I,I,L], Zipper [] 0 []) ==
    (Zipper [I,I,O] Loop [], Zipper [] 0 [])
executeInstruction Open (Zipper [I] Open [I,I,L], Zipper [] 1 []) ==
    (Zipper [I] Open [I,I,L], Zipper [] 1 [])
executeInstruction Loop (Zipper [D,O,I] Loop [I], Zipper [] 0 []) ==
    (Zipper [D,O,I] Loop [I], Zipper [] 0 [])
executeInstruction Loop (Zipper [O,I] Loop [I], Zipper [] 1 []) ==
    (Zipper [I] Open [L,I], Zipper [] 1 [])
executeInstruction Read (Zipper [] Read [], Zipper [] 0 []) ==
    error "Unsupported instruction!"
executeInstruction Wrte (Zipper [I] Wrte [], Zipper [] 1 []) ==
    error "Unsupported instruction!"
```

#### `executeCode`

Implement the `executeCode` function, which takes a `Code` and a `Tape` and returns the `Tape` created by executing each `Instruction` in `Code` on the given `Tape`. Examples:

```
executeCode (Zipper [] Next [I,I,N,D]) (Zipper [] 0 []) ==
    (Zipper [N,I,I,N] Decr [], Zipper [2, 0] -1 [])
executeCode Zip (Zipper [] 0 []) == Zipper [] 0 []
```

## Building a Brainfuck Interpreter

### Understanding Actions that Occur Within a Context

#### IO in Brainfunc

To implement the `Read` and `Wrte` instructions of the Brainfuck language it is necessary to perform IO actions. In Haskell, all IO operations occur within the `IO` context. The intention of this section is to provide an intuition for understanding this concept of 'contexts' and how values that are within a context can be modified using a syntactical structure called `do` notation. This is required knowledge for performing IO actions in Brainfunc.

#### A Notion of Context

Types like `List a` and `Maybe a` can be understood as providing a context for `a`. `List` takes `a` and represents a context where many values of `a` may exist. `Maybe` takes an `a` and represents the context that a value of type `a` may or may not be present. Often there is a sequence of operations to be applied to a value and each result is in a context. Handling the context can be cumbersome. Consider, for example, a function `squareOf` that returns the number that its argument is a square of (if any such number exists) and a function `naturalLog` that returns the natural logarithm of an integer:

```
squareOf :: Integer -> Maybe Integer
naturalLog :: Integer -> Maybe Double
```

Note that for certain inputs both functions do not define a result, therefore the result must be in the `Maybe` context. To apply both `squareOf` and `naturalLog` to a value it is necessary to first match on the result of `squareOf`, which is verbose when using pattern matching:

```
squareOfAndLogn :: Integer -> Maybe Double
squareOfAndLogn x = case squareOf x of
    Just x' -> naturalLog x'
    Nothing -> Nothing
```

#### Using `do` Notation

Haskell provides a special syntax, called `do` notation, that enables values that are in a context to be extracted from the context without requiring pattern matching. For example, `do` notation allows us to implement `squareOfAndLogn` as follows:

```
squareOfAndLogn :: Integer -> Maybe Double
squareOfAndLogn x = do
    x' <- squareOf x -- 1. Extracts x' from the Maybe Integer that is returned
    			     --    by squareOf
    naturalLog x'    -- 2. Applies the naturalLog function to the extracted
                     --    Integer x', resulting in a Maybe Double
```

The `<-` syntax extracts the value from the context to its right, giving it the name of the term on its left (see 1.). The `<name> <-` syntax is optional and only required if the contextual value returned by the expression on the right is needed (see 2.). All expressions in a `do` block must evaluate to a type in the same context - e.g. `squareOf` and `naturalLog` both return `Maybe`s - but that the 'wrapped' type can vary - e.g. `squareOf` returns `Maybe Integer` whereas `naturalLog` returns `Maybe Double`. The result of a `do` block is the result of its last expression. Although the context represented by the `List` type is very different from the context of the `Maybe` type, it provides the same abstraction. Consider the following functions that take a `String` that represents the state of a Tic Tac Toe board and return a `List` of `String`s of the next possible board states:

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

The relevance of this abstraction to Brainfunc (other than the simplification of `nextCursor` made possible by `do` notation) is that all data in Haskell that is associated with IO operations exists in the `IO` context, and we can manipulate that data using `do` notation. For example, the following functions enable input from and output to the terminal:

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

In cases where it is useful to return a value that is not inside a context from a `do` block, the function `pure` exists. This function takes a value and returns that value in the required context. the appropriate context is determined from the usage of the expression in which `pure` appears. For example:

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
        _   -> pure ()           -- 3b. Evaluates to () in the IO context
```

The functions `putStrLn` and `getLine` provide the IO functionality required to implement the `Read` and `Wrte` IO instructions of Brainfuck in Haskell.

#### Obtaining Integer Input

The function `getLine` returns a `String`, but for the purposes of our Brainfuck interpreter we require `Integer` input from the user. It is for this reason that the `readMaybe` function exists. It has the following type:

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

### Required Functions

#### Overview

To complete the IO version of the interpreter we need to implement functions for the IO instructions and modify the existing `instruction` and `process` functions to account for the `IO` context. The following functions are required:

* `executeRead`: Allows us to apply the `.` Brainfuck instruction to a code and tape.
* `executeWrte`: Allows us to apply the `,` Brainfuck instruction to a code and tape.
* `executeInstructionIO`: Extends `executeInstruction` to support IO instructions.
* `executeCodeIO`: Allows us to execute a sequence of instructions, including IO, on the tape.

#### `executeRead`

Implement the `executeRead` function, which receives a `Code` and `Tape`, writes the value in the current cell of the `Tape` to the terminal, then returns the resultant `Code` and `Tape`. Example:

```
executeRead (Zipper [I,I] R []) (Zipper [] 2 []) ==
    (Zipper [I,I] R []) (Zipper [] 2 []) -- '2' is printed to terminal
```

#### `executeWrte`

Implement the `executeWrte` function, which receives a `Code` and `Tape` and returns the resultant `Code` and `Tape` after executing the `Wrte` instruction, which replaces the `Tape`'s cursor element with the `Integer` value input by the user at the terminal. Example:

```
executeWrte (Zipper [I,I] W []) (Zipper [] 2 []) ==
    (Zipper [I,I] W []) (Zipper [] 5 []) -- '5' was entered at terminal
```

**Suggestion:** First implement `executeWrte` so that it calls `error` when a `String` that cannot be converted into an `Integer` is provided by the user. After completing the interpreter, extend `executeWrte` so that it ignores 'unreadable' input and simply prompts the user for input until they provide a value in the expected format.

#### `executeInstructionIO`

Implement the `executeInstructionIO` function, which receives an `Instruction` and a `Code` and `Tape` and returns the resultant `Code` and `Tape` in the context of `IO` after 'executing' the `Instruction`. Examples:

```
executeInstructionIO Read (Zipper [I,I] Read [N,I], Zipper [] 2 []) ==
    IO (Zipper [I,I] Read [N,I], Zipper [] 2 []) -- '2' is printed to terminal
executeInstructionIO Wrte (Zipper [D,D] Wrte [P], Zipper [] (-2) []) ==
    IO (Zipper [D,D] Wrte [P], Zipper [] 5 []) -- '5' was entered at terminal
executeInstructionIO Incr (Zipper [N,I,I] Incr [N,I], Zipper [2] 0 [0,0]) ==
    IO (Zipper [N,I,I] Incr [N,I], Zipper [2] 1 [0,0])
executeInstructionIO Open (Zipper [I] Open [I,I,L], Zipper [] 1 []) ==
    IO (Zipper [I] Open [I,I,L], Zipper [] 1 [])
executeInstructionIO Loop (Zipper [O,I] Loop [I], Zipper [] 1 []) ==
    IO (Zipper [I] Open [L,I], Zipper [] 1 [])
```

#### `executeCodeIO`

Implement the `executeCodeIO` function, which takes a `Code` and a `Tape`, 'executes' every instruction in the `Code` on the `Tape`, and returns `IO ()`. Examples:

```
executeCodeIO (Zipper [] Incr [I,R,N,W,P,R,N,R]) (Zipper [] 0 []) == IO ()
    -- Prints '2' to the terminal, waits for input from the user,
    -- prints '2' to the terminal again, then prints the input value
executeCodeIO Zip (Zipper [] 0 []) == IO ()
```

## Completing Brainfunc

### Interpreting Source Code

Although `processIO` executes our interpreter, it requires Brainfuck programs to be encoded as a series of `Instruction`s. To complete Brainfunc we need to enable loading Brainfuck source code - the common representation of Brainfuck programs - and convert it into our instruction sequence. We also need to support the user providing that source code to the program in some manner. These final tasks will turn our interpreter functionality into a complete program that can be run from the command prompt.

### Required Functions

#### Overview

To complete Brainfunc, the following functions are required:

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

Implement the `stringToCode` function, which takes a `String` representing Brainfuck source code and returns the `Code` that the `String` defines. Examples:

```
stringToCode "++." == Zipper [] Incr [I,R]
stringToCode "+h." == Zipper [] Incr [R]
stringToCode "cat" == Zip
```

#### `process`

Implement the `process` function, which takes a `String` representing Brainfuck source code and generates the corresponding `Code`, then applies `processIO` to that `Code` and a default `Tape`. Examples:

```
process "+.>,<.>." == IO ()
    -- Prints '2' to the terminal, waits for input from the user,
    -- prints '2' to the terminal again, then prints the input value
process [] == IO ()
    -- Performs no side-effects
```

#### `main`

Implement the `main` function, which asks a user to enter Brainfuck source code directly into the terminal, converts the source code to a value of type `Code`, then executes that code. Note that the return type of `main` is always `IO ()` (at the top level, all Haskell programs perform some side-effect and return a value of a type that represents nothing).

## Extending Brainfunc

Consider extending your Brainfunc program in the following ways:

* Modify the `main` function so that it executes a Brainfuck source file at a location specified by a command-line argument. Note that there is a `getArgs :: IO [String]` function in the `System.Environment` module that returns a list of the arguments provided to your program at the command line.
* Modify `instructionIO` so that it supports reading and writing `Integer` values that are less than 256 as `ASCII`-formatted characters. Note that the functions `ord` and `chr` in the `Data.Char` module can convert `Char` to `Int` and `Int` to `Char`, respectively. There is also a function `toInteger` that can convert `Int` to `Integer` in the base module.

Finally, reflect on the code you have created. Consider its overall structure, and the correctness of each function. Consider how robust the design is to logical defects and assess the risk of defects being introduced during the maintenance phase. How could the code be reorganised to eliminate some of the risks to correctness? How could the exceptional cases be represented using types, and when is the use of `error` justified? Also consider the patterns that repeat over and over in the code. Where can `do` notation be used to simplify Brainfunc? How can we exploit the symmetry of `incrCell`-`decrCell`, `nextCell`-`prevCell` and `findMatchingLoop`-`findMatchingOpen` to simplify the implementation? What implications would such changes have on the correctness of the program? Hopefully Brainfunc has provided some insight into threats to correctness and the power of Haskell in minimising the risk of software defects.
