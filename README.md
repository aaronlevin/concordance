# To Run

`git clone https://github.com/weirdcanada/concordance.git ; cd concordance ; ./sbt run`

# About

We solve the concordance problem in two ways:

### 1. Naive

In this solutino we take the most naive apporach. Split a sentance on `'.'` and words on `' '` and `foldLeft` into oblivion. It is naive for a number of reasons:

- not all words are separated by spaces (compond words and things with hyphens)
- not all sentances are separated by periods (e.g. semi-colons)
- we are doing a very naive punctuation cleanup
- this is all done in memory and will blow-up on a large data set.

### 2. Constant Memory using Iteratees

In this solution we assume that we're solving the concordance problem on a huge text file or other buffered input. We use pure functional `Iteratee`s, similar to a Free Monadic implementation of a function that steps through a computation in constant memory. There are several advantages to Iteratees (they are composable, use constant memory, and are fairly flexible) and disadvantages (they are complex and hard to use).

