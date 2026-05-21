# syntax-pattern

---

*This project is part of my doctorate's studies and is therefore still experimental 
in nature, intended primarily for educational and research purposes.*

---

**syntax-pattern** is a Haskell library for parsing, analyzing, matching, and 
rewriting syntax trees using Parsing Expression Grammars (PEGs) and user-defined 
patterns. It is designed for research and experimentation with syntax-driven 
transformations and pattern matching in abstract syntax trees (ASTs).

## Features

- **PEG Parsing:** Define grammars using PEGs and parse input strings into syntax 
trees.
- **Pattern Language:** Express complex patterns over syntax trees, including 
variables, choices, sequences, and repetitions.
- **Pattern Matching:** Match patterns against parsed trees and capture subtrees.
- **Rewriting:** Rewrite syntax trees by applying pattern-based transformations.
- **Semantic Analysis:** Validate grammars and patterns, detect left recursion, 
duplicate rules, and other semantic errors.
- **Pretty Printing:** Human-readable output for grammars, patterns, and trees.
- **Extensible:** Modular design for easy extension and integration.

## Project Structure

```haskell
src/
  Match/
    Capture.hs       -- Pattern matching and capture
    Rewrite.hs       -- Tree rewriting
  Parser/
    Base.hs          -- Parser combinators and utilities
    ParsedTree.hs    -- PEG-based parser to AST
    Pattern.hs       -- Pattern parser
    Peg.hs           -- PEG grammar parser
  Pipeline/
    MatchPipeline.hs -- High-level pipeline for parsing, matching, and rewriting
  Quote/
    Base.hs          -- Quasi-quoter base functions
    Pattern.hs       -- Pattern quasi-quoter
    Peg.hs           -- PEG quasi-quotter
  Semantic/
    Pattern.hs       -- Semantic analysis for patterns
    Peg.hs           -- Semantic analysis for PEGs
  Syntax/
    Base.hs          -- Core types (Terminal, NonTerminal, etc.)
    ParsedTree.hs    -- AST definition and utilities
    Pattern.hs       -- Pattern types
    Peg.hs           -- PEG types and utilities
input/
  peg/               -- Example PEG grammars
  pattern/           -- Example pattern files
  file/              -- Example input files
test/
  Main.hs            -- Property and sanity tests
```

## Getting Started

### Prerequisites

You may build the project either locally or using Docker.

#### Local environment

- [GHC](https://www.haskell.org/ghc/) (>= 8.10)
- [Cabal](https://www.haskell.org/cabal/), or [Stack](https://docs.haskellstack.org/en/stable/)

#### Docker environment

- [Docker](https://www.docker.com/)
- Docker Compose

### Building

First, clone the repository:

```bash
git clone https://github.com/guinasc2/ast-pattern-matching
cd syntax-pattern
```

After this, you may build using Cabal:

```bash
cabal update
cabal build
```

Or with Stack:

```bash
stack build
```

Alternatively, you also may use docker to setup. 
After cloning the repository, run:

```bash
docker compose build
docker compose run ghci
```

This will already run ```cabal update``` inside the container for you.

## Usage

You can use the library in your own Haskell projects or run the provided pipelines 
for parsing, matching, and rewriting:

```haskell
import Pipeline.MatchPipeline

-- Parse and validate a PEG grammar from a string
let grammarResult = parseValidGrammar "S <- \"a\" S / \"b\""

-- Parse and validate patterns
let patternsResult = parseValidPatterns grammarString patternString

-- Parse an input file and match patterns
let matchResult = parseMatch grammarString patternString inputString
```

### File-based Pipeline Functions

Most pipeline functions also have `IO` variants that accept file paths instead of raw 
strings. These allow you to directly specify files containing PEG, patterns, and input data.  
You can find several example PEG, pattern, and input files in the `input/` directory 
to experiment with. The file extension for PEG and pattern files are .peg and .pat
respectively, but they are simple text files.

### Tests

The `test/` directory contains property-based and sanity tests for the main algorithms
and is still in progress. 
You can use these to check the correctness and robustness of the library.

See the [Haddock documentation](#documentation) for detailed API usage and examples.

### Running examples

After building the project, you may run some provived examples. First, run the REPL:

```bash
cabal repl
```

And then load the pipeline module:
```bash
:l Pipeline.MatchPipeline
```

#### Example 1 - Parsing a file: 
Run
```bash
parseFileIO "input/peg/expressao.peg" "input/file/expressao.txt" True
```

The ```parseFileIO``` function takes as arguments two files and a boolean. The first is a 
file that contains the PEG, while the second contains the input data. The boolean indicates
in which way you want the parsed content to be displayed: if ```True```, it will flatten the content
and show it exactly as is in the file. Otherwise, it you show the generated tree.

The result should be:
```bash
(1+2)*3
```

If you ran with ```False```:
```bash
NT E
╰╴Seq
  ├╴NT T
  | ╰╴Seq
  |   ├╴NT F
  |   | ╰╴Right
  |   |   ╰╴Seq
  |   |     ├╴"("
  |   |     ╰╴Seq
  |   |       ├╴NT E
  |   |       | ╰╴Seq
  |   |       |   ├╴NT T
  |   |       |   | ╰╴Seq
  |   |       |   |   ├╴NT F
  |   |       |   |   | ╰╴Left
  |   |       |   |   |   ╰╴NT n
  |   |       |   |   |     ╰╴"1"
  |   |       |   |   ╰╴Star []
  |   |       |   ╰╴Star [
  |   |       |     ├╴Seq
  |   |       |     | ├╴"+"
  |   |       |     | ╰╴NT T
  |   |       |     |   ╰╴Seq
  |   |       |     |     ├╴NT F
  |   |       |     |     | ╰╴Left
  |   |       |     |     |   ╰╴NT n
  |   |       |     |     |     ╰╴"2"
  |   |       |     |     ╰╴Star []
  |   |       |     ╰╴]
  |   |       ╰╴")"
  |   ╰╴Star [
  |     ├╴Seq
  |     | ├╴"*"
  |     | ╰╴NT F
  |     |   ╰╴Left
  |     |     ╰╴NT n
  |     |       ╰╴"3"
  |     ╰╴]
  ╰╴Star []
```

#### Example 2 - Matching with patterns: 
Run
```bash
parseMatch1IO "input/peg/python.peg" "input/pattern/factorial.pat" "input/file/fact_math.py" "factorial_call"
```

The ```parseCallGraphIO``` function takes as arguments three files and one string. The files
are the PEG file, pattern file and input file, respectively. The string is an identifiers for 
any pattern inside the pattern file. In this case, ```factorial_call``` is a pattern that 
matches with calls to functions named ```math.factorial```.

The output should be something like this:
```bash
factorial_call: match!
```
Indicating that the indicated pattern did match inside the file.

Running
```bash
parseMatch1IO "input/peg/python.peg" "input/pattern/factorial.pat" "input/file/fact_while.py" "factorial_call"
```
will produce something like this:
```bash
factorial_call: not match!
```
Indicating that the indicated pattern did not match inside the file.

#### Example 3 - extracting call graph from a Python file: 
Run
```bash
parseCallGraphIO "input/peg/python.peg" "input/pattern/call_graph.pat" "input/file/ex4.py" "definition" "call"
```

The ```parseCallGraphIO``` function takes as arguments three files and two strings. The files
are the PEG file, pattern file and input file, respectively. The strings are identifiers for 
patterns inside the pattern file, where the first one is a pattern that matches with functions definitions and the second one a pattern that matches with function calls.

The output should be something like this:
```bash
bhaskara -> delta
bhaskara -> math.sqrt
```
Indicating that the function ```bhaskara``` calls both ```delta``` and ```math.sqrt```.

#### Example 4 - rewriting based on patterns: 
This is the content of ```input/file/if.py```:
```python
if not a:
    print(b)
    print(b1)
else:
    print(c)
    print(c1)
```

Run
```bash
parseRewriteIO "input/peg/python.peg" "input/pattern/subst_if.pat" "input/file/if.py" "if_def" "subst"
```

The ```parseRewriteIO``` function takes as arguments three files and two strings. The files
are the PEG file, pattern file and input file, respectively. The strings are identifiers for 
patterns inside the pattern file, where the first one is a pattern that matches with some
desired data and the second one specifies how to rewrite the matched data.

The output should be something like this:
```bash
if a:print(c)
print(c1)else:print(b)
print(b1)
```
The printing is a bit broken, but it is possible to see that it swapped the ```if``` and
```else``` body and removed the ```not``` from the condition.

You may change the input files (and their contents) for new tests, if you wish.

## Documentation

All modules are documented with Haddock. To generate HTML documentation:

```bash
cabal haddock
```

or

```bash
stack haddock
```

- The documentation covers:
  - PEG and pattern syntax
  - Pattern matching and rewriting
  - Error handling and semantic checks
