# syntax-pattern

---

*This project is part of my master's studies and is therefore still experimental 
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

- [GHC](https://www.haskell.org/ghc/) (>= 8.10)
- [Cabal](https://www.haskell.org/cabal/), or [Stack](https://docs.haskellstack.org/en/stable/)

### Building

Clone the repository and build with Cabal:

```bash
git clone https://github.com/yourusername/syntax-pattern.git
cd syntax-pattern
cabal build
```

Or with Stack:

```bash
stack build
```

### Usage

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

#### File-based Pipeline Functions

Most pipeline functions also have `IO` variants that accept file paths instead of raw 
strings. These allow you to directly specify files containing PEG grammars, patterns, 
and input data.  
You can find several example PEG, pattern, and input files in the `input/` directory 
to experiment with.

#### Tests

The `test/` directory contains property-based and sanity tests for the main algorithms
and is still in progress. 
You can use these to check the correctness and robustness of the library.

See the [Haddock documentation](docs/index.html) for detailed API usage and examples.

## Documentation

- All modules are documented with Haddock. To generate HTML documentation:

```bash
cabal haddock
# or
stack haddock
```

- The documentation covers:
  - PEG and pattern syntax
  - Pattern matching and rewriting
  - Error handling and semantic checks

## License

MIT © Guilherme Drummond, 2025

## Author

Guilherme Drummond  
[guiadnguto@gmail.com](mailto:guiadnguto@gmail.com)
