# Smol Agent Protocol Implementations

This directory contains implementations of the Smol Agent Protocol (Code Minimalization as Constraint Optimization) in various programming languages.

## Overview

The Smol Agent Protocol is a systematic approach to code minimization that treats the problem as a constraint optimization challenge:

- **Objective Function**: Minimize `size(code)` in bytes
- **Hard Constraint**: `functionality(optimized_code) == functionality(original_code)`

## Available Implementations

### Lisp Family
- **smol.scm** - Guile Scheme implementation
- **smol.lisp** - Common Lisp implementation
- **smol.el** - Emacs Lisp implementation
- **smol.echo.lisp** - Echo Lisp implementation (https://github.com/o9nn/echolisp)
- **smol.rkt** - Racket implementation

### Logic Programming
- **smol-prolog.pl** - Prolog implementation

### Systems Programming
- **smol.c** - C implementation
- **smol.cpp** - C++ implementation
- **smol.go** - Go implementation
- **smol.zig** - Zig implementation
- **smol.b** - Limbo implementation (Inferno OS)

### Scripting Languages
- **smol-perl.pl** - Perl implementation
- **smol.raku** - Raku (Perl 6) implementation
- **smol.py** - Python implementation
- **smol.rb** - Ruby implementation
- **smol.jl** - Julia implementation

### Functional Programming
- **smol.hs** - Haskell implementation
- **smol.ml** - OCaml implementation

### Esoteric/Historical
- **smol.forth** - Forth implementation

### Declarative Formats
- **smol.json** - JSON representation
- **smol.yaml** - YAML configuration format
- **smol.toml** - TOML configuration format
- **smol.xml** - XML representation

### Documentation Formats
- **smol.ascii** - ASCII art diagram of the protocol flow
- **smol.math.md** - Complete mathematical formulation with proofs

## Core Algorithm

Each implementation follows the same constraint optimization loop:

1. **Baseline Establishment**
   - Measure initial code size
   - Document functionality requirements
   - Run tests to verify current behavior

2. **Iterative Optimization Loop**
   - Apply transformations:
     - Syntax compaction (remove whitespace, shorten identifiers)
     - Statement reduction (arrow functions, comma operators)
     - Structural optimization (hoist constants, inline variables)
     - Semantic equivalence (shorter patterns)
   - Verify functionality preservation
   - Measure new size
   - Decision rule: Accept only if functionality preserved AND size reduced

3. **Convergence**
   - Continue until no more optimizations possible
   - Report final size and reduction percentage

## Key Principles

All implementations adhere to these principles:

1. **Functionality is Sacred** - Never sacrifice working code for size
2. **Measure Everything** - Every change must be measured in bytes
3. **Verify Continuously** - Test after every transformation
4. **Version Iteratively** - Keep track of each optimization attempt
5. **Embrace Reversibility** - Be ready to revert any change
6. **Converge Systematically** - Stop when no more byte savings found

## Usage Examples

### Scheme (Guile)
```scheme
(use-modules (smol-agent))
(minimize-code "path/to/file.js")
```

### Common Lisp
```lisp
(load "smol.lisp")
(smol-agent:minimize-code "path/to/file.js")
```

### Go
```bash
go run smol.go path/to/file.js
```

### Python
```bash
python smol.py path/to/file.js
```

### C
```bash
gcc smol.c -o smol
./smol path/to/file.js
```

## Decision Rule

The optimization decision rule is consistent across all implementations:

```
if functionality_preserved AND size_reduced:
    return ACCEPT  # Continue optimizing
elif functionality_preserved AND size_same:
    return NEUTRAL  # Try different optimization
else:
    return REJECT  # Revert changes
```

## Mathematical Formulation

This is a constrained optimization problem:

```
minimize: f(x) where f(x) = size(code)
subject to: g(x) = 0 where g(x) = functionality(original) - functionality(optimized)
```

## Testing

Each implementation includes:
- Syntax verification using `node -c`
- Functional testing using `npm test`
- Size measurement in bytes
- Iterative version tracking

## License

These implementations are provided as reference implementations of the Smol Agent Protocol for educational and research purposes.
