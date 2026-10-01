# Wylinka

devirt for imperva `reese84`.

```
served .js
   │  analysis   parse + AST cleanup, pull bytecode + opcode table
   ▼
bytecode (265k instrs)
   │  ingest     decode -> (operator, operand, dest) triplets
   ▼
triplets (88k)
   │  flatten    abstract-interpret the combinator stream, decurry
   ▼
flat stmts (11k)
   │  fold       collapse the pure coercion games to constants
   ▼
output/*.clean.flat.js   (lossy — enough to read, analyze, and solve against)
```

```
cargo run --release -- input/one.js
```

## ai usage

some of the tedious manual work (opcode mapping and the like) and optimisations were done primarily through AI,
but the reverse-engineering and the calls on what to keep vs throw away are mine