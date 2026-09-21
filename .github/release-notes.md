## 0.80.6 - 2026-09-21

Two fixes, both found by the fuzzer.

### A brace inside a string in `%{...}` did not lex

The interpolation ended at the first `}` that balanced the braces counted
since it opened, and a brace written inside a string was counted with them.
So a `{` in a string ran off the end of the file, and a `}` ended the
interpolation one character into the argument.

```
"%{String.replace "{" "[" s}"

-- before    lex error: unterminated string interpolation
-- now       a[b}
```

### `wand f` left a list of punned fields on one line however long it was

A construction whose fields all pun is written as a list of bare names, and
that form had no wrapped shape. The named spelling of the same construction
did wrap, so reformatting it twice gave two answers.
