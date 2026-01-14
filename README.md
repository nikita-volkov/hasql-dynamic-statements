# hasql-dynamic-statements

[![Hackage](https://img.shields.io/hackage/v/hasql-dynamic-statements.svg)](https://hackage.haskell.org/package/hasql-dynamic-statements)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/hasql-dynamic-statements/)

Hasql extension for composable, dynamic construction of SQL statements.

## Overview

This library provides a **Snippet** API that simplifies building SQL statements where the structure depends on runtime parameters. It abstracts over placeholder management and encoder matching, eliminating boilerplate and reducing bugs.

## Key Features

- **Composable**: Build statements using `Semigroup`/`Monoid` operations
- **Type-safe**: Automatic parameter encoding with type-driven encoder derivation
- **Dynamic**: Construct SQL conditionally based on parameters
- **Clean API**: No manual placeholder numbering or encoder alignment

## Quick Example

```haskell
import Hasql.DynamicStatements.Snippet

-- Build a dynamic substring query
selectSubstring :: Text -> Maybe Int32 -> Maybe Int32 -> Snippet
selectSubstring string from to =
  "select substring(" <> param string <>
  foldMap (\x -> " from " <> param x) from <>
  foldMap (\x -> " for " <> param x) to <>
  ")"

-- Execute in a session
result <- toSession (selectSubstring "hello" (Just 2) Nothing) 
  (singleRow $ column $ nonNullable text)
```

## Without Snippet API

Compare the above to manual construction:

```haskell
selectSubstring' :: Text -> Maybe Int32 -> Maybe Int32 -> Statement () Text
selectSubstring' string from to = 
  let sql = case (from, to) of
        (Just _, Just _)  -> "select substring($1 from $2 to $3)"
        (Just _, Nothing) -> "select substring($1 from $2)"
        (Nothing, Just _) -> "select substring($1 to $2)"
        (Nothing, Nothing) -> "select substring($1)"
      encoder = 
        Encoders.param (string >$ Encoders.text) <>
        foldMap (\x -> Encoders.param (x >$ Encoders.int8)) from <>
        foldMap (\x -> Encoders.param (x >$ Encoders.int8)) to
      decoder = singleRow $ column $ nonNullable text
  in Statement.preparable sql encoder decoder
```

The Snippet API eliminates placeholder numbering, pattern matching on parameter presence, and manual encoder sequencing.

## Core API

### Construction

- **`sql`** - Raw SQL text
- **`param`** - Parameter with implicit encoder
- **`encoderAndParam`** - Parameter with explicit encoder

### Execution

- **`toSql`** - Compile to SQL text with placeholders
- **`toStatement`** - Create a `Statement`
- **`toSession`** - Execute directly in `Session`
- **`toPipeline`** - Execute in `Pipeline`
