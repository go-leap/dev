# udevlex
--
    import "github.com/go-leap/dev/lex"


## Usage

#### func  Lex

```go
func Lex(filePath string, src string) (tokenStream []Token, errs []*LexError)
```
Lex returns the `Token`s lexed from `src`, or all `LexError`s encountered while
lexing.

If `errs` has a `len` greater than 0, `tokenStream` will be empty (and vice
versa).

#### type LexError

```go
type LexError struct {
	Pos scanner.Position
}
```

LexError holds a message returned by `Error` and `String`, plus additional
positional details.

#### func (*LexError) Error

```go
func (me *LexError) Error() string
```
Error implements the `error` interface.

#### func (*LexError) String

```go
func (me *LexError) String() string
```
String implements the `fmt.Stringer` interface.

#### type Token

```go
type Token interface {
	// contains filtered or unexported methods
}
```

Token is the interface implemented by the various `TokenFoo` structs in this
package.

#### type TokenChar

```go
type TokenChar struct {
	Token rune
}
```

TokenChar holds a `rune` that was scanned from a quoted literal.

#### type TokenComment

```go
type TokenComment struct {
	Token string

	// SingleLine denotes whether the comment started with `//` (as opposed to `/*`), it does not actually reflect the number of lines in `Token`.
	SingleLine bool
}
```

TokenComment holds a comment `string` that was scanned from a `// ..` or `/* ..
*/` fragment, sans the separators.

#### type TokenFloat

```go
type TokenFloat struct {
	Token float64
}
```

TokenFloat holds a `float64` that was scanned from a literal.

#### type TokenIdent

```go
type TokenIdent struct {
	Token string
}
```

TokenIdent holds a `string` that was scanned from an unquoted alphanumeric range
of characters.

#### type TokenInt

```go
type TokenInt struct {
	Token int64
}
```

TokenInt holds an `int64` that was scanned from a literal.

#### type TokenOther

```go
type TokenOther struct {
	Token string
}
```

TokenOther holds (typically, but not guaranteed, uni-`rune`) `string`s that are
theoretically anything-not-fitting-other-token-types, but in practice for the
most part typically interpreted as operator, separation or punctuation
characters.

#### type TokenStr

```go
type TokenStr struct {
	Token string
}
```

TokenStr holds the unquoted `string` that was scanned from a quoted literal.

#### type TokenUInt

```go
type TokenUInt struct {
	Token uint64
}
```

TokenUInt holds an `uint64` that was scanned from a literal exceeding the
maximum-possible `int64`.
