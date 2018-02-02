# udevlex
--
    import "github.com/go-leap/dev/lex"


## Usage

#### func  Lex

```go
func Lex(filePath string, src string) (tokenStream []Token, errs []*Error)
```
Lex returns the `Token`s lexed from `src`, or all `LexError`s encountered while
lexing.

If `errs` has a `len` greater than 0, `tokenStream` will be empty (and vice
versa).

#### type Error

```go
type Error struct {
	Pos scanner.Position
}
```

Error holds a message returned by `Error` and `String`, plus additional
positional details.

#### func  Err

```go
func Err(pos *scanner.Position, msg string) *Error
```

#### func (*Error) Error

```go
func (me *Error) Error() string
```
Error implements the `error` interface.

#### type Token

```go
type Token interface {
	fmt.Stringer

	Meta() *TokenMeta
	// contains filtered or unexported methods
}
```

Token is the interface implemented by the various `TokenFoo` structs in this
package.

#### type TokenComment

```go
type TokenComment struct {
	Token string
	TokenMeta

	// SingleLine denotes whether the comment started with `//` (as opposed to `/*`), it does not actually reflect the number of lines in `Token`.
	SingleLine bool
}
```

TokenComment holds a comment `string` that was scanned from a `// ..` or `/* ..
*/` fragment, sans the separators.

#### func (*TokenComment) String

```go
func (me *TokenComment) String() string
```

#### type TokenFloat

```go
type TokenFloat struct {
	TokenMeta
	Token float64
}
```

TokenFloat holds a `float64` that was scanned from a literal.

#### func (*TokenFloat) String

```go
func (me *TokenFloat) String() string
```

#### type TokenIdent

```go
type TokenIdent struct {
	Token string
	TokenMeta
}
```

TokenIdent holds a `string` that was scanned from an unquoted alphanumeric range
of characters.

#### func (*TokenIdent) String

```go
func (me *TokenIdent) String() string
```

#### type TokenInt

```go
type TokenInt struct {
	TokenMeta
	Token int64
}
```

TokenInt holds an `int64` that was scanned from a literal.

#### func (*TokenInt) String

```go
func (me *TokenInt) String() string
```

#### type TokenMeta

```go
type TokenMeta struct {
	scanner.Position
	LineIndent int
}
```

TokenMeta is embedded by all `Token` implementers.

#### func (*TokenMeta) Meta

```go
func (me *TokenMeta) Meta() *TokenMeta
```

#### type TokenOther

```go
type TokenOther struct {
	Token string
	TokenMeta
}
```

TokenOther holds (typically, but not guaranteed, uni-`rune`) `string`s that are
theoretically anything-not-fitting-other-token-types, but in practice for the
most part typically interpreted as operator, separation or punctuation
characters.

#### func (*TokenOther) String

```go
func (me *TokenOther) String() string
```

#### type TokenRune

```go
type TokenRune struct {
	TokenMeta
	Token rune
}
```

TokenRune holds a `rune` that was scanned from a quoted literal.

#### func (*TokenRune) String

```go
func (me *TokenRune) String() string
```

#### type TokenStr

```go
type TokenStr struct {
	Token string
	TokenMeta
}
```

TokenStr holds the unquoted `string` that was scanned from a quoted literal.

#### func (*TokenStr) String

```go
func (me *TokenStr) String() string
```

#### type TokenUint

```go
type TokenUint struct {
	TokenMeta
	Token uint64
}
```

TokenUint holds an `uint64` that was scanned from a literal exceeding the
maximum-possible `int64`.

#### func (*TokenUint) String

```go
func (me *TokenUint) String() string
```
