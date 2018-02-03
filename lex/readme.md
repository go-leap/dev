# udevlex
--
    import "github.com/go-leap/dev/lex"


## Usage

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

#### type IPos

```go
type IPos interface {
	Pos() *TokenMeta
}
```


#### func  Pos

```go
func Pos(tokens Tokens, fallback IPos, fallbackFilePath string) IPos
```
Pos returns the last in `tokens`, or `fallback`, or a new `TokenMeta` at
position 1,1 for `fallbackFilePath`.

#### type IToken

```go
type IToken interface {
	fmt.Stringer
	IPos

	Meta() *TokenMeta
	// contains filtered or unexported methods
}
```

IToken is the interface implemented by the various `TokenFoo` structs in this
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

TokenFloat holds a `float64` that was scanned from a floating-point literal.

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

#### func (*TokenMeta) Pos

```go
func (me *TokenMeta) Pos() *TokenMeta
```

#### type TokenOther

```go
type TokenOther struct {
	Token string
	TokenMeta
}
```

TokenOther holds a `string` that is a consecutive sequence (1 or more
characters) of anything-not-fitting-other-token-types.

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

#### type TokenSep

```go
type TokenSep struct {
	Token string
	TokenMeta
}
```

TokenSep holds a (uni-`rune`) `string` that matched one of `Lex`s specified
`standAloneSeps`.

#### func (*TokenSep) String

```go
func (me *TokenSep) String() string
```

#### type TokenStr

```go
type TokenStr struct {
	Token string
	TokenMeta
	Raw bool
}
```

TokenStr holds the unquoted `string` that was scanned from a quoted literal.

#### func (*TokenStr) String

```go
func (me *TokenStr) String() (s string)
```

#### type TokenUint

```go
type TokenUint struct {
	TokenMeta
	Base  int
	Token uint64
}
```

TokenUint holds an `uint64` that was scanned from an integral literal.

#### func (*TokenUint) String

```go
func (me *TokenUint) String() string
```

#### type Tokens

```go
type Tokens []IToken
```


#### func  Lex

```go
func Lex(filePath string, src string, standAloneSeps ...string) (tokens Tokens, errs []*Error)
```
Lex returns the `Token`s lexed from `src`, or all `LexError`s encountered while
lexing.

If `errs` has a `len` greater than 0, `tokens` will be empty (and vice versa).

#### func (Tokens) BreakOnIdent

```go
func (me Tokens) BreakOnIdent(needleIdent string, skipForEachOccurrenceOfIdent string) (pref Tokens, suff Tokens, numUnclosed int)
```

#### func (Tokens) BreakOnIndent

```go
func (me Tokens) BreakOnIndent() (indented Tokens, outdented Tokens)
```

#### func (Tokens) BreakOnOther

```go
func (me Tokens) BreakOnOther(token string) (pref Tokens, suff Tokens)
```
BreakOnOther returns all `Tokens` preceding and succeeding the next occurence of
the specified `TokenOther` in `me`, if any — otherwise, `nil,nil` will be
returned.

#### func (Tokens) IndentBasedChunks

```go
func (me Tokens) IndentBasedChunks(minIndent int) (chunks []Tokens)
```
IndentBasedChunks breaks up `me` into a number of `chunks`: each 'non-indented'
line (with `LineIndent` <= `minIndent`) in `me` begins a new 'chunk' and any
subsequent 'indented' (`LineIndex` > `minIndent`) lines also belong to it.

#### func (Tokens) SansComments

```go
func (me Tokens) SansComments() (sans Tokens)
```
SansComments returns the newly allocated `sans` with a `cap` of `len(me)` and
containing all `Tokens` in `me` except `TokenComment`s.

#### func (Tokens) SubTokens

```go
func (me Tokens) SubTokens(sepOpen string, sepClose string) (sub Tokens, tail Tokens, numUnclosed int)
```
