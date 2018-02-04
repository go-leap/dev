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

#### type IToken

```go
type IToken interface {
	fmt.Stringer

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

	// SingleLine denotes whether the comment started with `//` (as opposed to `/*`), it does not actively denote the existence or absence of actual line-breaks in `Token`.
	SingleLine bool
}
```

TokenComment holds a comment `string` that was scanned from a `// ..` or `/* ..
*/` fragment, sans the separators.

#### type TokenFloat

```go
type TokenFloat struct {
	TokenMeta
	Token float64
}
```

TokenFloat holds a `float64` that was scanned from a floating-point literal.

#### type TokenIdent

```go
type TokenIdent struct {
	Token string
	TokenMeta
}
```

TokenIdent holds a `string` that was scanned from an unquoted alphanumeric range
of characters.

#### type TokenMeta

```go
type TokenMeta struct {
	scanner.Position
	LineIndent int
	Orig       string
}
```

TokenMeta is embedded by all `Token` implementers.

#### func (*TokenMeta) Meta

```go
func (me *TokenMeta) Meta() *TokenMeta
```

#### func (*TokenMeta) String

```go
func (me *TokenMeta) String() string
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

#### type TokenRune

```go
type TokenRune struct {
	TokenMeta
	Token rune
}
```

TokenRune holds a `rune` that was scanned from a quoted literal.

#### type TokenSep

```go
type TokenSep struct {
	Token string
	TokenMeta
}
```

TokenSep holds a (uni-`rune`) `string` that matched one of `Lex`s specified
`standAloneSeps`.

#### type TokenStr

```go
type TokenStr struct {
	Token string
	TokenMeta
	Raw bool
}
```

TokenStr holds the unquoted `string` that was scanned from a quoted literal.

#### type TokenUint

```go
type TokenUint struct {
	TokenMeta
	Base  int
	Token uint64
}
```

TokenUint holds an `uint64` that was scanned from an integral literal.

#### type Tokens

```go
type Tokens []IToken
```


#### func  Lex

```go
func Lex(filePath string, src string, restrictedWhitespace bool, standAloneSeps ...string) (tokens Tokens, errs []*Error)
```
Lex returns the `Token`s lexed from `src`, or all `LexError`s encountered while
lexing.

If `errs` has a `len` greater than 0, `tokens` will be empty (and vice versa).

#### func (Tokens) BreakOnIdent

```go
func (me Tokens) BreakOnIdent(needleIdent string, skipForEachOccurrenceOfIdent string) (pref Tokens, suff Tokens, numUnclosed int)
```
BreakOnIdent finds the desired occurrence of `needleIdent` in `me`, then returns
in `pref` all `Tokens` preceding it and in `suff` all following it. If
`skipForEachOccurrenceOfIdent` is given, then for every encountered `TokenIdent`
occurrence of it one `needleIdent` occurrence will be skipped. If `numUnclosed`
is not `0`, this typically indicates a syntax error depending on the language
being lexed; strictly speaking it denotes the number of skipped-and-not-closed
occurrences of `skipForEachOccurrenceOfIdent`. Unless a correct break position
was found, `pref` and `suff` will both be `nil`.

#### func (Tokens) BreakOnIndent

```go
func (me Tokens) BreakOnIndent(minIndent int) (indented Tokens, outdented Tokens)
```
BreakOnIndent returns in `indented` all `Tokens` on the same line as the first
in `me`, plus all subsequent `Tokens` with `LineIndent` greater than
`minIndent`; and in `outdented` the first and all following `Tokens` with a
`LineIndent` less-or-equal (if any).

#### func (Tokens) BreakOnOther

```go
func (me Tokens) BreakOnOther(token string) (pref Tokens, suff Tokens)
```
BreakOnOther returns all `Tokens` preceding and succeeding the next occurence of
the specified `TokenOther` in `me`, if any — otherwise, `me,nil` will be
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

#### func (Tokens) String

```go
func (me Tokens) String() string
```

#### func (Tokens) Sub

```go
func (me Tokens) Sub(sepOpen string, sepClose string) (sub Tokens, tail Tokens, numUnclosed int)
```
Sub assumes (but won't check: up to the caller) that `me` begins with a
`TokenSep` of `sepOpen` and returns in `sub` the subsequence of `Tokens` up
until a matching `TokenSep` of `sepClose`. If no correct subsequence is found,
`sub` is `nil` and `tail` is `me` (and `numUnclosed` might be non-`0` to
indicate the number of unclosed groupings) — otherwise `sub` is the subsequence
immediately following the opening `sepOpen` up to and excluding the matching
`sepClose`, and `tail` is all trailing `Tokens` immediately following it.
