# udevlex
--
    import "github.com/go-leap/dev/lex"


## Usage

#### func  Lex

```go
func Lex(filePath string, src io.Reader, restrictedWhitespace bool, lineOff int, posOff int, standAloneSeps ...string) (tokens Tokens, errs []*Error)
```
Lex returns the `Token`s lexed from `src`, or all `Error`s encountered while
lexing.

If `errs` has a `len` greater than 0, `tokens` will be empty (and vice versa).

#### type Error

```go
type Error struct {
	Pos scanner.Position
}
```

Error holds a message returned by `Error`, plus additional positional details.

#### func  Err

```go
func Err(pos *scanner.Position, msg string) *Error
```

#### func (*Error) Error

```go
func (me *Error) Error() string
```
Error implements Go's standard `error` interface.

#### type Token

```go
type Token struct {
	Meta TokenMeta

	Str   string
	Float float64
	Uint  uint64
}
```


#### func (*Token) IsCommentLong

```go
func (me *Token) IsCommentLong() bool
```

#### func (*Token) IsStrRaw

```go
func (me *Token) IsStrRaw() bool
```

#### func (*Token) Kind

```go
func (me *Token) Kind() (kind TokenKind)
```

#### func (*Token) Rune

```go
func (me *Token) Rune() (r rune)
```

#### func (*Token) String

```go
func (me *Token) String() string
```

#### func (*Token) UintBase

```go
func (me *Token) UintBase() int
```

#### type TokenKind

```go
type TokenKind = int
```


```go
const (
	TOKEN_STR TokenKind
	TOKEN_COMMENT
	TOKEN_FLOAT
	TOKEN_IDENT
	TOKEN_OTHER
	TOKEN_SEP
	TOKEN_RUNE
	TOKEN_UINT
)
```

#### type TokenMeta

```go
type TokenMeta struct {
	scanner.Position
	LineIndent int
	Orig       string
}
```


#### type Tokens

```go
type Tokens []Token
```


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
containing all `Tokens` in `me` except those with a `Kind` of `TOKEN_COMMENT`.

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
