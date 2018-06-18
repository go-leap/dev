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

Error holds a message returned by `Error`, plus additional positional details.

#### func  Err

```go
func Err(pos *scanner.Position, msg string) *Error
```

#### func (*Error) Error

```go
func (this *Error) Error() string
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
func (this *Token) IsCommentLong() bool
```

#### func (*Token) IsStrRaw

```go
func (this *Token) IsStrRaw() bool
```

#### func (*Token) Kind

```go
func (this *Token) Kind() (kind TokenKind)
```

#### func (*Token) Rune

```go
func (this *Token) Rune() (r rune)
```

#### func (*Token) String

```go
func (this *Token) String() string
```

#### func (*Token) UintBase

```go
func (this *Token) UintBase() int
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


#### func  Lex

```go
func Lex(filePath string, src string, restrictedWhitespace bool, standAloneSeps ...string) (tokens Tokens, errs []*Error)
```
Lex returns the `Token`s lexed from `src`, or all `Error`s encountered while
lexing.

If `errs` has a `len` greater than 0, `tokens` will be empty (and vice versa).

#### func (Tokens) BreakOnIdent

```go
func (this Tokens) BreakOnIdent(needleIdent string, skipForEachOccurrenceOfIdent string) (pref Tokens, suff Tokens, numUnclosed int)
```
BreakOnIdent finds the desired occurrence of `needleIdent` in `this`, then
returns in `pref` all `Tokens` preceding it and in `suff` all following it. If
`skipForEachOccurrenceOfIdent` is given, then for every encountered `TokenIdent`
occurrence of it one `needleIdent` occurrence will be skipped. If `numUnclosed`
is not `0`, this typically indicates a syntax error depending on the language
being lexed; strictly speaking it denotes the number of skipped-and-not-closed
occurrences of `skipForEachOccurrenceOfIdent`. Unless a correct break position
was found, `pref` and `suff` will both be `nil`.

#### func (Tokens) BreakOnIndent

```go
func (this Tokens) BreakOnIndent(minIndent int) (indented Tokens, outdented Tokens)
```
BreakOnIndent returns in `indented` all `Tokens` on the same line as the first
in `this`, plus all subsequent `Tokens` with `LineIndent` greater than
`minIndent`; and in `outdented` the first and all following `Tokens` with a
`LineIndent` less-or-equal (if any).

#### func (Tokens) BreakOnOther

```go
func (this Tokens) BreakOnOther(token string) (pref Tokens, suff Tokens)
```
BreakOnOther returns all `Tokens` preceding and succeeding the next occurence of
the specified `TokenOther` in `this`, if any — otherwise, `this,nil` will be
returned.

#### func (Tokens) IndentBasedChunks

```go
func (this Tokens) IndentBasedChunks(minIndent int) (chunks []Tokens)
```
IndentBasedChunks breaks up `this` into a number of `chunks`: each
'non-indented' line (with `LineIndent` <= `minIndent`) in `this` begins a new
'chunk' and any subsequent 'indented' (`LineIndex` > `minIndent`) lines also
belong to it.

#### func (Tokens) SansComments

```go
func (this Tokens) SansComments() (sans Tokens)
```
SansComments returns the newly allocated `sans` with a `cap` of `len(this)` and
containing all `Tokens` in `this` except those with a `Kind` of `TOKEN_COMMENT`.

#### func (Tokens) String

```go
func (this Tokens) String() string
```

#### func (Tokens) Sub

```go
func (this Tokens) Sub(sepOpen string, sepClose string) (sub Tokens, tail Tokens, numUnclosed int)
```
Sub assumes (but won't check: up to the caller) that `this` begins with a
`TokenSep` of `sepOpen` and returns in `sub` the subsequence of `Tokens` up
until a matching `TokenSep` of `sepClose`. If no correct subsequence is found,
`sub` is `nil` and `tail` is `this` (and `numUnclosed` might be non-`0` to
indicate the number of unclosed groupings) — otherwise `sub` is the subsequence
immediately following the opening `sepOpen` up to and excluding the matching
`sepClose`, and `tail` is all trailing `Tokens` immediately following it.
