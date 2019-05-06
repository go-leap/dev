# udevlex
--
    import "github.com/go-leap/dev/lex"


## Usage

```go
var (
	SanitizeDirtyFloatsNextToOpishs bool

	// RestrictedWhitespace causes lex errors when encountering standalone (outside
	// comment or string or character tokens) white-space tokens other than '\n' and ' '.
	RestrictedWhitespace bool

	// RestrictedWhitespaceRewriter, if set, is called instead of
	// raising a lexing error when `RestrictedWhitespace` is `true`.
	RestrictedWhitespaceRewriter func(rune) int

	// StandaloneSeps contains single-rune token strings that would ordinarily
	// be lexed as `TOKEN_OPISH`s and should instead be lexed as `TOKEN_SEPISH`s.
	// As such, they can never be part of multi-rune `TOKEN_OPISH`s either
	// and will always stand alone in the resulting stream of lexemes.
	StandaloneSeps []string

	// SepsForChunking is used by `Tokens.Chunked` and must be of even length
	// beginning with all the openers and ending with all the closers, ie. both
	// equal-length halves joined together such as "[(<{}>)]" or "«‹/\›»" etc.
	SepsForChunking string
)
```

#### func  Lex

```go
func Lex(src io.Reader, filePath string, lineOff int, posOff int, toksCap int) (tokens Tokens, errs []*Error)
```
Lex returns the `Token`s lexed from `src`, or all `Error`s encountered while
lexing.

If `errs` has a `len` greater than 0, `tokens` will be empty (and vice versa).

#### type Error

```go
type Error struct {
	Msg string
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


#### func (*Token) IsAnyOneOf

```go
func (me *Token) IsAnyOneOf(any ...string) bool
```

#### func (*Token) IsCommentSelfTerminating

```go
func (me *Token) IsCommentSelfTerminating() bool
```

#### func (*Token) IsStrRaw

```go
func (me *Token) IsStrRaw() bool
```

#### func (*Token) Kind

```go
func (me *Token) Kind() (kind TokenKind)
```

#### func (*Token) Or

```go
func (me *Token) Or(ifMeIsNilThenReturn *Token) *Token
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
	TOKEN_OPISH
	TOKEN_SEPISH
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
func (me Tokens) BreakOnIndent(minLineIndent int) (indented Tokens, outdented Tokens)
```
BreakOnIndent returns in `indented` all `Tokens` on the same line as the first
in `me`, plus all subsequent `Tokens` with `LineIndent` greater than
`minLineIndent`; and in `outdented` the first and all following `Tokens` with a
`LineIndent` less-or-equal (if any).

#### func (Tokens) BreakOnLeadingComments

```go
func (me Tokens) BreakOnLeadingComments() (leadingComments Tokens, rest Tokens)
```

#### func (Tokens) BreakOnOpish

```go
func (me Tokens) BreakOnOpish(token string) (pref Tokens, op *Token, suff Tokens)
```
BreakOnOpish returns all `Tokens` preceding and succeeding the next occurence of
the specified `TokenOther` in `me`, if any — otherwise, `me,nil` will be
returned.

#### func (Tokens) BreakOnSpace

```go
func (me Tokens) BreakOnSpace(sepOpen byte, sepClose byte) (pref Tokens, suff Tokens, didBreak bool)
```

#### func (Tokens) Chunked

```go
func (me Tokens) Chunked(byOrig string, stopChunkingOn string) (chunks []Tokens)
```

#### func (Tokens) ChunkedBySpacing

```go
func (me Tokens) ChunkedBySpacing(sepOpen byte, sepClose byte, breaker string) (m map[*Token]int)
```

#### func (Tokens) CountKind

```go
func (me Tokens) CountKind(kind TokenKind) (count int)
```

#### func (Tokens) FindSub

```go
func (me Tokens) FindSub(beginsWith Tokens, endsWith Tokens) (slice Tokens)
```

#### func (Tokens) First

```go
func (me Tokens) First(matches func(*Token) bool) *Token
```

#### func (Tokens) FromUntil

```go
func (me Tokens) FromUntil(from *Token, until *Token, incl bool) (slice Tokens)
```

#### func (Tokens) Has

```go
func (me Tokens) Has(orig string) bool
```

#### func (Tokens) HasKind

```go
func (me Tokens) HasKind(kind TokenKind) bool
```

#### func (Tokens) HasSpaces

```go
func (me Tokens) HasSpaces() bool
```

#### func (Tokens) IndentBasedChunks

```go
func (me Tokens) IndentBasedChunks(minLineIndent int) (chunks []Tokens)
```
IndentBasedChunks breaks up `me` into a number of `chunks`: each 'non-indented'
line (with `LineIndent` <= `minLineIndent`) in `me` begins a new 'chunk' and any
subsequent 'indented' (`LineIndent` > `minLineIndent`) lines also belong to it.

#### func (Tokens) IsAnyOneOf

```go
func (me Tokens) IsAnyOneOf(any ...string) bool
```

#### func (Tokens) Last

```go
func (me Tokens) Last(matches func(*Token) bool) *Token
```

#### func (Tokens) Length

```go
func (me Tokens) Length() (length int)
```

#### func (Tokens) NumCharsBetweenFirstAndLastOf

```go
func (me Tokens) NumCharsBetweenFirstAndLastOf(other Tokens) (dist int)
```

#### func (Tokens) NumCharsBetweenLastAndFirstOf

```go
func (me Tokens) NumCharsBetweenLastAndFirstOf(other Tokens) (dist int)
```

#### func (Tokens) Pos

```go
func (me Tokens) Pos() *scanner.Position
```

#### func (Tokens) SansComments

```go
func (me Tokens) SansComments(keepIn map[*Token][]int, oldIndices map[*Token]int) (sans Tokens)
```
SansComments returns the newly allocated `sans` with a `cap` of `len(me)` and
containing all `Tokens` in `me` except those with a `Kind` of `TOKEN_COMMENT`.

If `keepIn` is not `nil`, it is filled with all non-comment `Token`s in `me`
mapped to the indices (in `me`) of their subsequent comment `Token`s.

If `oldIndices` is not `nil`, it keeps track of the original indices in `me`.

#### func (Tokens) SpacesBetween

```go
func (me Tokens) SpacesBetween(idxEarlier int, idxLater int) (numSpaces int)
```

#### func (Tokens) String

```go
func (me Tokens) String() string
```

#### func (Tokens) Sub

```go
func (me Tokens) Sub(sepOpen byte, sepClose byte) (sub Tokens, tail Tokens, numUnclosed int)
```
Sub assumes (but won't check: up to the caller) that `me` begins with a
`TokenSep` of `sepOpen` and returns in `sub` the subsequence of `Tokens` up
until a matching `TokenSep` of `sepClose`. If no correct subsequence is found,
`sub` is `nil` and `tail` is `me` (and `numUnclosed` might be non-`0` to
indicate the number of unclosed groupings) — otherwise `sub` is the subsequence
immediately following the opening `sepOpen` up to and excluding the matching
`sepClose`, and `tail` is all trailing `Tokens` immediately following it.
