# udevlex
--
    import "github.com/go-leap/dev/lex"


## Usage

```go
var (
	// SanitizeDirtyFloatsNextToDotOpishs, if `true`, will cause `Lex` to
	// call `Tokens.SanitizeDirtyFloatsNextToDotOpishs` on each lexeme produced.
	SanitizeDirtyFloatsNextToDotOpishs bool

	// RestrictedWhitespace causes lex errors when encountering standalone (outside
	// comment or string or character tokens) white-space tokens other than '\n' and ' '.
	RestrictedWhitespace bool

	// RestrictedWhitespaceRewriter, if set, is called instead of
	// raising a lexing error when `RestrictedWhitespace` is `true`.
	RestrictedWhitespaceRewriter func(rune) int

	// SepsGroupers, if it is to be used, must be set before the first call to
	// `Lex`, and must never be modified ever again for its consumers such as
	// `Tokens.Chunked`, `Tokens.BreakOnSpace`, `Tokens.Has`, `Tokens.Cliques`
	// to work correctly. It must be of even length beginning with all the
	// "openers" and ending with all the "closers": two equal-length halves
	// in one `string` such as "[(<{}>)]" or "«‹/\›»" etc.
	// ASCII bytes `< 128` only, no `>= 128` runes. Each is also only ever
	// lexed as a `TOKEN_SEPISH` and thus can never be part of a `TOKEN_OPISH`.
	// The mentioned methods skip their logic while passing tokens one or
	// several levels deep within these delimiters.
	SepsGroupers string

	// SepsOthers contains single-byte tokens that would ordinarily be lexed
	// as `TOKEN_OPISH`s and should instead be lexed as `TOKEN_SEPISH`s. As
	// such, they can never be part of multi-rune `TOKEN_OPISH`s and will
	// always stand alone in the resulting stream of lexemes. These are in
	// addition to any specified in `SepsGroupers`, for non-grouping solitary
	// non-operator seps such as eg. `,` or `;` etc.
	SepsOthers string
)
```

```go
var (
	ScannerLineCommentPrefix                 = "//"
	ScannerLongCommentPrefixAndSuffix        = "/**/"
	ScannerStringDelims               string = "'\""
)
```

#### func  Lex

```go
func Lex(src io.Reader, filePath string, toksCap int) (tokens Tokens, errs []*Error)
```
Lex returns the `Token`s lexed from `src`, or all `Error`s encountered while
lexing.

#### func  Scan

```go
func Scan(src string, srcFilePath string, on func(TokenKind, *Pos, int))
```

#### func  SepsGrouperCloserForOpener

```go
func SepsGrouperCloserForOpener(opener byte) (closer byte)
```

#### type Error

```go
type Error struct {
	Msg string
	Pos scanner.Position
}
```

Error holds a message obtained via `Scanner.Error`, plus additional positional
details.

#### func  Err

```go
func Err(pos *scanner.Position, msg string) *Error
```
Err returns a newly constructed `Error` with the given `Msg` and `Pos`.

#### func (*Error) Error

```go
func (me *Error) Error() string
```
Error implements Go's standard `error` interface.

#### type Pos

```go
type Pos struct {
	FilePath string
	Offset0  int
	Line1    int
	Col1     int
}
```


#### type Token

```go
type Token struct {
	// Meta holds a `Token`'s `Position`, `LineIndent` and `Orig` source sub-string.
	Meta TokenMeta

	// Str is set for non-number-literal lexemes.
	Str string
	// Float is only set if `Kind` returns `TOKEN_FLOAT`.
	Float float64
	// Uint is only set if `Kind` returns `TOKEN_UINT` or `TOKEN_RUNE`.
	Uint uint64
}
```

Token represents a lexeme.

#### func (*Token) IsAnyOneOf

```go
func (me *Token) IsAnyOneOf(any ...string) bool
```
IsAnyOneOf returns whether some value in `any` is equal to this `Token`'s
original source sub-string.

#### func (*Token) IsCommentSelfTerminating

```go
func (me *Token) IsCommentSelfTerminating() bool
```
IsCommentSelfTerminating returns `false` for `// ...` single-line comments, and
`true` for `/* ... */` multi-line comments.

#### func (*Token) IsStrRaw

```go
func (me *Token) IsStrRaw() bool
```
IsStrRaw returns whether this `Token` of `TOKEN_STR` `Kind` had backtick
delimiters.

#### func (*Token) Kind

```go
func (me *Token) Kind() (kind TokenKind)
```
Kind returns this `Token`'s `TokenKind`.

#### func (*Token) Or

```go
func (me *Token) Or(fallback *Token) *Token
```
Or returns `me` if not `nil`, else `fallback`.

#### func (*Token) Pos

```go
func (me *Token) Pos(lineOffset int, posOffset int) *scanner.Position
```

#### func (*Token) Rune

```go
func (me *Token) Rune() (r rune)
```
Rune returns the `rune` represented by this `Token` of `TOKEN_RUNE` `Kind`.

#### func (*Token) String

```go
func (me *Token) String() string
```
String returns the original source sub-string that this `Token` was produced
from.

#### func (*Token) UintBase

```go
func (me *Token) UintBase() int
```
UintBase returns the base of this `Token` with `TOKEN_UINT` `Kind`, ie. 10 for
decimal, 16 for hexadecimal, 8 for octal base etc. (For a `Token` of a different
`Kind`, the return value is usually the `Kind` itself.)

#### type TokenKind

```go
type TokenKind = int
```

TokenKind enumerates the possible values that could be returned by `Token.Kind`.

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
	TOKEN_OTHER
	TOKEN_SPACE
)
```

#### type TokenMeta

```go
type TokenMeta struct {
	Pos        scanner.Position
	LineIndent int
	Orig       string
}
```

TokenMeta provides a `Token`'s `Position`, `LineIndent` and `Orig` source
sub-string.

#### type Tokens

```go
type Tokens []Token
```

Tokens is a slice of `Token`s.

#### func (Tokens) AreEnclosing

```go
func (me Tokens) AreEnclosing(pos0ByteOffset int) bool
```
AreEnclosing returns whether the `Tokens` enclose the specified 0-based,
byte-based offset position.

#### func (Tokens) Between

```go
func (me Tokens) Between(after *Token, before *Token) (slice Tokens)
```
Between returns the `Tokens` that come after `after` and before `before`.

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
BreakOnLeadingComments returns the `leadingComments` (which could be empty) as
well as the `rest`.

#### func (Tokens) BreakOnOpish

```go
func (me Tokens) BreakOnOpish(token string) (pref Tokens, op *Token, suff Tokens)
```
BreakOnOpish returns all `Tokens` preceding and succeeding the next occurence of
the specified `TokenOther` in `me`, if any — otherwise, `me,nil` will be
returned.

#### func (Tokens) BreakOnSpace

```go
func (me Tokens) BreakOnSpace(deep bool) (pref Tokens, suff Tokens, didBreak bool)
```
BreakOnSpace splits up `me` into `pref` and `suff` between the first two
consecutive `Tokens` that suggest white-space in between each other. If no such
pair exists, `didBreak` is `false` and `pref` is `nil` and `suff` is `me`.

#### func (Tokens) Chunked

```go
func (me Tokens) Chunked(byOrig string, stopChunkingOn string) (chunks []Tokens)
```
Chunked splits `me` into `chunks` separated by `TokenMeta.Orig` occurrences of
`byOrig`, stopping at the first occurrence of `stopChunkingOn` (if specified).

#### func (Tokens) Cliques

```go
func (me Tokens) Cliques(isBreaker func(idxCur int, idxLast int) bool) (nums map[*Token]int)
```
Cliques records in `nums` any `Token` in `me` that begins a sequence of
non-white-space-separated lexemes and its number of `Tokens` If no such sequence
exists or if it would equal `me` entirely, `nums` will be `nil`, else any value
in it will be `> 1`. If `isBreaker` isn't `nil`, it can identify
language-specific tokens that break up this logic, such as `,`.

#### func (Tokens) CountKind

```go
func (me Tokens) CountKind(kind TokenKind) (count int)
```
CountKind returns the number of `Token`s with the specified `Kind`.

#### func (Tokens) EqLenAndOffsets

```go
func (me Tokens) EqLenAndOffsets(toks Tokens, checkInnerOffsetsToo bool) bool
```
EqLenAndOffsets returns at least whether `me` and `toks` have the same `len` and
the `First` and `Last` of both share the same `TokenMeta.Pos.Offset`. If
`checkInnerOffsetsToo` is `true`, all other `Tokens` (not just the `First` and
`Last` ones) are compared as well.

#### func (Tokens) FindSub

```go
func (me Tokens) FindSub(beginsWith Tokens, endsWith Tokens) (slice Tokens)
```
FindSub initially calls `FromUntil` but if the result is `nil` because
`beginsWith` / `endsWith` aren't sub-slices of `me`, it figures out the proper
beginner/ender from `TokenMeta.Pos.Offset` values of the `First(nil)` of
`beginsWith` and the `Last(nil)` of `endsWith`. In any case, only the first
`Token` in `beginsWith` and the last in `endsWith` are ever considered.

#### func (Tokens) First

```go
func (me Tokens) First(matches func(*Token) bool) *Token
```
First returns the first `Token` if `matches` is `nil`, else the first for which
it returns `true`.

#### func (Tokens) First1

```go
func (me Tokens) First1() (r *Token)
```

#### func (Tokens) FromUntil

```go
func (me Tokens) FromUntil(from *Token, until *Token, incl bool) Tokens
```
FromUntil returns the `Tokens` from `from` (or the beginning, if `nil`) until
`until` (or the end, if `nil`). If `incl` is `true`, `until` is included in
`slice`.

#### func (Tokens) Has

```go
func (me Tokens) Has(orig string, deep bool) bool
```
Has returns whether any of the `Tokens` was produced from the specified original
source sub-string.

#### func (Tokens) HasKind

```go
func (me Tokens) HasKind(kind TokenKind) bool
```
HasKind returns whether any of the `Tokens` is of the specified `Kind`.

#### func (Tokens) HasSpaces

```go
func (me Tokens) HasSpaces() bool
```
HasSpaces returns whether any two consecutive `Tokens` suggest that there is
white-space in between each other.

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
IsAnyOneOf calls `Token.IsAnyOneOf` is `me` has only one `Token`.

#### func (Tokens) Last

```go
func (me Tokens) Last(matches func(*Token) bool) *Token
```
Last returns the last `Token` if `matches` is `nil`, else the last one for which
it returns `true`.

#### func (Tokens) Last1

```go
func (me Tokens) Last1() (r *Token)
```

#### func (Tokens) Length

```go
func (me Tokens) Length() (length int)
```
Length returns the length of the original source sub-string that this `Tokens`
slice represents (without traversing it).

#### func (Tokens) Next

```go
func (me Tokens) Next(after *Token, fallback bool) *Token
```

#### func (Tokens) NumCharsBetweenFirstAndLastOf

```go
func (me Tokens) NumCharsBetweenFirstAndLastOf(other Tokens) (dist int)
```
NumCharsBetweenFirstAndLastOf returns the number of characters between the first
`Token` in `me` and the end of the last `Token` in `other`.

#### func (Tokens) NumCharsBetweenLastAndFirstOf

```go
func (me Tokens) NumCharsBetweenLastAndFirstOf(other Tokens) (dist int)
```
NumCharsBetweenLastAndFirstOf returns the number of characters between the first
`Token` in `other` and the end of the last `Token` in `me`.

#### func (Tokens) Pos

```go
func (me Tokens) Pos() *scanner.Position
```
Pos returns the `TokenMeta.Pos` of the `First` `Token` in `me`.

#### func (Tokens) Prev

```go
func (me Tokens) Prev(before *Token, fallback bool) *Token
```

#### func (*Tokens) SanitizeDirtyFloatsNextToDotOpishs

```go
func (me *Tokens) SanitizeDirtyFloatsNextToDotOpishs(i int)
```
SanitizeDirtyFloatsNextToDotOpishs attempts to undo sometimes-unwanted float
tokenizations by `text/scanner`, such as `0..1` into `0.0` and `0.1`, or `0...1`
into `0.0` and `.` and `0.1`, replacing with the corresponding uint / dot
`Token` combinations instead. `i > 0 && i < len(me)` must hold.

#### func (Tokens) SansComments

```go
func (me Tokens) SansComments(keepIn map[*Token][]int, oldIndices map[*Token]int) (sans Tokens)
```
SansComments returns the newly allocated `sans` with a `cap` of `len(me)` and
containing all `Tokens` in `me` except those with a `Kind` of `TOKEN_COMMENT`.

If `keepIn` is not `nil`, it is filled with all non-comment `Token`s in `me`
mapped to the indices (in `me`) of their subsequent comment `Token`s.

If `oldIndices` is not `nil`, it keeps track of the original indices in `me`.

#### func (Tokens) String

```go
func (me Tokens) String() string
```
String returns a representation of `Tokens` handy for troubleshooting /
diagnostics.

#### func (Tokens) Sub

```go
func (me Tokens) Sub(sepOpen byte, sepClose byte) (sub Tokens, tail Tokens, numUnclosed int)
```
Sub assumes (but won't check: up to the caller) that `me` begins with a
`TOKEN_SEPISH` of `sepOpen` and returns in `sub` the subsequence of `Tokens` up
until a matching `TOKEN_SEPISH` of `sepClose`. If no correct subsequence is
found, `sub` is `nil` and `tail` is `me` (and `numUnclosed` might be non-`0` to
indicate the number of unclosed groupings) — otherwise `sub` is the subsequence
immediately following the opening `sepOpen` up to and excluding the matching
`sepClose`, and `tail` is all trailing `Tokens` immediately following it.
