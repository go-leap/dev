package udevlex

import (
	"text/scanner"
)

// LexError holds a message returned by `Error` and `String`, plus additional positional details.
type LexError struct {
	msg string
	Pos scanner.Position
}

// Error implements the `error` interface.
func (me *LexError) Error() string { return me.msg }

// String implements the `fmt.Stringer` interface.
func (me *LexError) String() string { return me.Error() }

// Token is the interface implemented by the various `TokenFoo` structs in this package.
type Token interface {
	init(*scanner.Position, int)
}

// TokenRune holds a `rune` that was scanned from a quoted literal.
type TokenRune struct {
	TokenMeta
	Token rune
}

// TokenComment holds a comment `string` that was scanned from a `// ..` or `/* .. */` fragment, sans the separators.
type TokenComment struct {
	Token string
	TokenMeta

	// SingleLine denotes whether the comment started with `//` (as opposed to `/*`), it does not actually reflect the number of lines in `Token`.
	SingleLine bool
}

// TokenFloat holds a `float64` that was scanned from a literal.
type TokenFloat struct {
	TokenMeta
	Token float64
}

// TokenIdent holds a `string` that was scanned from an unquoted alphanumeric range of characters.
type TokenIdent struct {
	Token string
	TokenMeta
}

// TokenInt holds an `int64` that was scanned from a literal.
type TokenInt struct {
	TokenMeta
	Token int64
}

// TokenOther holds (typically, but not guaranteed, uni-`rune`) `string`s that are theoretically anything-not-fitting-other-token-types, but in practice for the most part typically interpreted as operator, separation or punctuation characters.
type TokenOther struct {
	Token string
	TokenMeta
}

// TokenStr holds the unquoted `string` that was scanned from a quoted literal.
type TokenStr struct {
	Token string
	TokenMeta
}

// TokenUInt holds an `uint64` that was scanned from a literal exceeding the maximum-possible `int64`.
type TokenUInt struct {
	TokenMeta
	Token uint64
}

// TokenMeta is embedded by all `Token` implementers.
type TokenMeta struct {
	scanner.Position
	LineIndent int
}

func (me *TokenMeta) init(pos *scanner.Position, indent int) {
	me.Position, me.LineIndent = *pos, indent
}
