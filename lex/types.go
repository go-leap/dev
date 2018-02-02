package udevlex

import (
	"fmt"
	"text/scanner"
)

func Err(pos *scanner.Position, msg string) *Error {
	return &Error{Pos: *pos, msg: msg}
}

// Error holds a message returned by `Error` and `String`, plus additional positional details.
type Error struct {
	msg string
	Pos scanner.Position
}

// Error implements the `error` interface.
func (me *Error) Error() string { return me.msg }

type IPos interface {
	Pos() *TokenMeta
}

// IToken is the interface implemented by the various `TokenFoo` structs in this package.
type IToken interface {
	fmt.Stringer
	IPos
	init(*scanner.Position, int)
	Meta() *TokenMeta
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

// TokenFloat holds a `float64` that was scanned from a floating-point literal.
type TokenFloat struct {
	TokenMeta
	Token float64
}

// TokenIdent holds a `string` that was scanned from an unquoted alphanumeric range of characters.
type TokenIdent struct {
	Token string
	TokenMeta
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
	Raw bool
}

// TokenUint holds an `uint64` that was scanned from an integral literal.
type TokenUint struct {
	TokenMeta
	Base  int
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

func (me *TokenMeta) Meta() *TokenMeta {
	return me
}

func (me *TokenMeta) Pos() *TokenMeta {
	return me
}
