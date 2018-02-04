package udevlex

import (
	"fmt"
	"text/scanner"
)

// IToken is the interface implemented by the various `TokenFoo` structs in this package.
type IToken interface {
	fmt.Stringer
	init(*scanner.Position, int, string)
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

	// SingleLine denotes whether the comment started with `//` (as opposed to `/*`), it does not actively denote the existence or absence of actual line-breaks in `Token`.
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

// TokenOther holds a `string` that is a consecutive sequence (1 or more characters) of anything-not-fitting-other-token-types.
type TokenOther struct {
	Token string
	TokenMeta
}

// TokenSep holds a (uni-`rune`) `string` that matched one of `Lex`s specified `standAloneSeps`.
type TokenSep struct {
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
	Orig       string
}

func (me *TokenMeta) init(pos *scanner.Position, indent int, orig string) {
	me.Position, me.LineIndent, me.Orig = *pos, indent, orig
}

func (me *TokenMeta) Meta() *TokenMeta {
	return me
}
