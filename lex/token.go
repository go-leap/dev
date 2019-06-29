package udevlex

import (
	"strings"
	"text/scanner"
)

// TokenKind enumerates the possible values that could be returned by `Token.Kind`.
type TokenKind = int

const ( // note, order of enumerants is being relied-on in Kind()
	_ TokenKind = 36 + iota
	_TOKEN_STR_RAW
	_TOKEN_COMMENT_ENCL
	TOKEN_STR
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

// Token represents a lexeme.
type Token struct {
	// Meta holds a `Token`'s `Position`, `LineIndent` and `Orig` source sub-string.
	Meta TokenMeta

	// Str is set for non-number-literal lexemes.
	Str string
	// Float is only set if `Kind` returns `TOKEN_FLOAT`.
	Float float64
	// Uint is only set if `Kind` returns `TOKEN_UINT` or `TOKEN_RUNE`.
	Uint uint64

	flag int
}

// Kind returns this `Token`'s `TokenKind`.
func (me *Token) Kind() (kind TokenKind) {
	if kind = me.flag; kind < TOKEN_STR {
		if kind < _TOKEN_STR_RAW {
			kind = TOKEN_UINT
		} else if kind == _TOKEN_COMMENT_ENCL {
			kind = TOKEN_COMMENT
		} else if kind == _TOKEN_STR_RAW {
			kind = TOKEN_STR
		}
	}
	return
}

func (me *Token) Pos(lineOffset int, posOffset int) *scanner.Position {
	pos := me.Meta.Pos
	pos.Line += lineOffset
	pos.Offset += posOffset
	return &pos
}

// IsCommentSelfTerminating returns `false` for `// ...` single-line comments,
// and  `true` for `/* ... */` multi-line comments.
func (me *Token) IsCommentSelfTerminating() bool {
	return me.flag == _TOKEN_COMMENT_ENCL
}

// IsStrRaw returns whether this `Token` of `TOKEN_STR`
// `Kind` had backtick delimiters.
func (me *Token) IsStrRaw() bool {
	return me.flag == _TOKEN_STR_RAW
}

// Or returns `me` if not `nil`, else `fallback`.
func (me *Token) Or(fallback *Token) *Token {
	if me == nil {
		return fallback
	}
	return me
}

// Rune returns the `rune` represented by this `Token` of `TOKEN_RUNE` `Kind`.
func (me *Token) Rune() (r rune) {
	return rune(me.Uint)
}

// UintBase returns the base of this `Token` with `TOKEN_UINT` `Kind`, ie.
// 10 for decimal, 16 for hexadecimal, 8 for octal base etc. (For a `Token`
// of a different `Kind`, the return value is usually the `Kind` itself.)
func (me *Token) UintBase() int {
	return me.flag
}

// String returns the original source sub-string that this `Token` was produced from.
func (me *Token) String() string {
	return me.Meta.Orig
}

// IsAnyOneOf returns whether some value in `any` is equal to this `Token`'s original source sub-string.
func (me *Token) IsAnyOneOf(any ...string) bool {
	for i := range any {
		if me.Meta.Orig == any[i] {
			return true
		}
	}
	return false
}

// caution, a global. only mutated by Lex(), users are warned in doc-comments
// for SepsGroupers to set it before Lex call and never mutate it afterwards.
// The consumer of this is just called a lot and I cannot accept doing the
// always-exact-same by-2 division over and over and over again uselessly.
var idxSepsGroupersClosers int

func (me *Token) sepsDepthIncrement(should bool) int {
	if should && me.flag == TOKEN_SEPISH {
		if at := strings.IndexByte(SepsGroupers, me.Meta.Orig[0]); at >= 0 {
			if at < idxSepsGroupersClosers {
				return 1
			} else {
				return -1
			}
		}
	}
	return 0
}

// TokenMeta provides a `Token`'s `Position`, `LineIndent` and `Orig` source sub-string.
type TokenMeta struct {
	Pos        scanner.Position
	LineIndent int
	Orig       string
}

func (me *TokenMeta) init(pos *scanner.Position, indent int, orig string) {
	me.Pos, me.LineIndent, me.Orig = *pos, indent, orig
	me.Pos.Line, me.Pos.Offset = me.Pos.Line, me.Pos.Offset
}
