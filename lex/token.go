package udevlex

import (
	"strings"
)

// TokenKind enumerates the possible values that could be returned by `Token.Kind`.
type TokenKind = int

const (
	_ TokenKind = iota
	TOKEN_STR
	TOKEN_COMMENT
	TOKEN_FLOAT
	TOKEN_IDENT
	TOKEN_OPISH
	TOKEN_SEPISH
	TOKEN_UINT
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

	Kind TokenKind
}

func (me *Token) Pos(lineOffset int, posOffset int) *Pos {
	pos := me.Meta.Pos
	pos.Ln1 += lineOffset
	pos.Off0 += posOffset
	return &pos
}

func (me *Token) IsLineComment() bool {
	return strings.HasPrefix(me.Meta.Orig, ScannerLineCommentPrefix)
}

func (me *Token) IsLongComment() bool {
	halfway := len(ScannerLongCommentPrefixAndSuffix) / 2
	pref, suff := ScannerLongCommentPrefixAndSuffix[:halfway], ScannerLongCommentPrefixAndSuffix[halfway:]
	return strings.HasPrefix(me.Meta.Orig, pref) && strings.HasSuffix(me.Meta.Orig, suff)
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
	if should && me.Kind == TOKEN_SEPISH {
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
	Orig string
	Pos
	LineIndent int
}

func (me *TokenMeta) init(pos *Pos, indent int, orig string) {
	me.Pos, me.LineIndent, me.Orig = *pos, indent, orig
}
