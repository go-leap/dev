package udevlex

import (
	"strings"
	"unicode/utf8"
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
	Lexeme string
	Pos
	LineIndent int
	Kind       TokenKind

	// Val is `uint64` or `float64` or `string` for literals and comments, else `nil`
	Val interface{}
}

func (me *Token) init(pos *Pos, indent int, orig string) {
	me.Pos, me.LineIndent, me.Lexeme = *pos, indent, orig
}

func (me *Token) OffPos(lineOffset int, posOffset int) *Pos {
	pos := me.Pos
	pos.Ln1 += lineOffset
	pos.Off0 += posOffset
	return &pos
}

func (me *Token) OffPosEnd(lineOffset int, posOffset int) *Pos {
	pos := me.Pos
	pos.Off0 += len(me.Lexeme) + posOffset
	pos.Ln1 += lineOffset
	pos.Col1 += utf8.RuneCountInString(me.Lexeme)
	if ilb := strings.LastIndexByte(me.Lexeme, '\n'); ilb >= 0 {
		for i := 0; i < len(me.Lexeme); i++ {
			if me.Lexeme[i] == '\n' {
				pos.Ln1++
			}
		}
		pos.Col1 = utf8.RuneCountInString(me.Lexeme[ilb:])
	}
	return &pos
}

func (me *Token) IsLineComment() bool {
	return strings.HasPrefix(me.Lexeme, ScannerLineCommentPrefix)
}

func (me *Token) IsLongComment() bool {
	halfway := len(ScannerLongCommentPrefixAndSuffix) / 2
	pref, suff := ScannerLongCommentPrefixAndSuffix[:halfway], ScannerLongCommentPrefixAndSuffix[halfway:]
	return strings.HasPrefix(me.Lexeme, pref) && strings.HasSuffix(me.Lexeme, suff)
}

func (me *Token) CanMultiLine() bool {
	return me.Kind == TOKEN_STR || (me.Kind == TOKEN_COMMENT && !strings.HasPrefix(me.Lexeme, ScannerLineCommentPrefix))
}

func (me *Token) MultiLine() bool {
	return me.CanMultiLine() && strings.IndexByte(me.Lexeme, '\n') > 0
}

func (me *Token) NumLFs() (ret int) {
	if me.CanMultiLine() {
		if idx := strings.IndexByte(me.Lexeme, '\n'); idx > 0 {
			ret = 1
			for i := idx + 1; i < len(me.Lexeme); i++ {
				if me.Lexeme[i] == '\n' {
					ret++
				}
			}
		}
	}
	return
}

// Or returns `me` if not `nil`, else `fallback`.
func (me *Token) Or(fallback *Token) *Token {
	if me == nil {
		return fallback
	}
	return me
}

// String returns the original source sub-string that this `Token` was produced from.
func (me *Token) String() string {
	return me.Lexeme
}

// IsAnyOneOf returns whether some value in `any` is equal to this `Token`'s original source sub-string.
func (me *Token) IsAnyOneOf(any ...string) bool {
	for i := range any {
		if me.Lexeme == any[i] {
			return true
		}
	}
	return false
}

func (me *Token) sepsDepthIncrement(should bool) int {
	if should && me.Kind == TOKEN_SEPISH {
		if at := strings.IndexByte(SepsGroupers, me.Lexeme[0]); at >= 0 {
			if at < idxSepsGroupersClosers {
				return 1
			} else {
				return -1
			}
		}
	}
	return 0
}
