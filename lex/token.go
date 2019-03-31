package udevlex

import (
	"text/scanner"
)

type TokenKind = int

const ( // note, order of enumerants in being relied-on in Kind()
	_ TokenKind = 16 + iota
	_TOKEN_STR_RAW
	_TOKEN_COMMENT_ENCL
	TOKEN_STR
	TOKEN_COMMENT
	TOKEN_FLOAT
	TOKEN_IDENT
	TOKEN_OTHER
	TOKEN_SEP
	TOKEN_RUNE
	TOKEN_UINT
)

type Token struct {
	Meta TokenMeta

	Str   string
	Float float64
	Uint  uint64

	flag int
}

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

func (me *Token) IsCommentSelfTerminating() bool {
	return me.flag == _TOKEN_COMMENT_ENCL
}

func (me *Token) IsStrRaw() bool {
	return me.flag == _TOKEN_STR_RAW
}

func (me *Token) Rune() (r rune) {
	return rune(me.Uint)
}

func (me *Token) UintBase() int {
	return me.flag
}

func (me *Token) String() string {
	return me.Meta.Orig
}

type TokenMeta struct {
	scanner.Position
	LineIndent int
	Orig       string
}

func (me *TokenMeta) init(pos *scanner.Position, indent int, orig string, lineOff int, posOff int) {
	me.Position, me.LineIndent, me.Orig = *pos, indent, orig
	me.Position.Line, me.Position.Offset = me.Position.Line+lineOff, me.Position.Offset+posOff
}
