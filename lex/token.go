package udevlex

import (
	"text/scanner"
)

type TokenKind = int

const ( // note, order of enumerants in being relied-on in Kind()
	_ TokenKind = 16 + iota
	_TOKEN_STR_RAW
	_TOKEN_COMMENT_LONG
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

func (this *Token) Kind() (kind TokenKind) {
	if kind = this.flag; kind < TOKEN_STR {
		if kind < _TOKEN_STR_RAW {
			kind = TOKEN_UINT
		} else if kind == _TOKEN_COMMENT_LONG {
			kind = TOKEN_COMMENT
		} else if kind == _TOKEN_STR_RAW {
			kind = TOKEN_STR
		}
	}
	return
}

func (this *Token) IsCommentLong() bool {
	return this.flag == _TOKEN_COMMENT_LONG
}

func (this *Token) IsStrRaw() bool {
	return this.flag == _TOKEN_STR_RAW
}

func (this *Token) Rune() (r rune) {
	return rune(this.Uint)
}

func (this *Token) UintBase() int {
	return this.flag
}

func (this *Token) String() string {
	return this.Meta.Orig
}

type TokenMeta struct {
	scanner.Position
	LineIndent int
	Orig       string
}

func (this *TokenMeta) init(pos *scanner.Position, indent int, orig string) {
	this.Position, this.LineIndent, this.Orig = *pos, indent, orig
}
