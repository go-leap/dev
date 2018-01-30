package udevlex

import (
	"text/scanner"
)

type LexError struct {
	Pos scanner.Position
	msg string
}

func (me *LexError) Error() string  { return me.msg }
func (me *LexError) String() string { return me.Error() }

type Token interface {
	setPos(*scanner.Position)
}

type tokenBase struct {
	scanner.Position
}

func (me *tokenBase) setPos(pos *scanner.Position) { me.Position = *pos }

type TokenChar struct {
	tokenBase
	Token rune
}

type TokenComment struct {
	tokenBase
	Token string
}

type TokenFloat struct {
	tokenBase
	Token float64
}

type TokenIdent struct {
	tokenBase
	Token string
}

type TokenInt struct {
	tokenBase
	Token int64
}

type TokenOther struct {
	tokenBase
	Token string
}

type TokenStr struct {
	tokenBase
	Token string
}

type TokenUInt struct {
	tokenBase
	Token uint64
}
