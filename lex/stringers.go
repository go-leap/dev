package udevlex

import (
	"strconv"
)

func (me *TokenComment) String() string {
	if me.SingleLine {
		return "[//]" + me.Token
	}
	return "[/*]" + me.Token
}

func (me *TokenFloat) String() string {
	return "[Lf]" + strconv.FormatFloat(me.Token, 'g', -1, 64)
}

func (me *TokenInt) String() string {
	return "[Li]" + strconv.FormatInt(me.Token, 10)
}

func (me *TokenUint) String() string {
	return "[Lu]" + strconv.FormatUint(me.Token, 10)
}

func (me *TokenIdent) String() string {
	return "[Id]" + me.Token
}

func (me *TokenOther) String() string {
	return "[??]" + me.Token
}

func (me *TokenRune) String() string {
	return "[Lr]" + string(me.Token)
}

func (me *TokenStr) String() string {
	return "[Lt]" + me.Token
}
