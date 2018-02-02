package udevlex

import (
	"strconv"
)

func (me *TokenComment) String() string {
	if me.SingleLine {
		return "//" + me.Token
	}
	return "/*" + me.Token + "*/"
}

func (me *TokenFloat) String() string {
	return strconv.FormatFloat(me.Token, 'g', -1, 64)
}

func (me *TokenInt) String() string {
	return strconv.FormatInt(me.Token, 10)
}

func (me *TokenUint) String() string {
	return strconv.FormatUint(me.Token, 10)
}

func (me *TokenIdent) String() string {
	return me.Token
}

func (me *TokenOther) String() string {
	return me.Token
}

func (me *TokenRune) String() string {
	return strconv.QuoteRune(me.Token)
}

func (me *TokenStr) String() string {
	return strconv.Quote(me.Token)
}
