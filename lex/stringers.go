package udevlex

import (
	"bytes"
	// "strconv"
)

func (me *TokenMeta) String() string {
	return me.Orig
}

// func (me *TokenComment) String() string {
// 	if me.SingleLine {
// 		return "//" + me.Token
// 	}
// 	return "/*" + me.Token + "*/"
// }

// func (me *TokenFloat) String() string {
// 	return strconv.FormatFloat(me.Token, 'g', -1, 64)
// }

// func (me *TokenUint) String() string {
// 	var pref string
// 	if me.Base == 16 {
// 		pref = "0x"
// 	} else if me.Base == 8 {
// 		pref = "0"
// 	}
// 	return pref + strconv.FormatUint(me.Token, me.Base)
// }

// func (me *TokenIdent) String() string {
// 	return me.Token
// }

// func (me *TokenSep) String() string {
// 	return me.Token
// }

// func (me *TokenOther) String() string {
// 	return me.Token
// }

// func (me *TokenRune) String() string {
// 	return strconv.QuoteRune(me.Token)
// }

// func (me *TokenStr) String() (s string) {
// 	return strconv.Quote(me.Token)
// }

func (me Tokens) String() string {
	if len(me) == 0 {
		return ""
	}
	var buf bytes.Buffer
	for _, tok := range me {
		buf.WriteRune('·')
		buf.WriteString(tok.String())
	}
	return buf.String()[1:]
}
