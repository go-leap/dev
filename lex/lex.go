package udevlex

import (
	"strconv"
	"strings"
	"text/scanner"
)

func Lex(filePath string, src string) (tokenStream []Token, errs []*LexError) {
	tokenStream = make([]Token, 0, len(src)/4) // a shot in the dark for an initial cap that's better than default 0. could be sub-optimal for source files of several 100s of MB — revisit when that becomes realistic/common

	var lexer scanner.Scanner
	lexer.Init(strings.NewReader(src)).Filename = filePath
	lexer.Mode = scanner.ScanChars | scanner.ScanComments | scanner.ScanFloats | scanner.ScanIdents | scanner.ScanInts | scanner.ScanRawStrings | scanner.ScanStrings
	lexer.Error = func(_ *scanner.Scanner, msg string) {
		err := &LexError{msg: msg, Pos: lexer.Position}
		err.Pos.Filename = filePath
		tokenStream, errs = nil, append(errs, err)
	}

	on := func(token Token) {
		if len(errs) == 0 {
			token.setPos(&lexer.Position)
			tokenStream = append(tokenStream, token)
		}
	}

	for tok := lexer.Scan(); tok != scanner.EOF; tok = lexer.Scan() {
		sym := lexer.TokenText()
		switch tok {
		case scanner.Ident:
			on(&TokenIdent{Token: sym})
		case scanner.Char:
			if c, _, _, errchr := strconv.UnquoteChar(sym[1:], '\''); errchr == nil {
				on(&TokenChar{Token: c})
			} else {
				lexer.Error(nil, errchr.Error())
			}
		case scanner.RawString, scanner.String:
			if s, errstr := strconv.Unquote(sym); errstr == nil {
				on(&TokenStr{Token: s})
			} else {
				lexer.Error(nil, errstr.Error())
			}
		case scanner.Float:
			if f, errfloat := strconv.ParseFloat(sym, 64); errfloat == nil {
				on(&TokenFloat{Token: f})
			} else {
				lexer.Error(nil, errfloat.Error())
			}
		case scanner.Int:
			if i, errint := strconv.ParseInt(sym, 0, 64); errint == nil {
				on(&TokenInt{Token: i})
			} else if errnum, _ := errint.(*strconv.NumError); errnum == nil || errnum.Err != strconv.ErrRange {
				lexer.Error(nil, errint.Error())
			} else if u, erruint := strconv.ParseUint(sym, 0, 64); erruint == nil {
				on(&TokenUInt{Token: u})
			} else {
				lexer.Error(nil, erruint.Error())
			}
		case scanner.Comment:
			if strings.HasPrefix(sym, "//") {
				on(&TokenComment{Token: sym[2:]})
			} else if strings.HasPrefix(sym, "/*") && strings.HasSuffix(sym, "*/") {
				on(&TokenComment{Token: sym[2 : len(sym)-2]})
			} else {
				lexer.Error(nil, "unexpected comment format")
			}
		default:
			on(&TokenOther{Token: sym})
		}
	}
	return
}
