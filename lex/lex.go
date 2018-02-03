package udevlex

import (
	"strconv"
	"strings"
	"text/scanner"
	"unicode"
)

// Lex returns the `Token`s lexed from `src`, or all `LexError`s encountered while lexing.
//
// If `errs` has a `len` greater than 0, `tokens` will be empty (and vice versa).
func Lex(filePath string, src string, standAloneSeps ...string) (tokens Tokens, errs []*Error) {
	tokens = make(Tokens, 0, len(src)/4) // a shot in the dark for an initial cap that's better than default 0. could be sub-optimal for source files of several 100s of MB — revisit when that becomes realistic/common
	var (
		onlyspacesinlinesofar = true
		lineindent            int
		lexer                 scanner.Scanner
		otheraccum            *TokenOther
	)
	lexer.Init(strings.NewReader(src)).Filename = filePath
	lexer.Whitespace, lexer.Mode = 1<<'\r', scanner.ScanChars|scanner.ScanComments|scanner.ScanFloats|scanner.ScanIdents|scanner.ScanInts|scanner.ScanRawStrings|scanner.ScanStrings
	lexer.Error = func(_ *scanner.Scanner, msg string) {
		err := Err(&lexer.Position, msg)
		err.Pos.Filename = filePath
		tokens, errs = nil, append(errs, err)
	}

	on := func(token IToken) {
		if otheraccum != nil {
			if len(errs) == 0 {
				tokens = append(tokens, otheraccum)
			}
			otheraccum = nil
		}
		if onlyspacesinlinesofar = false; len(errs) == 0 && token != nil {
			token.init(&lexer.Position, lineindent)
			tokens = append(tokens, token)
		}
	}
	for tok := lexer.Scan(); tok != scanner.EOF; tok = lexer.Scan() {
		sym := lexer.TokenText()
		switch tok {
		case scanner.Ident:
			on(&TokenIdent{Token: sym})
		case scanner.Char:
			if c, _, _, errchr := strconv.UnquoteChar(sym[1:], '\''); errchr == nil {
				on(&TokenRune{Token: c})
			} else {
				lexer.Error(nil, errchr.Error())
			}
		case scanner.RawString, scanner.String:
			if s, errstr := strconv.Unquote(sym); errstr == nil {
				if tok != scanner.RawString && strings.HasPrefix(sym, "`") && strings.HasSuffix(sym, "`") {
					tok = scanner.RawString
				}
				on(&TokenStr{Token: s, Raw: tok == scanner.RawString})
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
			var base, i int
			if l := len(sym); l > 2 && sym[0] == '0' && (sym[1] == 'x' || sym[1] == 'X') {
				i, base = 2, 16
			} else if l > 1 && sym[0] == '0' {
				i, base = 1, 8
			}
			if u, erruint := strconv.ParseUint(sym[i:], base, 64); erruint == nil {
				if base == 0 {
					base = 10
				}
				on(&TokenUint{Token: u, Base: base})
			} else {
				lexer.Error(nil, erruint.Error())
			}
		case scanner.Comment:
			if strings.HasPrefix(sym, "//") {
				on(&TokenComment{SingleLine: true, Token: sym[2:]})
			} else if strings.HasPrefix(sym, "/*") && strings.HasSuffix(sym, "*/") {
				on(&TokenComment{SingleLine: false, Token: sym[2 : len(sym)-2]})
			} else {
				lexer.Error(nil, "unexpected comment format: "+sym)
			}
		default:
			var issep bool
			for _, sep := range standAloneSeps {
				if issep = sym == sep; issep {
					on(&TokenSep{Token: sym})
					break
				}
			}
			if !issep {
				for _, r := range sym { // as of today, at this point len(sym)==1 always. but we need the r anyway and the iteration would logically hold even for a longer sym
					if !unicode.IsSpace(r) {
						if onlyspacesinlinesofar = false; otheraccum == nil {
							otheraccum = &TokenOther{Token: ""}
							otheraccum.init(&lexer.Position, lineindent)
						}
						otheraccum.Token += sym
					} else if r == '\n' {
						lineindent, onlyspacesinlinesofar = 0, true
					} else if onlyspacesinlinesofar {
						lineindent++
					}
				}
			}
		}
	}
	on(nil) // to capture dangling otheraccum if any
	return
}
