package udevlex

import (
	"strconv"
	"strings"
	"text/scanner"
	"unicode"
)

// Pos returns the last in `tokens`, or `fallback`, or a new `TokenMeta` at position 1,1 for `fallbackFilePath`.
func Pos(tokens []IToken, fallback IPos, fallbackFilePath string) IPos {
	if l := len(tokens); l > 0 {
		return tokens[l-1]
	}
	if fallback != nil {
		return fallback
	}
	return &TokenMeta{Position: scanner.Position{Line: 1, Column: 1, Filename: fallbackFilePath}}
}

// Lex returns the `Token`s lexed from `src`, or all `LexError`s encountered while lexing.
//
// If `errs` has a `len` greater than 0, `tokenStream` will be empty (and vice versa).
func Lex(filePath string, src string, standAloneSeps ...string) (tokenStream []IToken, errs []*Error) {
	tokenStream = make([]IToken, 0, len(src)/4) // a shot in the dark for an initial cap that's better than default 0. could be sub-optimal for source files of several 100s of MB — revisit when that becomes realistic/common
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
		tokenStream, errs = nil, append(errs, err)
	}

	on := func(token IToken) {
		if otheraccum != nil {
			otheraccum, tokenStream = nil, append(tokenStream, otheraccum)
		}
		if onlyspacesinlinesofar = false; len(errs) == 0 && token != nil {
			token.init(&lexer.Position, lineindent)
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
			base := 0
			if l := len(sym); l > 2 && sym[0] == '0' && (sym[1] == 'x' || sym[1] == 'X') {
				base = 16
			} else if l > 1 && sym[0] == '0' {
				base = 8
			}
			if u, erruint := strconv.ParseUint(sym, 0, 64); erruint == nil {
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
				lexer.Error(nil, "unexpected comment format")
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
				for _, r := range sym {
					if !unicode.IsSpace(r) {
						if otheraccum == nil {
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

// IndentBasedChunks breaks up `tokens` into a number of `chunks`:
// each 'non-indented' line (with `LineIndent` <= `minIndent`) in `tokens` begins a new
// 'chunk' and any subsequent 'indented' (`LineIndex` > `minIndent`) lines also belong to it.
func IndentBasedChunks(tokens []IToken, minIndent int) (chunks [][]IToken) {
	var cur int
	for i, ln, l := 0, 1, len(tokens); i < l; i++ {
		if i == l-1 {
			if tlc := tokens[cur:]; len(tlc) > 0 {
				chunks = append(chunks, tlc)
			}
		} else if tpos := tokens[i].Meta(); tpos.LineIndent <= minIndent && tpos.Line != ln {
			if tlc := tokens[cur:i]; len(tlc) > 0 {
				chunks = append(chunks, tlc)
			}
			cur, ln = i, tpos.Line
		}
	}
	return
}
