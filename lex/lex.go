package udevlex

import (
	"io"
	"strconv"
	"text/scanner"
	"unicode"
)

var RestrictedWhitespaceRewriter func(rune) int
var RestrictedWhitespace bool
var StandaloneSeps []string

// Lex returns the `Token`s lexed from `src`, or all `Error`s encountered while lexing.
//
// If `errs` has a `len` greater than 0, `tokens` will be empty (and vice versa).
func Lex(src io.Reader, filePath string, lineOff int, posOff int, toksCap int) (tokens Tokens, errs []*Error) {
	tokens = make(Tokens, 0, toksCap) // a caller's shot-in-the-dark for an initial cap that's better than default 0
	var (
		onlyspacesinlinesofar = true
		lineindent            int
		lexer                 scanner.Scanner
		otheraccum            *Token
	)
	lexer.Init(src).Filename = filePath
	lexer.Whitespace, lexer.Mode = 1<<'\r', scanner.ScanChars|scanner.ScanComments|scanner.ScanFloats|scanner.ScanIdents|scanner.ScanInts|scanner.ScanRawStrings|scanner.ScanStrings
	lexer.Error = func(_ *scanner.Scanner, msg string) {
		err := Err(&lexer.Position, msg)
		err.Pos.Filename, err.Pos.Line, err.Pos.Offset =
			filePath, err.Pos.Line+lineOff, err.Pos.Offset+posOff
		tokens, errs = nil, append(errs, err)
	}

	unaccum := func() {
		if otheraccum != nil {
			if len(errs) == 0 {
				otheraccum.Meta.Orig = otheraccum.Str
				tokens = append(tokens, *otheraccum)
			}
			otheraccum = nil
		}
	}

	on := func(origSym string, token Token) {
		unaccum()
		if onlyspacesinlinesofar = false; len(errs) == 0 {
			token.Meta.init(&lexer.Position, lineindent, origSym, lineOff, posOff)
			tokens = append(tokens, token)
		}
	}

	for tok := lexer.Scan(); tok != scanner.EOF; tok = lexer.Scan() {
		sym := lexer.TokenText()
		switch tok {
		case scanner.Ident:
			on(sym, Token{flag: TOKEN_IDENT, Str: sym})
		case scanner.Char:
			if c, _, _, errchr := strconv.UnquoteChar(sym[1:], '\''); errchr == nil {
				on(sym, Token{flag: TOKEN_RUNE, Uint: uint64(c)})
			} else {
				lexer.Error(nil, errchr.Error())
			}
		case scanner.RawString, scanner.String:
			if s, errstr := strconv.Unquote(sym); errstr == nil {
				if tok != scanner.RawString && sym[0] == '`' && sym[len(sym)-1] == '`' {
					tok = scanner.RawString
				}
				flag := TOKEN_STR
				if tok == scanner.RawString {
					flag = _TOKEN_STR_RAW
				}
				on(sym, Token{flag: flag, Str: s})
			} else {
				lexer.Error(nil, errstr.Error())
			}
		case scanner.Float:
			if f, errfloat := strconv.ParseFloat(sym, 64); errfloat == nil {
				on(sym, Token{flag: TOKEN_FLOAT, Float: f})
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
				on(sym, Token{flag: base, Uint: u})
			} else {
				lexer.Error(nil, erruint.Error())
			}
		case scanner.Comment:
			if l, sl := len(sym), sym[0] == '/'; l > 1 && sl && sym[1] == '/' {
				on(sym, Token{flag: TOKEN_COMMENT, Str: sym[2:]})
			} else if l > 3 && sl && sym[1] == '*' && sym[l-2] == '*' && sym[l-1] == '/' {
				on(sym, Token{flag: _TOKEN_COMMENT_ENCL, Str: sym[2 : l-2]})
			} else {
				lexer.Error(nil, "unexpected comment format: "+sym)
			}
		default:
			var issep bool
			for _, sep := range StandaloneSeps {
				if issep = (sym == sep); issep {
					on(sym, Token{flag: TOKEN_SEP, Str: sym})
					break
				}
			}
			if !issep {
				for _, r := range sym { // as of today, at this point len(sym)==1 always. but we need the r anyway and the iteration would logically hold even for a longer sym
					if !unicode.IsSpace(r) {
						if onlyspacesinlinesofar = false; otheraccum == nil {
							otheraccum = &Token{flag: TOKEN_OTHER}
							otheraccum.Meta.init(&lexer.Position, lineindent, "", lineOff, posOff)
						}
						otheraccum.Str += sym
					} else if unaccum(); r == '\n' {
						lineindent, onlyspacesinlinesofar = 0, true
					} else if RestrictedWhitespace && r != ' ' {
						if RestrictedWhitespaceRewriter == nil {
							lexer.Error(nil, "illegal white-space "+strconv.QuoteRune(r)+": only '\\n' and ' ' permissible")
						} else if onlyspacesinlinesofar {
							lineindent += RestrictedWhitespaceRewriter(r)
						}
					} else if onlyspacesinlinesofar {
						lineindent++
					}
				}
			}
		}
	}
	unaccum()
	return
}
