package udevlex

import (
	"errors"
	"io"
	"strconv"
	scan "text/scanner"
	"unicode"
)

var (
	// SanitizeDirtyFloatsNextToDotOpishs, if `true`, will cause `Lex` to
	// call `Tokens.SanitizeDirtyFloatsNextToDotOpishs` on each lexeme produced.
	SanitizeDirtyFloatsNextToDotOpishs bool

	// RestrictedWhitespace causes lex errors when encountering standalone (outside
	// comment or string or character tokens) white-space tokens other than '\n' and ' '.
	RestrictedWhitespace bool

	// RestrictedWhitespaceRewriter, if set, is called instead of
	// raising a lexing error when `RestrictedWhitespace` is `true`.
	RestrictedWhitespaceRewriter func(rune) int

	// SepsGroupers, if it is to be used, must be set before the first call to
	// `Lex`, and must never be modified ever again for its consumers such as
	// `Tokens.Chunked`, `Tokens.BreakOnSpace`, `Tokens.Has`, `Tokens.Cliques`
	// to work correctly. It must be of even length beginning with all the
	// "openers" and ending with all the "closers": two equal-length halves
	// in one `string` such as "[(<{}>)]" or "«‹/\›»" etc.
	// ASCII bytes `< 128` only, no `>= 128` runes. Each is also only ever
	// lexed as a `TOKEN_SEPISH` and thus can never be part of a `TOKEN_OPISH`.
	// The mentioned methods skip their logic while passing tokens one or
	// several levels deep within these delimiters.
	SepsGroupers string

	// SepsOthers contains single-byte tokens that would ordinarily be lexed
	// as `TOKEN_OPISH`s and should instead be lexed as `TOKEN_SEPISH`s. As
	// such, they can never be part of multi-rune `TOKEN_OPISH`s and will
	// always stand alone in the resulting stream of lexemes. These are in
	// addition to any specified in `SepsGroupers`, for non-grouping solitary
	// non-operator seps such as eg. `,` or `;` etc.
	SepsOthers string
)

// Lex returns the `Token`s lexed from `src`, or all `Error`s encountered while lexing.
func Lex(src io.Reader, filePath string, toksCap int) (tokens Tokens, errs []*Error) {
	tokens = make(Tokens, 0, toksCap) // a caller's shot-in-the-dark for an initial cap that's better than default 0
	var (
		onlyspacesinlinesofar = true
		lineindent            int
		scanner               scan.Scanner
		otheraccum            *Token
	)
	scanner.Init(src).Filename, idxSepsGroupersClosers = filePath, len(SepsGroupers)/2
	scanner.Whitespace, scanner.Mode = 1<<'\r', scan.ScanChars|scan.ScanComments|scan.ScanFloats|scan.ScanIdents|scan.ScanInts|scan.ScanRawStrings|scan.ScanStrings

	tokerr := false
	scanner.Error = func(_ *scan.Scanner, msg string) {
		err := Err(&scanner.Position, msg)
		err.Pos.Filename = filePath
		tokerr, errs = true, append(errs, err)
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
			token.Meta.init(&scanner.Position, lineindent, origSym)
			tokens = append(tokens, token)
		}
	}

	allseps := SepsOthers + SepsGroupers
	for tokrune := scanner.Scan(); tokrune != scan.EOF; tokrune = scanner.Scan() {
		lexeme := scanner.TokenText()
		switch tokrune {
		case scan.Ident:
			on(lexeme, Token{flag: TOKEN_IDENT, Str: lexeme})
		case scan.Char:
			if !tokerr {
				if c, _, _, errchr := strconv.UnquoteChar(lexeme[1:], '\''); errchr == nil {
					on(lexeme, Token{flag: TOKEN_RUNE, Uint: uint64(c)})
				} else {
					scanner.Error(nil, errchr.Error())
				}
			}
		case scan.RawString, scan.String:
			if !tokerr {
				if s, errstr := strconv.Unquote(lexeme); errstr == nil {
					if tokrune != scan.RawString && lexeme[0] == '`' && lexeme[len(lexeme)-1] == '`' {
						tokrune = scan.RawString
					}
					flag := TOKEN_STR
					if tokrune == scan.RawString {
						flag = _TOKEN_STR_RAW
					}
					on(lexeme, Token{flag: flag, Str: s})
				} else {
					scanner.Error(nil, errstr.Error())
				}
			}
		case scan.Float:
			if !tokerr {
				if f, errfloat := strconv.ParseFloat(lexeme, 64); errfloat == nil {
					on(lexeme, Token{flag: TOKEN_FLOAT, Float: f})
				} else {
					scanner.Error(nil, errfloat.Error())
				}
			}
		case scan.Int:
			if !tokerr {
				var base, i int
				if l := len(lexeme); l > 2 && lexeme[0] == '0' && (lexeme[1] == 'x' || lexeme[1] == 'X') {
					i, base = 2, 16
				} else if l > 1 && lexeme[0] == '0' {
					i, base = 1, 8
				}
				if u, erruint := strconv.ParseUint(lexeme[i:], base, 64); erruint == nil {
					if base == 0 {
						base = 10
					}
					on(lexeme, Token{flag: base, Uint: u})
				} else {
					scanner.Error(nil, erruint.Error())
				}
			}
		case scan.Comment:
			if !tokerr {
				if l, sl := len(lexeme), lexeme[0] == '/'; l > 1 && sl && lexeme[1] == '/' {
					on(lexeme, Token{flag: TOKEN_COMMENT, Str: lexeme[2:]})
				} else if l > 3 && sl && lexeme[1] == '*' {
					if lexeme[l-2] == '*' && lexeme[l-1] == '/' {
						on(lexeme, Token{flag: _TOKEN_COMMENT_ENCL, Str: lexeme[2 : l-2]})
					} else {
						scanner.Error(nil, "missing `*/` at end of comment")
					}
				} else {
					scanner.Error(nil, "unexpected comment format: "+lexeme)
				}
			}
		default:
			var issep bool
			if len(lexeme) == 1 { // as of today, at this point should always be true
				for i := 0; i < len(allseps); i++ {
					if issep = (lexeme[0] == allseps[i]); issep {
						on(lexeme, Token{flag: TOKEN_SEPISH, Str: lexeme})
						break
					}
				}
			}
			if !issep {
				for _, r := range lexeme {
					if !unicode.IsSpace(r) {
						if onlyspacesinlinesofar = false; otheraccum == nil {
							otheraccum = &Token{flag: TOKEN_OPISH}
							otheraccum.Meta.init(&scanner.Position, lineindent, "")
						}
						otheraccum.Str += lexeme
					} else if unaccum(); r == '\n' {
						lineindent, onlyspacesinlinesofar = 0, true
					} else if RestrictedWhitespace && r != ' ' {
						if RestrictedWhitespaceRewriter == nil {
							scanner.Error(nil, "illegal white-space "+strconv.QuoteRune(r)+": only '\\n' and ' ' permissible")
						} else if onlyspacesinlinesofar {
							lineindent += RestrictedWhitespaceRewriter(r)
						}
					} else if onlyspacesinlinesofar {
						lineindent++
					}
				}
			}
		}
		if SanitizeDirtyFloatsNextToDotOpishs {
			tokens.SanitizeDirtyFloatsNextToDotOpishs(len(tokens) - 1)
		}
		tokerr = false
	}
	unaccum()
	return
}

func Lex2(src string, filePath string, toksCap int) (tokens Tokens, errs []*Error) {
	tokens = make(Tokens, 0, toksCap) // a caller's shot-in-the-dark for an initial cap that's better than default 0
	var (
		onlyspacesinlinesofar = true
		lineindent            int
		opishaccum            *Token
	)
	idxSepsGroupersClosers = len(SepsGroupers) / 2

	topos := func(at *Pos) *scan.Position {
		return &scan.Position{Filename: at.FilePath, Column: at.Col1, Line: at.Ln1, Offset: at.Off0}
	}
	accumed := func() {
		if opishaccum != nil {
			if len(errs) == 0 {
				opishaccum.Meta.Orig = opishaccum.Str
				tokens = append(tokens, *opishaccum)
			}
			opishaccum = nil
		}
	}
	on := func(at *Pos, origSym string, token Token) {
		accumed()
		if onlyspacesinlinesofar = false; len(errs) == 0 {
			token.Meta.init(topos(at), lineindent, origSym)
			tokens = append(tokens, token)
		}
	}
	onerr := func(at *Pos, err error) {
		opishaccum, tokens = nil, nil
		errs = append(errs, Err(topos(at), err.Error()))
	}

	allseps := SepsOthers + SepsGroupers
	Scan(src, filePath, func(kind TokenKind, at *Pos, untilOff0 int) {
		lexeme := src[at.Off0:untilOff0]
		switch kind {
		case TOKEN_IDENT:
			on(at, lexeme, Token{flag: TOKEN_IDENT, Str: lexeme})
		case TOKEN_STR:
			if s, err := strconv.Unquote(lexeme); err == nil {
				on(at, lexeme, Token{flag: TOKEN_STR, Str: s})
			} else {
				onerr(at, err)
			}
		case TOKEN_FLOAT:
			if f, err := strconv.ParseFloat(lexeme, 64); err == nil {
				on(at, lexeme, Token{flag: TOKEN_FLOAT, Float: f})
			} else {
				onerr(at, err)
			}
		case TOKEN_UINT:
			if u, err := strconv.ParseUint(lexeme, 0, 64); err == nil {
				on(at, lexeme, Token{flag: TOKEN_UINT, Uint: u})
			} else {
				onerr(at, err)
			}
		case TOKEN_COMMENT:
			on(at, lexeme, Token{flag: TOKEN_COMMENT})
		case TOKEN_OPISH:
			var issep bool
			if len(lexeme) == 1 {
				for i := 0; i < len(allseps); i++ {
					if issep = (lexeme[0] == allseps[i]); issep {
						on(at, lexeme, Token{flag: TOKEN_SEPISH, Str: lexeme})
						break
					}
				}
			}
			if !issep {
				if onlyspacesinlinesofar = false; opishaccum == nil {
					opishaccum = &Token{flag: TOKEN_OPISH}
					opishaccum.Meta.init(topos(at), lineindent, "")
				}
				opishaccum.Str += lexeme
			}
		case -1:
			accumed()
			for _, r := range lexeme {
				if r == '\n' {
					lineindent, onlyspacesinlinesofar = 0, true
				} else if RestrictedWhitespace && r != ' ' {
					if RestrictedWhitespaceRewriter == nil {
						onerr(at, errors.New("illegal white-space "+strconv.QuoteRune(r)+": only '\\n' and ' ' permissible"))
					} else if onlyspacesinlinesofar {
						lineindent += RestrictedWhitespaceRewriter(r)
					}
				} else if onlyspacesinlinesofar {
					lineindent++
				}
			}
		}
	})
	accumed()
	return
}
