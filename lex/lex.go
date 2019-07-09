package udevlex

import (
	"strconv"
	"strings"
	"unsafe"
)

var (
	// RestrictedWhitespace causes lex errors when encountering standalone (outside
	// comment or string or character tokens) white-space tokens other than '\n' and ' '.
	RestrictedWhitespace bool

	// RestrictedWhitespaceRewriter, if set, is called instead of
	// raising a lexing error when `RestrictedWhitespace` is `true`.
	RestrictedWhitespaceRewriter func(rune) int

	// SepsGroupers, if it is to be used, must be set once and once only before
	// the first call to `Lex`, and must never be modified ever again for its
	// consumers such as `Tokens.Chunked`, `Tokens.BreakOnSpace`, `Tokens.Has`,
	// `Tokens.Cliques` to work correctly. It must be of even length beginning
	// with all the "openers" and ending with all the "closers": two equal-length
	// halves in one `string` such as "[(<{}>)]" or "«‹/\›»" etc.
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

	// caution, a global. only mutated by Lex(), users are warned in doc-comments
	// for SepsGroupers to set it before Lex call and never mutate it afterwards.
	// The consumer of this is just called a lot and I cannot accept doing the
	// always-exact-same by-2 division over and over and over again uselessly.
	idxSepsGroupersClosers int
)

// Lex returns the `Token`s lexed from `src`, or all `Error`s encountered while lexing.
func Lex(srcUtf8WithoutBom []byte, srcFilePath string, toksCap int, indentIfRestrictedWhitespace rune) (tokens Tokens, errs []*Error) {
	tokens = make(Tokens, 0, toksCap) // a caller's shot-in-the-dark for an initial cap that's better than default 0
	var (
		onlyspacesinlinesofar = true
		lineindent            int
		opishaccum            *Token
	)
	idxSepsGroupersClosers = len(SepsGroupers) / 2

	opishaccumed := func() {
		if opishaccum != nil {
			if len(errs) == 0 {
				tokens = append(tokens, *opishaccum)
			}
			opishaccum = nil
		}
	}
	on := func(at *Pos, origSym string, token Token) {
		opishaccumed()
		if onlyspacesinlinesofar = false; len(errs) == 0 {
			token.init(at, lineindent, origSym)
			tokens = append(tokens, token)
		}
	}
	onerr := func(at *Pos, errmsg string) {
		opishaccum, tokens = nil, nil
		errs = append(errs, &Error{Msg: errmsg, Pos: *at})
	}

	allseps, lcl :=
		(SepsOthers + SepsGroupers), (len(ScannerLongCommentPrefixAndSuffix) / 2)

	Scan(string(srcUtf8WithoutBom), srcFilePath,
		func(kind TokenKind, at *Pos, untiloff0 int, multiline bool) {
			lexeme := string(srcUtf8WithoutBom[at.Off0:untiloff0])
			switch kind {

			case TOKEN_IDENT:
				on(at, lexeme, Token{Kind: TOKEN_IDENT})

			case TOKEN_OPISH:
				var issep bool
				if issep = (-1 != strings.IndexByte(allseps, lexeme[0])); issep {
					on(at, lexeme, Token{Kind: TOKEN_SEPISH})
				}
				if !issep {
					if onlyspacesinlinesofar = false; opishaccum == nil {
						opishaccum = &Token{Kind: TOKEN_OPISH}
						opishaccum.init(at, lineindent, "")
					}
					opishaccum.Lexeme += lexeme
				}

			case TOKEN_STR:
				var val string
				var err error
				if fixed, delim := lexeme, lexeme[0]; delim == ScannerStringDelimNoEsc {
					if missingdelim := len(fixed) < 2 || fixed[len(fixed)-1] != delim; missingdelim {
						val = fixed[1:]
					} else {
						val = fixed[1 : len(fixed)-1]
					}
				} else {
					missingdelim := len(fixed) < 2 || fixed[len(fixed)-1] != delim || fixed[len(fixed)-2] == '\\'
					if multiline {
						/*
							for esc-strs, we reuse strconv.Unquote for str-lits for
							now, which allows "" or `` delims -- the former supports
							escape-codes but no LFs, the latter vice versa. we aim
							to lex str-lits with both LFs and escape-codes. so
							rewrite LFs (\n) to escaped LFs (\\n) -- this block is
							in essence a strings.ReplaceByteWithString('\n',"\\n").
							the first LF byte is found into `idx` via asm, then iterate
						*/
						buf, idx := make([]byte, 0, len(fixed)+8), strings.IndexByte(fixed, '\n')
						buf = append(buf, fixed[:idx]...)
						buf = append(buf, '\\', 'n')
						for i := idx + 1; i < len(fixed); i++ {
							if fixed[i] == '\n' {
								buf = append(buf, fixed[idx+1:i]...)
								buf = append(buf, '\\', 'n')
								idx = i
							}
						}
						buf = append(buf, fixed[idx+1:]...)
						if missingdelim {
							buf = append(buf, '"')
						}
						fixed = *(*string)(unsafe.Pointer(&buf))
					} else if missingdelim {
						fixed = fixed + string(delim)
					}
					val, err = strconv.Unquote(fixed)
				}
				if err == nil {
					on(at, lexeme, Token{Kind: TOKEN_STR, Val: val})
				} else {
					onerr(at, "text-string literal: "+err.Error()+" (check delimiters and escape codes)")
				}

			case TOKEN_FLOAT:
				if f, err := strconv.ParseFloat(lexeme, 64); err == nil {
					on(at, lexeme, Token{Kind: TOKEN_FLOAT, Val: f})
				} else {
					onerr(at, "floating-point literal: "+err.Error())
				}

			case TOKEN_UINT:
				if u, err := strconv.ParseUint(lexeme, 0, 64); err == nil {
					on(at, lexeme, Token{Kind: TOKEN_UINT, Val: u})
				} else {
					onerr(at, "unsigned-integer literal: "+err.Error())
				}

			case TOKEN_COMMENT:
				commenttext := lexeme
				if strings.HasPrefix(commenttext, ScannerLineCommentPrefix) {
					commenttext = commenttext[len(ScannerLineCommentPrefix):]
				} else {
					commenttext = commenttext[lcl : len(commenttext)-lcl]
				}
				on(at, lexeme, Token{Kind: TOKEN_COMMENT, Val: commenttext})

			case -1: // white-space
				opishaccumed()
				for _, r := range lexeme {
					if r == '\n' {
						lineindent, onlyspacesinlinesofar = 0, true
					} else if onlyspacesinlinesofar {
						if RestrictedWhitespace && r != indentIfRestrictedWhitespace {
							if RestrictedWhitespaceRewriter == nil {
								onerr(at, "illegal indenting white-space "+strconv.QuoteRune(r)+": only "+strconv.QuoteRune(indentIfRestrictedWhitespace)+" permissible in this block")
							} else if onlyspacesinlinesofar {
								lineindent += RestrictedWhitespaceRewriter(r)
							}
						} else {
							lineindent++
						}
					}
				}

			}
		})
	opishaccumed()
	return
}
