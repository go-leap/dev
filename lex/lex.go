package udevlex

import (
	"strconv"
	"strings"
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
func Lex(srcUtf8WithoutBom []byte, filePath string, toksCap int) (tokens Tokens, errs []*Error) {
	tokens = make(Tokens, 0, toksCap) // a caller's shot-in-the-dark for an initial cap that's better than default 0
	var (
		onlyspacesinlinesofar = true
		lineindent            int
		opishaccum            *Token
	)
	idxSepsGroupersClosers = len(SepsGroupers) / 2

	accumed := func() {
		if opishaccum != nil {
			if len(errs) == 0 {
				tokens = append(tokens, *opishaccum)
			}
			opishaccum = nil
		}
	}
	on := func(at *Pos, origSym string, token Token) {
		accumed()
		if onlyspacesinlinesofar = false; len(errs) == 0 {
			token.init(at, lineindent, origSym)
			tokens = append(tokens, token)
		}
	}
	onerr := func(at *Pos, errmsg string) {
		opishaccum, tokens = nil, nil
		errs = append(errs, &Error{Msg: errmsg, Pos: *at})
	}

	allseps, lcl := SepsOthers+SepsGroupers, len(ScannerLongCommentPrefixAndSuffix)/2
	Scan(string(srcUtf8WithoutBom), filePath, func(kind TokenKind, at *Pos, untilOff0 int) {
		lexeme := string(srcUtf8WithoutBom[at.Off0:untilOff0])
		switch kind {
		case TOKEN_IDENT:
			on(at, lexeme, Token{Kind: TOKEN_IDENT})
		case TOKEN_STR:
			fixup := lexeme
			if idx := strings.IndexByte(fixup, '\n'); idx > 0 {
				var buf strings.Builder
				buf.Grow(len(lexeme) + 8)
				buf.WriteString(fixup[:idx])
				buf.WriteString("\\n")
				for i := idx + 1; i < len(fixup); i++ {
					if fixup[i] == '\n' {
						buf.WriteString(fixup[idx+1 : i])
						buf.WriteString("\\n")
						idx = i
					}
				}
				buf.WriteString(fixup[idx+1:])
				fixup = buf.String()
			}
			if s, err := strconv.Unquote(fixup); err == nil {
				on(at, lexeme, Token{Kind: TOKEN_STR, Val: s})
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
		case TOKEN_OPISH:
			var issep bool
			if len(lexeme) == 1 {
				for i := 0; i < len(allseps); i++ {
					if issep = (lexeme[0] == allseps[i]); issep {
						on(at, lexeme, Token{Kind: TOKEN_SEPISH})
						break
					}
				}
			}
			if !issep {
				if onlyspacesinlinesofar = false; opishaccum == nil {
					opishaccum = &Token{Kind: TOKEN_OPISH}
					opishaccum.init(at, lineindent, "")
				}
				opishaccum.Lexeme += lexeme
			}
		case -1:
			accumed()
			for _, r := range lexeme {
				if r == '\n' {
					lineindent, onlyspacesinlinesofar = 0, true
				} else if RestrictedWhitespace && r != ' ' {
					if RestrictedWhitespaceRewriter == nil {
						onerr(at, "illegal white-space "+strconv.QuoteRune(r)+": only '\\n' and ' ' permissible")
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
