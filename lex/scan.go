package udevlex

import (
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

type Pos struct {
	FilePath string
	Off0     int
	Ln1      int
	Col1     int
}

func (me *Pos) String() string {
	if me == nil {
		return "‹pos=nil›"
	}
	return me.FilePath + ":" + strconv.Itoa(me.Ln1) + ":" + strconv.Itoa(me.Col1)
}

var (
	// ScannerLineCommentPrefix must be set before any calls to Lex or Scan and then never again.
	ScannerLineCommentPrefix = "//"

	// ScannerLongCommentPrefixAndSuffix must be set before any calls to Lex or Scan and then never again.
	ScannerLongCommentPrefixAndSuffix = "/**/"

	// ScannerStringDelims must be set before any calls to Lex or Scan and then never again.
	ScannerStringDelims string = "\""

	// ScannerStringDelimNoEscape must be set before any calls to Lex or Scan and then never again. It must also be mentioned inside `ScannerStringDelims`.
	ScannerStringDelimNoEsc byte
)

type scanState struct {
	src string
	Pos
	since    Pos
	lcl      int
	cur      rune
	isLf     bool
	numDot   bool
	strNoEsc bool
}

func (me *scanState) isNumOrLetterOrUnderscore() bool {
	return me.src[me.Off0] == '_' || unicode.In(me.cur, unicode.Letter, unicode.Number)
}

func Scan(src string, srcFilePath string, on func(TokenKind, *Pos, int, bool)) {
	s := scanState{src: src, lcl: len(ScannerLongCommentPrefixAndSuffix) / 2,
		Pos: Pos{FilePath: srcFilePath, Ln1: 1}}

	var waiton func(*scanState) TokenKind

	for s.Off0, s.cur = range src {
		if s.isLf = (src[s.Off0] == '\n'); s.isLf {
			s.Pos.Col1, s.Pos.Ln1 = 0, s.Pos.Ln1+1
		} else {
			s.Pos.Col1++
		}

		if waiton != nil {
			if tokkind := waiton(&s); tokkind != 0 {
				on(tokkind, &s.since, s.Off0, s.Ln1 != s.since.Ln1)
				waiton = nil
			}
		}
		if waiton == nil && !s.isLf {
			if /* NUM-LIT? */ src[s.Off0] > '/' && src[s.Off0] < ':' {
				s.since, waiton, s.numDot = s.Pos, scanWaitOnLitNumber, false
			} else /* COMMENT? */ if offl := s.Off0 + len(ScannerLineCommentPrefix); offl <= len(src) && ScannerLineCommentPrefix == src[s.Off0:offl] {
				s.since, waiton = s.Pos, scanWaitOnCommentLine
			} else /* COMMENT? */ if offl = s.Off0 + s.lcl; offl <= len(src) && src[s.Off0:offl] == ScannerLongCommentPrefixAndSuffix[:s.lcl] {
				s.since, waiton = s.Pos, scanWaitOnCommentLong
			} else /* STR-LIT? */ if -1 != strings.IndexByte(ScannerStringDelims, src[s.Off0]) {
				s.since, waiton, s.strNoEsc = s.Pos, scanWaitOnLitString, src[s.Off0] == ScannerStringDelimNoEsc
			} else /* IDENT? */ if s.isNumOrLetterOrUnderscore( /* cat N not for 0..9 but for ² and ⅜ etc. */ ) {
				s.since, waiton = s.Pos, scanWaitOnIdent
			}
		}
		if waiton == nil {
			if rl := utf8.RuneLen(s.cur); unicode.IsSpace(s.cur) {
				on(-1, &s.Pos, s.Off0+rl, false)
			} else {
				on(TOKEN_OPISH, &s.Pos, s.Off0+rl, false)
			}
		}
	}

	if s.cur, s.Off0 = -1, len(src); waiton != nil {
		on(waiton(&s), &s.since, s.Off0, s.Ln1 != s.since.Ln1)
	}
}

func scanWaitOnCommentLine(s *scanState) (ret TokenKind) {
	if s.cur == -1 || s.isLf {
		ret = TOKEN_COMMENT
	}
	return
}

func scanWaitOnCommentLong(s *scanState) (ret TokenKind) {
	if s.cur == -1 || (s.Off0 >= (s.since.Off0+len(ScannerLongCommentPrefixAndSuffix)) && s.src[s.Off0-s.lcl:s.Off0] == ScannerLongCommentPrefixAndSuffix[s.lcl:]) {
		ret = TOKEN_COMMENT
	}
	return
}
func scanWaitOnIdent(s *scanState) (ret TokenKind) {
	if s.cur == -1 || !s.isNumOrLetterOrUnderscore() {
		ret = TOKEN_IDENT
	}
	return
}
func scanWaitOnLitString(s *scanState) (ret TokenKind) {
	if s.cur == -1 || (s.Off0 > (s.since.Off0+1) && s.src[s.Off0-1] == s.src[s.since.Off0] && (s.strNoEsc || s.src[s.Off0-2] != '\\')) {
		ret = TOKEN_STR
	}
	return
}
func scanWaitOnLitNumber(s *scanState) (ret TokenKind) {
	numend := (s.cur == -1)
	if !numend {
		if s.src[s.Off0] == '.' {
			if off1 := s.Off0 + 1; (!s.numDot) && off1 < len(s.src) && s.src[off1] > '/' && s.src[off1] < ':' {
				s.numDot = true
			} else {
				numend = true
			}
		} else if (s.src[s.Off0] < '0' || s.src[s.Off0] > '9') && !s.isNumOrLetterOrUnderscore() {
			numend = true
		}
	}
	if numend {
		if s.numDot {
			ret = TOKEN_FLOAT
		} else {
			ret = TOKEN_UINT
		}
	}
	return
}
