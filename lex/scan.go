package udevlex

import (
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

var (
	ScannerLineCommentPrefix                 = "//"
	ScannerLongCommentPrefixAndSuffix        = "/**/"
	ScannerStringDelims               string = "'\""
)

type scanState struct {
	src string
	Pos
	since  Pos
	lcl    int
	cur    rune
	isLf   bool
	numDot bool
}

func Scan(src string, srcFilePath string, on func(TokenKind, *Pos, int)) {
	s := scanState{src: src, lcl: len(ScannerLongCommentPrefixAndSuffix) / 2,
		Pos: Pos{FilePath: srcFilePath, Ln1: 1, Col1: 1}}

	var waiton func(*scanState) TokenKind

	for s.Off0, s.cur = range src {
		if s.isLf = (src[s.Off0] == '\n'); s.isLf {
			s.Pos.Col1, s.Pos.Ln1 = 1, s.Pos.Ln1+1
		} else {
			s.Pos.Col1++
		}

		if waiton != nil {
			if tokkind := waiton(&s); tokkind != 0 {
				on(tokkind, &s.since, s.Off0-s.since.Off0)
				waiton = nil
			}
		}
		if waiton == nil && !s.isLf {
			if /* NUM-LIT? */ src[s.Off0] > '/' && src[s.Off0] < ':' {
				s.since, waiton, s.numDot = s.Pos, scanWaitOnLitNumber, false
			} else /* STR-LIT? */ if strings.IndexByte(ScannerStringDelims, src[s.Off0]) != -1 {
				s.since, waiton = s.Pos, scanWaitOnLitString
			} else /* COMMENT? */ if offl := s.Off0 + len(ScannerLineCommentPrefix); offl <= len(src) && ScannerLineCommentPrefix == src[s.Off0:offl] {
				s.since, waiton = s.Pos, scanWaitOnCommentLine
			} else /* COMMENT? */ if offl = s.Off0 + s.lcl; offl <= len(src) && src[s.Off0:offl] == ScannerLongCommentPrefixAndSuffix[:s.lcl] {
				s.since, waiton = s.Pos, scanWaitOnCommentLong
			} else /* IDENT? */ if src[s.Off0] == '_' || unicode.IsLetter(s.cur) {
				s.since, waiton = s.Pos, scanWaitOnIdent
			}
		}
		if waiton == nil {
			if rl := utf8.RuneLen(s.cur); unicode.IsSpace(s.cur) {
				on(TOKEN_SPACE, &s.Pos, rl)
			} else {
				on(TOKEN_OTHER, &s.Pos, rl)
			}
		}
	}
	if s.cur, s.Off0 = -1, len(src); waiton != nil {
		on(waiton(&s), &s.since, s.Off0-s.since.Off0)
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
		ret = _TOKEN_COMMENT_ENCL
	}
	return
}
func scanWaitOnIdent(s *scanState) (ret TokenKind) {
	if s.cur == -1 || (s.src[s.Off0] != '_' && !unicode.In(s.cur, unicode.Letter, unicode.Number)) {
		ret = TOKEN_IDENT
	}
	return
}
func scanWaitOnLitString(s *scanState) (ret TokenKind) {
	if s.cur == -1 || (s.Off0 > (s.since.Off0+1) && s.src[s.Off0-1] == s.src[s.since.Off0] && s.src[s.Off0-2] != '\\') {
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
		} else if s.src[s.Off0] != '_' && (s.src[s.Off0] < '0' || s.src[s.Off0] > '9') &&
			!unicode.In(s.cur, unicode.Letter, unicode.Number) {
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
