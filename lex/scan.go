package udevlex

import (
	"strings"
	"unicode"
	"unicode/utf8"
)

type Pos struct {
	FilePath string
	Offset0  int
	Line1    int
	Col1     int
}

var (
	ScannerLineCommentPrefix                 = "//"
	ScannerLongCommentPrefixAndSuffix        = "/**/"
	ScannerStringDelims               string = "'\""
)

func Scan(src string, srcFilePath string, on func(TokenKind, *Pos, int)) {
	pos := Pos{FilePath: srcFilePath, Line1: 1, Col1: 1}

	var off0 int
	var rcur rune
	var isnewln bool
	var numdot bool

	var wait4end func() TokenKind
	var waitsince Pos
	lcl := len(ScannerLongCommentPrefixAndSuffix) / 2
	wait4endlinecomment := func() (ret TokenKind) {
		if rcur == -1 || isnewln {
			ret = TOKEN_COMMENT
		}
		return
	}
	wait4endlongcomment := func() (ret TokenKind) {
		if rcur == -1 || (off0 >= (waitsince.Offset0+len(ScannerLongCommentPrefixAndSuffix)) && src[off0-lcl:off0] == ScannerLongCommentPrefixAndSuffix[lcl:]) {
			ret = _TOKEN_COMMENT_ENCL
		}
		return
	}
	wait4endstring := func() (ret TokenKind) {
		if rcur == -1 || (off0 > (waitsince.Offset0+1) && src[off0-1] == src[waitsince.Offset0] && src[off0-2] != '\\') {
			ret = TOKEN_STR
		}
		return
	}
	wait4endident := func() (ret TokenKind) {
		if rcur == -1 || (src[off0] != '_' && !unicode.In(rcur, unicode.Letter, unicode.Number)) {
			ret = TOKEN_IDENT
		}
		return
	}
	wait4endnumber := func() (ret TokenKind) {
		numend := (rcur == -1)
		if !numend {
			if src[off0] == '.' {
				if off1 := off0 + 1; (!numdot) && off1 < len(src) && src[off1] > '/' && src[off1] < ':' {
					numdot = true
				} else {
					numend = true
				}
			} else if src[off0] != '_' && (src[off0] < '0' || src[off0] > '9') &&
				!unicode.In(rcur, unicode.Letter, unicode.Number) {
				numend = true
			}
		}
		if numend {
			if numdot {
				ret = TOKEN_FLOAT
			} else {
				ret = TOKEN_UINT
			}
		}
		return
	}

	for off0, rcur = range src {
		if pos.Offset0, isnewln = off0, (src[off0] == '\n'); isnewln {
			pos.Col1, pos.Line1 = 1, pos.Line1+1
		} else {
			pos.Col1++
		}

		if wait4end != nil {
			if tokkind := wait4end(); tokkind != 0 {
				on(tokkind, &waitsince, off0-waitsince.Offset0)
				wait4end = nil
			}
		}
		if wait4end == nil && !isnewln {
			if /* NUM-LIT? */ src[off0] > '/' && src[off0] < ':' {
				waitsince, wait4end, numdot = pos, wait4endnumber, false
			} else /* STR-LIT? */ if strings.IndexByte(ScannerStringDelims, src[off0]) != -1 {
				waitsince, wait4end = pos, wait4endstring
			} else /* COMMENT? */ if offl := off0 + len(ScannerLineCommentPrefix); offl <= len(src) && ScannerLineCommentPrefix == src[off0:offl] {
				waitsince, wait4end = pos, wait4endlinecomment
			} else /* COMMENT? */ if offl = off0 + lcl; offl <= len(src) && src[off0:offl] == ScannerLongCommentPrefixAndSuffix[:lcl] {
				waitsince, wait4end = pos, wait4endlongcomment
			} else /* IDENT? */ if src[off0] == '_' || unicode.IsLetter(rcur) {
				waitsince, wait4end = pos, wait4endident
			}
		}
		if wait4end == nil {
			if rl := utf8.RuneLen(rcur); unicode.IsSpace(rcur) {
				on(TOKEN_SPACE, &pos, rl)
			} else {
				on(TOKEN_OTHER, &pos, rl)
			}
		}
	}
	if rcur, off0 = -1, len(src); wait4end != nil {
		on(wait4end(), &waitsince, off0-waitsince.Offset0)
	}
}
