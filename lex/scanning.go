package udevlex

import (
	"strings"
	"unicode"
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
	var numhasdot bool

	var waitends func() TokenKind
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
		if rcur == -1 || (src[off0] != '_' && !unicode.IsLetter(rcur)) {
			ret = TOKEN_IDENT
		}
		return
	}
	wait4endnumber := func() (ret TokenKind) {
		numdone := (rcur == -1)
		if !numdone {
			if src[off0] == '.' {
				if off1 := off0 + 1; off1 < len(src) && src[off1] > 47 && src[off1] < 58 {
					numhasdot = true
				} else {
					numdone = true
				}
			} else if src[off0] != '_' && (src[off0] < 48 || src[off0] > 57) &&
				!unicode.IsLetter(rcur) {
				numdone = true
			}
		}
		if numdone {
			if numhasdot {
				ret = TOKEN_FLOAT
			} else {
				ret = TOKEN_UINT
			}
		}
		return
	}

	for off0, rcur = range src {
		pos.Offset0 = off0
		isnewln = (src[off0] == '\n')
		if isnewln {
			pos.Col1, pos.Line1 = 1, pos.Line1+1
		} else {
			pos.Col1++
		}

		if waitends != nil {
			if tokkind := waitends(); tokkind != 0 {
				on(tokkind, &waitsince, off0-waitsince.Offset0)
				waitends = nil
			}
		}
		if waitends == nil && !isnewln {
			if /* NUM-LIT? */ src[off0] > 47 && src[off0] < 58 {
				waitsince, waitends, numhasdot = pos, wait4endnumber, false
			} else /* STR-LIT? */ if strings.IndexByte(ScannerStringDelims, src[off0]) != -1 {
				waitsince, waitends = pos, wait4endstring
			} else /* COMMENT? */ if ScannerLineCommentPrefix == src[off0:off0+len(ScannerLineCommentPrefix)] {
				// TODO: bounds-check above
				waitsince, waitends = pos, wait4endlinecomment
			} else /* COMMENT? */ if src[off0:off0+lcl] == ScannerLongCommentPrefixAndSuffix[:lcl] {
				// TODO: bounds-check above
				waitsince, waitends = pos, wait4endlongcomment
			} else /* IDENT? */ if src[off0] == '_' || unicode.IsLetter(rcur) {
				waitsince, waitends = pos, wait4endident
			}
		}
		if waitends == nil {

		}
	}
	if rcur, off0 = -1, len(src); waitends != nil {
		on(waitends(), &waitsince, off0-waitsince.Offset0)
	}
}
