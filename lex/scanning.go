package udevlex

import (
	"unicode"
)

type Pos struct {
	FilePath string
	Offset0  int
	Line1    int
	Col1     int
}

var (
	ScannerLineComment      = "//"
	ScannerLongComment      = "/**/"
	ScannerStringDelim byte = '"'
)

type Scanner struct {
	FileName string
	Position Pos
	On       func(TokenKind, int, int)
}

func (me *Scanner) Scan(src string, srcFilePath string) {
	me.Position.FilePath, me.Position.Offset0, me.Position.Line1, me.Position.Col1 =
		me.FileName, 0, 1, 1

	var off0 int
	var rcur rune
	var isnewln bool
	var islast bool

	var waitends func() TokenKind
	waitsince, offlast, lcl := -1, len(src)-1, len(ScannerLongComment)/2

	wait4endlinecomment := func() (ret TokenKind) {
		if islast || isnewln {
			ret = TOKEN_COMMENT
		}
		return
	}
	wait4endlongcomment := func() (ret TokenKind) {
		if islast || (off0 >= (waitsince+len(ScannerLongComment)) && src[off0-lcl:off0] == ScannerLongComment[lcl:]) {
			ret = _TOKEN_COMMENT_ENCL
		}
		return
	}
	wait4endstring := func() (ret TokenKind) {
		if islast || (off0 > (waitsince+1) && src[off0-1] == ScannerStringDelim && src[off0-2] != '\\') {
			ret = TOKEN_STR
		}
		return
	}
	wait4endnumber := func() (ret TokenKind) {
		// begin with 0..9
		// inside can have 0..9 or letters or `_` or `.`
		// end with 0..9 followed by non-inside (else peek at next for inside-compat)
		return
	}

	for off0, rcur = range src {
		me.Position.Offset0 = off0
		islast, isnewln = (off0 == offlast), (src[off0] == '\n')
		if isnewln {
			me.Position.Col1, me.Position.Line1 = 1, me.Position.Line1+1
		} else {
			me.Position.Col1++
		}

		if waitends != nil {
			if tokkind := waitends(); tokkind != 0 {
				me.On(tokkind, waitsince, off0)
				waitsince, waitends = -1, nil
			}
		}
		if waitends == nil && !isnewln {
			switch {
			case src[off0] == ScannerStringDelim:
				waitsince, waitends = off0, wait4endstring
			case src[off0:off0+len(ScannerLineComment)] == ScannerLineComment:
				waitsince, waitends = off0, wait4endlinecomment
			case src[off0:off0+lcl] == ScannerLongComment[:lcl]:
				waitsince, waitends = off0, wait4endlongcomment
			case unicode.IsNumber(rcur):
				waitsince, waitends = off0, wait4endnumber
			}
		}
		if waitends == nil {

		}
	}
}
