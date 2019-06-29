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
	var numhasdot bool

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
		if src[off0] == '.' {
			numhasdot = true
		} else if src[off0] != '_' && (src[off0] < 48 || src[off0] > 57) && !unicode.IsLetter(rcur) {
			if numhasdot {
				ret = TOKEN_FLOAT
			} else {
				ret = TOKEN_UINT
			}
		}
		return
	}
	wait4endident := func() (ret TokenKind) {
		if src[off0] != '_' && !unicode.IsLetter(rcur) {
			ret = TOKEN_IDENT
		}
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
				// TODO: bounds-check above
				waitsince, waitends = off0, wait4endlinecomment
			case src[off0:off0+lcl] == ScannerLongComment[:lcl]:
				// TODO: bounds-check above
				waitsince, waitends = off0, wait4endlongcomment
			case src[off0] > 47 && src[off0] < 58:
				waitsince, waitends, numhasdot = off0, wait4endnumber, false
			case src[off0] == '_' || unicode.IsLetter(rcur):
				waitsince, waitends = off0, wait4endident
			}
		}
		if waitends == nil {

		}
	}
}
