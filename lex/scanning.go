package udevlex

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
	src       string
	FileName  string
	Position  Pos
	OnComment func(string, bool)
	OnString  func(string)
	OnIdent   func(string)
	OnOther   func(rune)
	OnNumber  func(string, bool)
}

func (me *Scanner) Init(src string) {
	me.src, me.Position.FilePath = src, me.FileName
	me.Position.Col1, me.Position.Line1, me.Position.Offset0 = 1, 1, 0
}

func (me *Scanner) Scan() {
	type onwhat byte
	const (
		_ onwhat = iota
		oncomment
		onstring
		onident
		onnumber
	)
	var off0 int
	var isnewln bool
	var islast bool

	var waitends func() (onwhat, bool)
	waitsince, src, offlast, lcl := -1, me.src, len(me.src)-1, len(ScannerLongComment)/2
	waitforendoflinecomment := func() (on onwhat, islong bool) {
		if islast || isnewln {
			on = oncomment
		}
		return
	}
	waitforendoflongcomment := func() (on onwhat, islong bool) {
		if islast || (off0 >= (waitsince+len(ScannerLongComment)) && src[off0-lcl:off0] == ScannerLongComment[lcl:]) {
			on, islong = oncomment, true
		}
		return
	}
	waitforendofstring := func() (on onwhat, _ bool) {
		if islast || (off0 > (waitsince+1) && src[off0-1] == ScannerStringDelim && src[off0-2] != '\\') {
			on = onstring
		}
		return
	}

	for off0 = range src {
		me.Position.Offset0 = off0
		islast, isnewln = (off0 == offlast), (src[off0] == '\n')
		if isnewln {
			me.Position.Col1, me.Position.Line1 = 1, me.Position.Line1+1
		} else {
			me.Position.Col1++
		}

		if waitends != nil {
			if on, b := waitends(); on != 0 {
				switch on {
				case oncomment:
					me.OnComment(src[waitsince:off0], b)
				case onident:
					me.OnIdent(src[waitsince:off0])
				case onnumber:
					me.OnNumber(src[waitsince:off0], b)
				case onstring:
					me.OnString(src[waitsince:off0])
				}
				waitsince, waitends = -1, nil
			}
		}
		if waitends == nil {
			canbeginwait := !isnewln
			switch {
			case canbeginwait && src[off0] == ScannerStringDelim:
				waitsince, waitends = off0, waitforendofstring
			case canbeginwait && src[off0:off0+len(ScannerLineComment)] == ScannerLineComment:
				waitsince, waitends = off0, waitforendoflinecomment
			case canbeginwait && src[off0:off0+lcl] == ScannerLongComment[:lcl]:
				waitsince, waitends = off0, waitforendoflongcomment
			}
		}
	}
}
