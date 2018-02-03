package udevlex

type Tokens []IToken

func (me Tokens) BreakOnIndent() (indented Tokens, outdented Tokens) {
	ln, minindent := me[0].Meta().Line, me[0].Meta().LineIndent
	for i := 1; i < len(me); i++ {
		if tpos := me[i].Meta(); tpos.Line != ln && tpos.LineIndent <= minindent {
			indented, outdented = me[:i], me[i:]
			return
		}
	}
	indented = me
	return
}

// BreakOnOther returns all `Tokens` preceding and succeeding the next occurence of the specified `TokenOther` in `me`, if any — otherwise, `nil,nil` will be returned.
func (me Tokens) BreakOnOther(token string) (pref Tokens, suff Tokens) {
	for i, tok := range me {
		if toth, _ := tok.(*TokenOther); toth != nil && toth.Token == token {
			pref, suff = me[:i], me[i+1:]
			return
		}
	}
	return
}

// SansComments returns the newly allocated `sans` with a `cap` of `len(me)` and containing all `Tokens` in `me` except `TokenComment`s.
func (me Tokens) SansComments() (sans Tokens) {
	sans = make(Tokens, 0, len(me))
	for _, tok := range me {
		if tcmnt, _ := tok.(*TokenComment); tcmnt == nil {
			sans = append(sans, tok)
		}
	}
	return
}

func (me Tokens) SubTokens(sepOpen string, sepClose string) (sub Tokens, tail Tokens, numUnclosed int) {
	sep, _ := me[0].(*TokenSep)
	if tail = me; sep == nil || sep.Token != sepOpen {
		return
	}

	for i := 1; i < len(me); i++ {
		if sep, _ = me[i].(*TokenSep); sep != nil {
			if sep.Token == sepOpen {
				numUnclosed++
			} else if sep.Token == sepClose {
				if numUnclosed == 0 {
					sub, tail = me[1:i], me[i+1:]
					return
				}
				numUnclosed--
			}
		}
	}
	return
}

// IndentBasedChunks breaks up `me` into a number of `chunks`:
// each 'non-indented' line (with `LineIndent` <= `minIndent`) in `me` begins a new
// 'chunk' and any subsequent 'indented' (`LineIndex` > `minIndent`) lines also belong to it.
func (me Tokens) IndentBasedChunks(minIndent int) (chunks []Tokens) {
	var cur int
	for i, ln, l := 0, 1, len(me); i < l; i++ {
		if i == l-1 {
			if tlc := me[cur:]; len(tlc) > 0 {
				chunks = append(chunks, tlc)
			}
		} else if tpos := me[i].Meta(); tpos.LineIndent <= minIndent && tpos.Line != ln {
			if tlc := me[cur:i]; len(tlc) > 0 {
				chunks = append(chunks, tlc)
			}
			cur, ln = i, tpos.Line
		}
	}
	return
}
