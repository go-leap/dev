package udevlex

import (
	"bytes"
	"text/scanner"
)

type Tokens []Token

// BreakOnIndent returns in `indented` all `Tokens` on the same line as the first in `me`, plus all subsequent `Tokens` with `LineIndent` greater than `minLineIndent`; and in `outdented` the first and all following `Tokens` with a `LineIndent` less-or-equal (if any).
func (me Tokens) BreakOnIndent(minLineIndent int) (indented Tokens, outdented Tokens) {
	if len(me) > 0 {
		linenum := me[0].Meta.Line
		for i := 1; i < len(me); i++ {
			if me[i].Meta.Line != linenum && me[i].Meta.LineIndent <= minLineIndent {
				indented, outdented = me[:i], me[i:]
				return
			}
		}
	}
	indented = me
	return
}

// BreakOnIdent finds the desired occurrence of `needleIdent` in `me`, then returns in `pref` all `Tokens` preceding it and in `suff` all following it.
// If `skipForEachOccurrenceOfIdent` is given, then for every encountered `TokenIdent` occurrence of it one `needleIdent` occurrence will be skipped.
// If `numUnclosed` is not `0`, this typically indicates a syntax error depending on the language being lexed; strictly speaking it denotes the number of skipped-and-not-closed occurrences of `skipForEachOccurrenceOfIdent`.
// Unless a correct break position was found, `pref` and `suff` will both be `nil`.
func (me Tokens) BreakOnIdent(needleIdent string, skipForEachOccurrenceOfIdent string) (pref Tokens, suff Tokens, numUnclosed int) {
	for i := 0; i < len(me); i++ {
		if me[i].flag == TOKEN_IDENT {
			switch me[i].Meta.Orig {
			case skipForEachOccurrenceOfIdent:
				numUnclosed++
			case needleIdent:
				if numUnclosed > 0 {
					numUnclosed--
				} else {
					pref, suff = me[:i], me[i+1:]
					return
				}
			}
		}
	}
	return
}

// BreakOnOpish returns all `Tokens` preceding and succeeding the next occurence of the specified `TokenOther` in `me`, if any — otherwise, `me,nil` will be returned.
func (me Tokens) BreakOnOpish(token string) (pref Tokens, op *Token, suff Tokens) {
	pref = me
	for i := 0; i < len(me); i++ {
		if me[i].flag == TOKEN_OPISH && me[i].Meta.Orig == token {
			pref, op, suff = me[:i], &me[i], me[i+1:]
			return
		}
	}
	return
}

func (me Tokens) CountKind(kind TokenKind) (count int) {
	for i := range me {
		if me[i].Kind() == kind {
			count++
		}
	}
	return
}

func (me Tokens) HasKind(kind TokenKind) bool {
	for i := range me {
		if me[i].Kind() == kind {
			return true
		}
	}
	return false
}

func (me Tokens) SpacesBetween(idxEarlier int, idxLater int) (numSpaces int) {
	if idxEarlier != idxLater {
		poslatter, posformer := me[idxLater].Meta.Position.Offset, me[idxEarlier].Meta.Position.Offset+len(me[idxEarlier].Meta.Orig)
		numSpaces = poslatter - posformer
	}
	return
}

func (me Tokens) HasSpaces() bool {
	for i := 1; i < len(me); i++ {
		if diff := me[i].Meta.Position.Offset - (me[i-1].Meta.Position.Offset + len(me[i-1].Meta.Orig)); diff > 0 {
			return true
		}
	}
	return false
}

func (me Tokens) NumCharsBetweenFirstAndLastOf(other Tokens) (dist int) {
	mfirst, olast := &me[0], &other[len(other)-1]
	mpos, opos := mfirst.Meta.Position.Offset, olast.Meta.Position.Offset
	dist = mpos - (opos + len(olast.Meta.Orig))
	return
}

func (me Tokens) NumCharsBetweenLastAndFirstOf(other Tokens) (dist int) {
	mlast, ofirst := &me[len(me)-1], &other[0]
	mpos, opos := mlast.Meta.Position.Offset, ofirst.Meta.Position.Offset
	dist = opos - (mpos + len(mlast.Meta.Orig))
	return
}

func (me Tokens) First(matches func(*Token) bool) *Token {
	if len(me) == 0 {
		return nil
	}
	if matches != nil {
		for i := range me {
			if t := &me[i]; matches(t) {
				return t
			}
		}
		return nil
	}
	return &me[0]
}

func (me Tokens) Last(matches func(*Token) bool) *Token {
	if len(me) == 0 {
		return nil
	}
	if matches != nil {
		for i := len(me) - 1; i >= 0; i-- {
			if t := &me[i]; matches(t) {
				return t
			}
		}
		return nil
	}
	return &me[len(me)-1]
}

func (me Tokens) FindSub(beginsWith Tokens, endsWith Tokens) (slice Tokens) {
	beginner, ender := beginsWith.First(nil), endsWith.Last(nil)
	if slice = me.FromUntil(beginner, ender, true); slice == nil {
		var db, de bool
		for i := range me {
			if (!db) && me[i].Meta.Offset == beginner.Meta.Offset {
				db, beginner = true, &me[i]
			} else if db && me[i].Meta.Offset == ender.Meta.Offset {
				de, ender = true, &me[i]
				break
			}
		}
		if de {
			slice = me.FromUntil(beginner, ender, true)
		}
	}
	return
}

func (me Tokens) FromUntil(from *Token, until *Token, incl bool) (slice Tokens) {
	fromisntnil := from != nil
	var startfrom, endbefore int
	if fromisntnil {
		startfrom = -1
	}
	for i := range me {
		if fromisntnil && &me[i] == from {
			startfrom = i
		} else if &me[i] == until {
			if endbefore = i; incl {
				endbefore++
			}
			break
		}
	}
	if startfrom > -1 && endbefore > startfrom {
		slice = me[startfrom:endbefore]
	}
	return
}

func (me Tokens) Length() (length int) {
	if l := len(me); l == 1 {
		length = len(me[0].Meta.Orig)
	} else if l > 1 {
		mfirst, mlast := &me[0].Meta, &me[l-1].Meta
		length = (mlast.Position.Offset - mfirst.Position.Offset) + len(mlast.Orig)
	}
	return
}

func (me Tokens) Pos() *scanner.Position {
	return &me[0].Meta.Position
}

func (me Tokens) BreakOnSpace(sepOpen byte, sepClose byte) (pref Tokens, suff Tokens, didBreak bool) {
	var depth int
	for i := 0; i < len(me); i++ {
		if me[i].flag == TOKEN_SEPISH {
			if me[i].Meta.Orig[0] == sepOpen {
				depth++
			} else if me[i].Meta.Orig[0] == sepClose {
				depth--
			}
		} else if depth == 0 && i > 0 {
			if diff := me[i].Meta.Position.Offset - (me[i-1].Meta.Position.Offset + len(me[i-1].Meta.Orig)); diff > 0 {
				pref, suff, didBreak = me[:i], me[i:], true
				return
			}
		}
	}
	suff = me
	return
}

func (me Tokens) ChunkedBySpacing(sepOpen byte, sepClose byte) (m map[*Token]int) {
	var depth, startfrom int
	for i := range me {
		if depth == 0 && i > 0 {
			if diff := me[i].Meta.Offset - (me[i-1].Meta.Offset + len(me[i-1].Meta.Orig)); diff > 0 {
				if m == nil {
					m = make(map[*Token]int, len(me))
				}
				m[&me[startfrom]] = i - startfrom
				startfrom = i
			}
		}
		if me[i].flag == TOKEN_SEPISH {
			if me[i].Meta.Orig[0] == sepOpen {
				depth++
			} else if me[i].Meta.Orig[0] == sepClose {
				depth--
			}
		}
	}
	if startfrom > 0 {
		m[&me[startfrom]] = len(me) - startfrom
	}
	return
}

// SansComments returns the newly allocated `sans` with a `cap` of `len(me)`
// and containing all `Tokens` in `me` except those with a `Kind` of `TOKEN_COMMENT`.
//
// If `keepIn` is not `nil`, it is filled with all non-comment `Token`s in
// `me` mapped to the indices (in `me`) of their subsequent comment `Token`s.
//
// If `oldIndices` is not `nil`, it keeps track of the original indices in `me`.
func (me Tokens) SansComments(keepIn map[*Token][]int, oldIndices map[*Token]int) (sans Tokens) {
	var nextcopypos int
	sans = make(Tokens, 0, len(me))
	var keeps []int
	for i, keep, oldidx, lastnoncomment := 0, keepIn != nil, oldIndices != nil, -1; i < len(me); i++ {
		iscomment := me[i].flag == TOKEN_COMMENT || me[i].flag == _TOKEN_COMMENT_ENCL
		if keep {
			if !iscomment {
				if lastnoncomment >= 0 && len(keeps) > 0 {
					keepIn[&me[lastnoncomment]], keeps = append(keepIn[&me[lastnoncomment]], keeps...), nil
				}
				lastnoncomment = i
			} else if lastnoncomment >= 0 {
				if keeps = append(keeps, i); i == len(me)-1 {
					keepIn[&me[lastnoncomment]], keeps = append(keepIn[&me[lastnoncomment]], keeps...), nil
				}
			}
		}

		if nucount := (nextcopypos < 0); (!iscomment) && nucount {
			nextcopypos = i
		} else if ls := len(sans); iscomment && (!nucount) {
			if sans = append(sans, me[nextcopypos:i]...); oldidx {
				for j := nextcopypos; j < i; ls, j = ls+1, j+1 {
					oldIndices[&sans[ls]] = j
				}
			}
			nextcopypos = -1
		}
	}
	if ls := len(sans); nextcopypos >= 0 {
		if sans = append(sans, me[nextcopypos:]...); oldIndices != nil {
			for j := nextcopypos; j < len(me); ls, j = ls+1, j+1 {
				oldIndices[&sans[ls]] = j
			}
		}
	}
	return
}

// Sub assumes (but won't check: up to the caller) that `me` begins with a `TokenSep` of
// `sepOpen` and returns in `sub` the subsequence of `Tokens` up until a matching `TokenSep` of
// `sepClose`. If no correct subsequence is found, `sub` is `nil` and `tail` is `me` (and
// `numUnclosed` might be non-`0` to indicate the number of unclosed groupings) — otherwise `sub`
// is the  subsequence immediately following the opening `sepOpen` up to and excluding the matching
// `sepClose`, and `tail` is all trailing `Tokens` immediately following it.
func (me Tokens) Sub(sepOpen byte, sepClose byte) (sub Tokens, tail Tokens, numUnclosed int) {
	tail, numUnclosed = me, 1
	for i := 1; i < len(me); i++ {
		if me[i].flag == TOKEN_SEPISH {
			if me[i].Meta.Orig[0] == sepOpen {
				numUnclosed++
			} else if me[i].Meta.Orig[0] == sepClose {
				if numUnclosed--; numUnclosed == 0 {
					sub, tail = me[1:i], me[i+1:]
					return
				}
			}
		}
	}
	return
}

func (me Tokens) Chunked(byOrig string) (chunks []Tokens) {
	var depth int
	var startfrom int
	skipsubs := len(SepsForChunking) > 0
	for i := range me {
		if depth == 0 && me[i].Meta.Orig == byOrig {
			chunks, startfrom = append(chunks, me[startfrom:i]), i+1
		} else if skipsubs && (me[i].flag == TOKEN_SEPISH || (me[i].flag == TOKEN_OPISH && len(me[i].Meta.Orig) == 1)) {
			for isclosefrom, s := len(SepsForChunking)/2, 0; s < len(SepsForChunking); s++ {
				if isopen := s < isclosefrom; isopen && me[i].Meta.Orig[0] == SepsForChunking[s] {
					depth++
					break
				} else if (!isopen) && me[i].Meta.Orig[0] == SepsForChunking[s] {
					depth--
					break
				}
			}
		}
	}
	if startfrom == 0 {
		chunks = []Tokens{me}
	} else if startfrom <= len(me) {
		chunks = append(chunks, me[startfrom:])
	}
	return
}

func (me Tokens) IsAnyOneOf(any ...string) bool {
	return len(me) == 1 && me[0].IsAnyOneOf(any...)
}

// IndentBasedChunks breaks up `me` into a number of `chunks`:
// each 'non-indented' line (with `LineIndent` <= `minLineIndent`) in `me` begins a new
// 'chunk' and any subsequent 'indented' (`LineIndent` > `minLineIndent`) lines also belong to it.
func (me Tokens) IndentBasedChunks(minLineIndent int) (chunks []Tokens) {
	var cur int
	if minLineIndent < 0 {
		minLineIndent = me[0].Meta.LineIndent
	}
	for i, linenum, l := 0, me[0].Meta.Line, len(me); i < l; i++ {
		if me[i].Meta.Line > linenum && me[i].Meta.LineIndent <= minLineIndent {
			if minLineIndent == 8 {
				panic(me[i].Meta.LineIndent)
			}
			if tlc := me[cur:i]; len(tlc) > 0 {
				chunks = append(chunks, tlc)
			}
			cur, linenum = i, me[i].Meta.Line
		}
	}
	if cur < len(me) {
		if tlc := me[cur:]; len(tlc) > 0 {
			chunks = append(chunks, tlc)
		}
	}
	return
}

func (me Tokens) String() string {
	if len(me) == 0 {
		return ""
	}
	var buf bytes.Buffer
	for i := 0; i < len(me); i++ {
		buf.WriteString(" [")
		buf.WriteString(me[i].String())
		buf.WriteString("]   ")
	}
	return buf.String()[1:]
}
