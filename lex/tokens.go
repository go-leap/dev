package udevlex

import (
	"bytes"
)

type Tokens []Token

// BreakOnIndent returns in `indented` all `Tokens` on the same line as the first in `me`, plus all subsequent `Tokens` with `LineIndent` greater than `minIndent`; and in `outdented` the first and all following `Tokens` with a `LineIndent` less-or-equal (if any).
func (me Tokens) BreakOnIndent(minIndent int) (indented Tokens, outdented Tokens) {
	linenum := me[0].Meta.Line
	for i := 1; i < len(me); i++ {
		if me[i].Meta.Line != linenum && me[i].Meta.LineIndent <= minIndent {
			indented, outdented = me[:i], me[i:]
			return
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
			switch me[i].Str {
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

// BreakOnOther returns all `Tokens` preceding and succeeding the next occurence of the specified `TokenOther` in `me`, if any — otherwise, `me,nil` will be returned.
func (me Tokens) BreakOnOther(token string) (pref Tokens, suff Tokens) {
	pref = me
	for i := 0; i < len(me); i++ {
		if me[i].flag == TOKEN_OTHER && me[i].Str == token {
			pref, suff = me[:i], me[i+1:]
			return
		}
	}
	return
}

// SansComments returns the newly allocated `sans` with a `cap` of `len(me)` and containing all `Tokens` in `me` except those with a `Kind` of `TOKEN_COMMENT`.
func (me Tokens) SansComments() (sans Tokens) {
	var nextcopypos int
	sans = make(Tokens, 0, len(me))
	for i := 0; i < len(me); i++ {
		if nucount, iscomment := (nextcopypos < 0), me[i].flag == TOKEN_COMMENT || me[i].flag == _TOKEN_COMMENT_LONG; (!iscomment) && nucount {
			nextcopypos = i
		} else if iscomment && (!nucount) {
			sans = append(sans, me[nextcopypos:i]...)
			nextcopypos = -1
		}
	}
	if nextcopypos >= 0 {
		sans = append(sans, me[nextcopypos:]...)
	}
	return
}

// Sub assumes (but won't check: up to the caller) that `me` begins with a `TokenSep` of
// `sepOpen` and returns in `sub` the subsequence of `Tokens` up until a matching `TokenSep` of
// `sepClose`. If no correct subsequence is found, `sub` is `nil` and `tail` is `me` (and
// `numUnclosed` might be non-`0` to indicate the number of unclosed groupings) — otherwise `sub`
// is the  subsequence immediately following the opening `sepOpen` up to and excluding the matching
// `sepClose`, and `tail` is all trailing `Tokens` immediately following it.
func (me Tokens) Sub(sepOpen string, sepClose string) (sub Tokens, tail Tokens, numUnclosed int) {
	tail = me
	for i := 1; i < len(me); i++ {
		if me[i].flag == TOKEN_SEP {
			if me[i].Str == sepOpen {
				numUnclosed++
			} else if me[i].Str == sepClose {
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
	for i, linenum, l := 0, 1, len(me); i < l; i++ {
		if i == l-1 {
			if tlc := me[cur:]; len(tlc) > 0 {
				chunks = append(chunks, tlc)
			}
		} else if me[i].Meta.LineIndent <= minIndent && me[i].Meta.Line != linenum {
			if tlc := me[cur:i]; len(tlc) > 0 {
				chunks = append(chunks, tlc)
			}
			cur, linenum = i, me[i].Meta.Line
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
		buf.WriteRune(' ')
		buf.WriteString(me[i].String())
	}
	return buf.String()[1:]
}
