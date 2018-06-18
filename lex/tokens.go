package udevlex

import (
	"bytes"
)

type Tokens []Token

// BreakOnIndent returns in `indented` all `Tokens` on the same line as the first in `this`, plus all subsequent `Tokens` with `LineIndent` greater than `minIndent`; and in `outdented` the first and all following `Tokens` with a `LineIndent` less-or-equal (if any).
func (this Tokens) BreakOnIndent(minIndent int) (indented Tokens, outdented Tokens) {
	linenum := this[0].Meta.Line
	for i := 1; i < len(this); i++ {
		if this[i].Meta.Line != linenum && this[i].Meta.LineIndent <= minIndent {
			indented, outdented = this[:i], this[i:]
			return
		}
	}
	indented = this
	return
}

// BreakOnIdent finds the desired occurrence of `needleIdent` in `this`, then returns in `pref` all `Tokens` preceding it and in `suff` all following it.
// If `skipForEachOccurrenceOfIdent` is given, then for every encountered `TokenIdent` occurrence of it one `needleIdent` occurrence will be skipped.
// If `numUnclosed` is not `0`, this typically indicates a syntax error depending on the language being lexed; strictly speaking it denotes the number of skipped-and-not-closed occurrences of `skipForEachOccurrenceOfIdent`.
// Unless a correct break position was found, `pref` and `suff` will both be `nil`.
func (this Tokens) BreakOnIdent(needleIdent string, skipForEachOccurrenceOfIdent string) (pref Tokens, suff Tokens, numUnclosed int) {
	for i := 0; i < len(this); i++ {
		if this[i].flag == TOKEN_IDENT {
			switch this[i].Str {
			case skipForEachOccurrenceOfIdent:
				numUnclosed++
			case needleIdent:
				if numUnclosed > 0 {
					numUnclosed--
				} else {
					pref, suff = this[:i], this[i+1:]
					return
				}
			}
		}
	}
	return
}

// BreakOnOther returns all `Tokens` preceding and succeeding the next occurence of the specified `TokenOther` in `this`, if any — otherwise, `this,nil` will be returned.
func (this Tokens) BreakOnOther(token string) (pref Tokens, suff Tokens) {
	pref = this
	for i := 0; i < len(this); i++ {
		if this[i].flag == TOKEN_OTHER && this[i].Str == token {
			pref, suff = this[:i], this[i+1:]
			return
		}
	}
	return
}

// SansComments returns the newly allocated `sans` with a `cap` of `len(this)` and containing all `Tokens` in `this` except those with a `Kind` of `TOKEN_COMMENT`.
func (this Tokens) SansComments() (sans Tokens) {
	var nextcopypos int
	sans = make(Tokens, 0, len(this))
	for i := 0; i < len(this); i++ {
		if nucount, iscomment := (nextcopypos < 0), this[i].flag == TOKEN_COMMENT || this[i].flag == _TOKEN_COMMENT_LONG; (!iscomment) && nucount {
			nextcopypos = i
		} else if iscomment && (!nucount) {
			sans = append(sans, this[nextcopypos:i]...)
			nextcopypos = -1
		}
	}
	if nextcopypos >= 0 {
		sans = append(sans, this[nextcopypos:]...)
	}
	return
}

// Sub assumes (but won't check: up to the caller) that `this` begins with a `TokenSep` of
// `sepOpen` and returns in `sub` the subsequence of `Tokens` up until a matching `TokenSep` of
// `sepClose`. If no correct subsequence is found, `sub` is `nil` and `tail` is `this` (and
// `numUnclosed` might be non-`0` to indicate the number of unclosed groupings) — otherwise `sub`
// is the  subsequence immediately following the opening `sepOpen` up to and excluding the matching
// `sepClose`, and `tail` is all trailing `Tokens` immediately following it.
func (this Tokens) Sub(sepOpen string, sepClose string) (sub Tokens, tail Tokens, numUnclosed int) {
	tail = this
	for i := 1; i < len(this); i++ {
		if this[i].flag == TOKEN_SEP {
			if this[i].Str == sepOpen {
				numUnclosed++
			} else if this[i].Str == sepClose {
				if numUnclosed == 0 {
					sub, tail = this[1:i], this[i+1:]
					return
				}
				numUnclosed--
			}
		}
	}
	return
}

// IndentBasedChunks breaks up `this` into a number of `chunks`:
// each 'non-indented' line (with `LineIndent` <= `minIndent`) in `this` begins a new
// 'chunk' and any subsequent 'indented' (`LineIndex` > `minIndent`) lines also belong to it.
func (this Tokens) IndentBasedChunks(minIndent int) (chunks []Tokens) {
	var cur int
	for i, linenum, l := 0, 1, len(this); i < l; i++ {
		if i == l-1 {
			if tlc := this[cur:]; len(tlc) > 0 {
				chunks = append(chunks, tlc)
			}
		} else if this[i].Meta.LineIndent <= minIndent && this[i].Meta.Line != linenum {
			if tlc := this[cur:i]; len(tlc) > 0 {
				chunks = append(chunks, tlc)
			}
			cur, linenum = i, this[i].Meta.Line
		}
	}
	return
}

func (this Tokens) String() string {
	if len(this) == 0 {
		return ""
	}
	var buf bytes.Buffer
	for i := 0; i < len(this); i++ {
		buf.WriteRune('·')
		buf.WriteString(this[i].String())
	}
	return buf.String()[1:]
}
