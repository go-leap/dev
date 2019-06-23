package udevlex

import (
	"bytes"
	"strconv"
	"text/scanner"
)

// Tokens is a slice of `Token`s.
type Tokens []Token

// BreakOnIndent returns in `indented` all `Tokens` on the same line as the first in `me`,
// plus all subsequent `Tokens` with `LineIndent` greater than `minLineIndent`; and in `outdented`
// the first and all following `Tokens` with a `LineIndent` less-or-equal (if any).
func (me Tokens) BreakOnIndent(minLineIndent int) (indented Tokens, outdented Tokens) {
	if len(me) > 0 {
		linenum := me[0].Meta.Pos.Line
		for i := 1; i < len(me); i++ {
			if me[i].Meta.Pos.Line != linenum && me[i].Meta.LineIndent <= minLineIndent {
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

// CountKind returns the number of `Token`s with the specified `Kind`.
func (me Tokens) CountKind(kind TokenKind) (count int) {
	for i := range me {
		if me[i].Kind() == kind {
			count++
		}
	}
	return
}

// Has returns whether any of the `Tokens` was produced from the specified
// original source sub-string.
func (me Tokens) Has(orig string, deep bool) bool {
	var depth int
	skipsubs := len(SepsForChunking) > 0 && !deep
	for i := range me {
		if d := me[i].sepsDepthIncrement(skipsubs); d != 0 {
			depth += d
		} else if me[i].Meta.Orig == orig {
			return true
		}
	}
	return false
}

// HasKind returns whether any of the `Tokens` is of the specified `Kind`.
func (me Tokens) HasKind(kind TokenKind) bool {
	for i := range me {
		if me[i].Kind() == kind {
			return true
		}
	}
	return false
}

// HasSpaces returns whether any two consecutive `Tokens` suggest that there is white-space in between each other.
func (me Tokens) HasSpaces() bool {
	for i := 1; i < len(me); i++ {
		if diff := me[i].Meta.Pos.Offset - (me[i-1].Meta.Pos.Offset + len(me[i-1].Meta.Orig)); diff > 0 {
			return true
		}
	}
	return false
}

// NumCharsBetweenFirstAndLastOf returns the number of characters between the first `Token` in `me` and the end of the last `Token` in `other`.
func (me Tokens) NumCharsBetweenFirstAndLastOf(other Tokens) (dist int) {
	mfirst, olast := &me[0], &other[len(other)-1]
	mpos, opos := mfirst.Meta.Pos.Offset, olast.Meta.Pos.Offset
	dist = mpos - (opos + len(olast.Meta.Orig))
	return
}

// NumCharsBetweenLastAndFirstOf returns the number of characters between the first `Token` in `other` and the end of the last `Token` in `me`.
func (me Tokens) NumCharsBetweenLastAndFirstOf(other Tokens) (dist int) {
	mlast, ofirst := &me[len(me)-1], &other[0]
	mpos, opos := mlast.Meta.Pos.Offset, ofirst.Meta.Pos.Offset
	dist = opos - (mpos + len(mlast.Meta.Orig))
	return
}

// First returns the first `Token` if `matches` is `nil`, else the first for which it returns `true`.
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

func (me Tokens) First1() (r *Token) {
	if len(me) > 0 {
		r = &me[0]
	}
	return
}

func (me Tokens) Last1() (r *Token) {
	if len(me) > 0 {
		r = &me[len(me)-1]
	}
	return
}

// Last returns the last `Token` if `matches` is `nil`, else the last one for which it returns `true`.
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

func (me Tokens) Prev(before *Token, fallback bool) *Token {
	for i := range me {
		if &me[i] == before && i > 0 {
			return &me[i-1]
		}
	}
	if fallback {
		return before
	}
	return nil
}

func (me Tokens) Next(after *Token, fallback bool) *Token {
	for i := range me {
		if &me[i] == after && i < (len(me)-1) {
			return &me[i+1]
		}
	}
	if fallback {
		return after
	}
	return nil
}

// FindSub initially calls `FromUntil` but if the result is `nil` because
// `beginsWith` / `endsWith` aren't sub-slices of `me`, it figures out the proper
// beginner/ender from `TokenMeta.Pos.Offset` values of the `First(nil)` of
// `beginsWith` and the `Last(nil)` of `endsWith`. In any case, only the first
// `Token` in `beginsWith` and the last in `endsWith` are ever considered.
func (me Tokens) FindSub(beginsWith Tokens, endsWith Tokens) (slice Tokens) {
	beginner, ender := beginsWith.First(nil), endsWith.Last(nil)
	if slice = me.FromUntil(beginner, ender, true); slice == nil {
		var db, de bool
		for i := range me {
			if (!db) && me[i].Meta.Pos.Offset == beginner.Meta.Pos.Offset {
				db, beginner = true, &me[i]
			} else if db && me[i].Meta.Pos.Offset == ender.Meta.Pos.Offset {
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

// Between returns the `Tokens` that come after `after` and before `before`.
func (me Tokens) Between(after *Token, before *Token) (slice Tokens) {
	if after != nil && before != nil {
		var hasb bool
		posb, posa := -1, -1
		for i := range me {
			if hasb {
				if &me[i] == after {
					posa = i
					break
				}
			} else if &me[i] == before {
				posb, hasb = i, true
			}
		}
		if hasb && posa > 0 {
			slice = me[posb+1 : posa]
		}
	}
	return
}

// AreEnclosing returns whether the `Tokens` enclose the specified 0-based, byte-based offset position.
func (me Tokens) AreEnclosing(pos0ByteOffset int) bool {
	if len(me) > 0 {
		return me[0].Meta.Pos.Offset <= pos0ByteOffset && pos0ByteOffset <= (me[len(me)-1].Meta.Pos.Offset+len(me[len(me)-1].Meta.Orig))
	}
	return false
}

// EqLenAndOffsets returns at least whether `me` and `toks` have the same `len` and the `First` and `Last` of both share the same `TokenMeta.Pos.Offset`. If `checkInnerOffsetsToo` is `true`, all other `Tokens` (not just the `First` and `Last` ones) are compared as well.
func (me Tokens) EqLenAndOffsets(toks Tokens, checkInnerOffsetsToo bool) bool {
	if l := len(me); l > 0 && l == len(toks) {
		if me[0].Meta.Pos.Offset == toks[0].Meta.Pos.Offset && me[l-1].Meta.Pos.Offset == toks[l-1].Meta.Pos.Offset {
			if checkInnerOffsetsToo {
				for i := 1; i < l-1; i++ {
					if me[i].Meta.Pos.Offset != toks[i].Meta.Pos.Offset {
						return false
					}
				}
			}
			return true
		}
	}
	return false
}

// FromUntil returns the `Tokens` from `from` (or the beginning, if `nil`) until `until` (or the end, if `nil`). If `incl` is `true`, `until` is included in `slice`.
func (me Tokens) FromUntil(from *Token, until *Token, incl bool) Tokens {
	if len(me) == 0 || (incl && (from == nil || from == &me[0]) && (until == nil || until == &me[len(me)-1])) {
		return me
	}
	var startfrom, endbefore int
	if from != nil {
		startfrom = -1
	}
	if until == nil {
		endbefore = len(me) - 1
	} else {
		endbefore = -1
	}
	for i := range me {
		if startfrom < 0 && &me[i] == from {
			startfrom = i
		}
		if endbefore < 0 && &me[i] == until {
			endbefore = i
		}
		if startfrom > -1 && endbefore > -1 {
			if incl {
				endbefore++
			}
			if endbefore >= startfrom {
				return me[startfrom:endbefore]
			}
		}
	}
	return nil
}

// Length returns the length of the original source sub-string that this `Tokens` slice represents (without traversing it).
func (me Tokens) Length() (length int) {
	if l := len(me); l == 1 {
		length = len(me[0].Meta.Orig)
	} else if l > 1 {
		mfirst, mlast := &me[0].Meta, &me[l-1].Meta
		length = (mlast.Pos.Offset - mfirst.Pos.Offset) + len(mlast.Orig)
	}
	return
}

// Pos returns the `TokenMeta.Pos` of the `First` `Token` in `me`.
func (me Tokens) Pos() *scanner.Position {
	if len(me) > 0 {
		return &me[0].Meta.Pos
	}
	return nil
}

// BreakOnSpace splits up `me` into `pref` and `suff` between the first two consecutive `Tokens` that suggest white-space in between each other. If no such pair exists, `didBreak` is `false` and `pref` is `nil` and `suff` is `me`.
func (me Tokens) BreakOnSpace(deep bool) (pref Tokens, suff Tokens, didBreak bool) {
	var depth int
	skipsubs := len(SepsForChunking) > 0 && !deep
	for i := 0; i < len(me); i++ {
		if d := me[i].sepsDepthIncrement(skipsubs); d != 0 {
			depth += d
			continue
		}
		if depth == 0 && i > 0 {
			if diff := me[i].Meta.Pos.Offset - (me[i-1].Meta.Pos.Offset + len(me[i-1].Meta.Orig)); diff > 0 {
				pref, suff, didBreak = me[:i], me[i:], true
				return
			}
		}
	}
	suff = me
	return
}

// CrampedOnes records in `m` any `Token` in `me` that begins a sequence of non-white-space-separated
// lexemes and the length of that sequence. If no such sequence exists, `m` will be `nil`.
// If `isBreaker` isn't `nil`, it can identify language-specific tokens that break this logic, such as `,`.
func (me Tokens) CrampedOnes(isBreaker func(int) bool) (m map[*Token]int) {
	var depth, startfrom int
	var wascomment, isbreaker, wasbreaker bool
	skipsubs := len(SepsForChunking) > 0
	for i := range me {
		d0 := (depth == 0)
		iscomment := d0 && (me[i].flag == TOKEN_COMMENT || me[i].flag == _TOKEN_COMMENT_ENCL)
		isbreaker = d0 && isBreaker != nil && isBreaker(i)
		if d0 && i > 0 {
			if diff := me[i].Meta.Pos.Offset - (me[i-1].Meta.Pos.Offset + len(me[i-1].Meta.Orig)); diff > 0 || wascomment || iscomment || wasbreaker || isbreaker {
				if m == nil {
					m = make(map[*Token]int, len(me))
				}
				m[&me[startfrom]] = i - startfrom
				startfrom = i
			}
		}
		if me[i].flag != TOKEN_SEPISH {
			wascomment, wasbreaker = iscomment, isbreaker
		} else {
			depth += me[i].sepsDepthIncrement(skipsubs)
		}
	}
	if startfrom > 0 {
		m[&me[startfrom]] = len(me) - startfrom
	}
	return
}

// BreakOnLeadingComments returns the `leadingComments` (which could be empty) as well as the `rest`.
func (me Tokens) BreakOnLeadingComments() (leadingComments Tokens, rest Tokens) {
	var stopbefore int
	for i := range me {
		if me[i].flag != _TOKEN_COMMENT_ENCL && me[i].flag != TOKEN_COMMENT {
			stopbefore = i
			break
		}
	}
	leadingComments, rest = me[:stopbefore], me[stopbefore:]
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

// Sub assumes (but won't check: up to the caller) that `me` begins with a `TOKEN_SEPISH` of
// `sepOpen` and returns in `sub` the subsequence of `Tokens` up until a matching `TOKEN_SEPISH` of
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

// Chunked splits `me` into `chunks` separated by `TokenMeta.Orig` occurrences of `byOrig`,
// stopping at the first occurrence of `stopChunkingOn` (if specified).
func (me Tokens) Chunked(byOrig string, stopChunkingOn string) (chunks []Tokens) {
	var depth int
	var startfrom int
	hasignore, skipsubs := stopChunkingOn != "", len(SepsForChunking) > 0
	for i := range me {
		if depth == 0 {
			if me[i].Meta.Orig == byOrig {
				chunks, startfrom = append(chunks, me[startfrom:i]), i+1
			} else if hasignore && me[i].Meta.Orig == stopChunkingOn {
				break
			}
		}
		depth += me[i].sepsDepthIncrement(skipsubs)
	}
	if startfrom == 0 {
		chunks = []Tokens{me}
	} else if startfrom <= len(me) {
		chunks = append(chunks, me[startfrom:])
	}
	return
}

// IsAnyOneOf calls `Token.IsAnyOneOf` is `me` has only one `Token`.
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
	for i, linenum, l := 0, me[0].Meta.Pos.Line, len(me); i < l; i++ {
		if me[i].Meta.Pos.Line > linenum && me[i].Meta.LineIndent <= minLineIndent {
			if tlc := me[cur:i]; len(tlc) > 0 {
				chunks = append(chunks, tlc)
			}
			cur, linenum = i, me[i].Meta.Pos.Line
		}
	}
	if cur < len(me) {
		if tlc := me[cur:]; len(tlc) > 0 {
			chunks = append(chunks, tlc)
		}
	}
	return
}

// SanitizeDirtyFloatsNextToDotOpishs attempts to undo sometimes-unwanted
// float tokenizations by `text/scanner`, such as `0..1` into `0.0` and `0.1`,
// or `0...1` into `0.0` and `.` and `0.1`, replacing with the corresponding
// uint / dot `Token` combinations instead. `i > 0 && i < len(me)` must hold.
func (me *Tokens) SanitizeDirtyFloatsNextToDotOpishs(i int) {
	if mod, tokens := false, *me; i > 0 && tokens[i].Meta.Pos.Line == tokens[i-1].Meta.Pos.Line {
		switch {
		case tokens[i].flag == TOKEN_OPISH && tokens[i-1].flag == TOKEN_FLOAT && tokens[i-1].Meta.Orig[len(tokens[i-1].Meta.Orig)-1] == '.' &&
			0 == (tokens[i].Meta.Pos.Offset-(tokens[i-1].Meta.Pos.Offset+len(tokens[i-1].Meta.Orig))):

			tokens[i].Meta.Orig = "." + tokens[i].Meta.Orig
			tokens[i].Str = "." + tokens[i].Str
			tokens[i].Meta.Pos.Offset--
			tokens[i].Meta.Pos.Column--
			tokens[i-1].flag = 10
			tokens[i-1].Meta.Orig = tokens[i-1].Meta.Orig[:len(tokens[i-1].Meta.Orig)-1]
			tokens[i-1].Uint, _ = strconv.ParseUint(tokens[i-1].Meta.Orig, 10, 64)

		case tokens[i].flag == TOKEN_FLOAT && tokens[i].Meta.Orig[0] == '.' &&
			0 == (tokens[i].Meta.Pos.Offset-(tokens[i-1].Meta.Pos.Offset+len(tokens[i-1].Meta.Orig))):

			var f2ui bool
			if tokens[i-1].flag == TOKEN_OPISH {
				tokens[i-1].Str += "."
				tokens[i-1].Meta.Orig += "."
				f2ui = true
			} else if tokens[i-1].flag == TOKEN_FLOAT && tokens[i-1].Meta.Orig[len(tokens[i-1].Meta.Orig)-1] == '.' {
				tokens[i-1].Meta.Orig = tokens[i-1].Meta.Orig[:len(tokens[i-1].Meta.Orig)-1]
				tokens[i-1].flag = 10
				tokens[i-1].Uint, _ = strconv.ParseUint(tokens[i-1].Meta.Orig, 10, 64)
				pref, suff := tokens[:i], tokens[i:]
				var dots Token
				dots.flag, dots.Str, dots.Meta = TOKEN_OPISH, "..", tokens[i].Meta
				dots.Meta.Orig = ".."
				dots.Meta.Pos.Offset--
				dots.Meta.Pos.Column--
				mod, tokens = true, append(pref, append(Tokens{dots}, suff...)...)
				f2ui, i = true, i+1
			}
			if f2ui {
				tokens[i].Meta.Pos.Offset++
				tokens[i].Meta.Pos.Column++
				tokens[i].Meta.Orig = tokens[i].Meta.Orig[1:]
				tokens[i].flag = 10
				tokens[i].Uint, _ = strconv.ParseUint(tokens[i].Meta.Orig, 10, 64)
			}
		}
		if mod {
			*me = tokens
		}
	}
}

// String returns a representation of `Tokens` handy for troubleshooting / diagnostics.
func (me Tokens) String() string {
	if len(me) == 0 {
		return ""
	}
	var buf bytes.Buffer
	for i := 0; i < len(me); i++ {
		buf.WriteString(" ‹")
		buf.WriteString(me[i].String())
		buf.WriteString("›   ")
	}
	return buf.String()[1:]
}
