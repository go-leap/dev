package udevlex

import (
	"bytes"
	"strings"
)

// Tokens is a slice of `Token`s.
type Tokens []Token

// BreakOnIndent returns in `indented` all `Tokens` on the same line as the first in `me`,
// plus all subsequent `Tokens` with `LineIndent` greater than `minLineIndent`; and in `outdented`
// the first and all following `Tokens` with a `LineIndent` less-or-equal (if any).
func (me Tokens) BreakOnIndent(minLineIndent int) (indented Tokens, outdented Tokens) {
	if len(me) == 1 || !me.MultipleLines() {
		indented, outdented = me, nil
	} else {
		var chunkfrom int
		for i, ip := 1, 0; i < len(me); i, ip = i+1, ip+1 {
			lastln := me[ip].Ln1
			if me[ip].MultiLine() {
				lastln = lastln + me[ip].NumLFs()
			}
			if me[i].Ln1 != lastln && me[i].LineIndent <= me[0].LineIndent {
				chunkfrom = i
				break
			}
		}
		if chunkfrom == 0 {
			indented, outdented = me, nil
		} else {
			indented, outdented = me[:chunkfrom], me[chunkfrom:]
		}
	}
	return
}

// BreakOnIdent finds the desired occurrence of `needleIdent` in `me`, then returns in `pref` all `Tokens` preceding it and in `suff` all following it.
// If `skipForEachOccurrenceOfIdent` is given, then for every encountered `TokenIdent` occurrence of it one `needleIdent` occurrence will be skipped.
// If `numUnclosed` is not `0`, this typically indicates a syntax error depending on the language being lexed; strictly speaking it denotes the number of skipped-and-not-closed occurrences of `skipForEachOccurrenceOfIdent`.
// Unless a correct break position was found, `pref` and `suff` will both be `nil`.
func (me Tokens) BreakOnIdent(needleIdent string, skipForEachOccurrenceOfIdent string) (pref Tokens, suff Tokens, numUnclosed int) {
	for i := 0; i < len(me); i++ {
		if me[i].Kind == TOKEN_IDENT {
			switch me[i].Lexeme {
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
		if me[i].Kind == TOKEN_OPISH && me[i].Lexeme == token {
			pref, op, suff = me[:i], &me[i], me[i+1:]
			return
		}
	}
	return
}

// CountKind returns the number of `Token`s with the specified `Kind`.
func (me Tokens) CountKind(kind TokenKind) (count int) {
	for i := range me {
		if me[i].Kind == kind {
			count++
		}
	}
	return
}

// Has returns whether any of the `Tokens` was produced from the specified
// original source sub-string.
func (me Tokens) Has(orig string, deep bool) bool {
	return -1 != me.Index(orig, deep)
}

func (me Tokens) ConsistsOnlyOfCommentsOrNothing() bool {
	for i := range me {
		if me[i].Kind != TOKEN_COMMENT {
			return false
		}
	}
	return true
}

func (me Tokens) Index(orig string, deep bool) int {
	var depth int
	skipsubs := len(SepsGroupers) > 0 && !deep
	for i := range me {
		if d := me[i].sepsDepthIncrement(skipsubs); d != 0 {
			depth += d
		} else if depth == 0 && me[i].Lexeme == orig {
			return i
		}
	}
	return -1
}

// HasKind returns whether any of the `Tokens` is of the specified `Kind`.
func (me Tokens) HasKind(kind TokenKind) bool {
	for i := range me {
		if me[i].Kind == kind {
			return true
		}
	}
	return false
}

// HasSpaces returns whether any two consecutive `Tokens` suggest that there is white-space in between each other.
func (me Tokens) HasSpaces() bool {
	for i := 1; i < len(me); i++ {
		if diff := me[i].Pos.Off0 - (me[i-1].Pos.Off0 + len(me[i-1].Lexeme)); diff > 0 {
			return true
		}
	}
	return false
}

// NumCharsBetweenFirstAndLastOf returns the number of characters between the first `Token` in `me` and the end of the last `Token` in `other`.
func (me Tokens) NumCharsBetweenFirstAndLastOf(other Tokens) (dist int) {
	mfirst, olast := &me[0], &other[len(other)-1]
	mpos, opos := mfirst.Pos.Off0, olast.Pos.Off0
	dist = mpos - (opos + len(olast.Lexeme))
	return
}

// NumCharsBetweenLastAndFirstOf returns the number of characters between the first `Token` in `other` and the end of the last `Token` in `me`.
func (me Tokens) NumCharsBetweenLastAndFirstOf(other Tokens) (dist int) {
	mlast, ofirst := &me[len(me)-1], &other[0]
	mpos, opos := mlast.Pos.Off0, ofirst.Pos.Off0
	dist = opos - (mpos + len(mlast.Lexeme))
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
// beginner/ender from `TokenPos.Off0` values of the `First(nil)` of
// `beginsWith` and the `Last(nil)` of `endsWith`. In any case, only the first
// `Token` in `beginsWith` and the last in `endsWith` are ever considered.
func (me Tokens) FindSub(beginsWith Tokens, endsWith Tokens) (slice Tokens) {
	beginner, ender := beginsWith.First(nil), endsWith.Last(nil)
	if slice = me.FromUntil(beginner, ender, true); slice == nil {
		var db, de bool
		for i := range me {
			if (!db) && me[i].Pos.Off0 == beginner.Pos.Off0 {
				db, beginner = true, &me[i]
			} else if db && me[i].Pos.Off0 == ender.Pos.Off0 {
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
		return me[0].Pos.Off0 <= pos0ByteOffset && pos0ByteOffset <= (me[len(me)-1].Pos.Off0+len(me[len(me)-1].Lexeme))
	}
	return false
}

// EqLenAndOffsets returns at least whether `me` and `toks` have the same `len` and the `First` and `Last` of both share the same `TokenPos.Off0`. If `checkInnerOffsetsToo` is `true`, all other `Tokens` (not just the `First` and `Last` ones) are compared as well.
func (me Tokens) EqLenAndOffsets(toks Tokens, checkInnerOffsetsToo bool) bool {
	if l := len(me); l > 0 && l == len(toks) {
		if me[0].Pos.Off0 == toks[0].Pos.Off0 && me[l-1].Pos.Off0 == toks[l-1].Pos.Off0 {
			if checkInnerOffsetsToo {
				for i := 1; i < l-1; i++ {
					if me[i].Pos.Off0 != toks[i].Pos.Off0 {
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
		length = len(me[0].Lexeme)
	} else if l > 1 {
		tfirst, tlast := &me[0], &me[l-1]
		length = (tlast.Pos.Off0 - tfirst.Pos.Off0) + len(tlast.Lexeme)
	}
	return
}

// Pos returns the `TokenPos` of the `First` `Token` in `me`.
func (me Tokens) Pos() *Pos {
	if len(me) > 0 {
		return &me[0].Pos
	}
	return nil
}

// BreakOnSpace splits up `me` into `pref` and `suff` between the first two consecutive `Tokens` that suggest white-space in between each other. If no such pair exists, `didBreak` is `false` and `pref` is `nil` and `suff` is `me`.
func (me Tokens) BreakOnSpace(deep bool) (pref Tokens, suff Tokens, didBreak bool) {
	var depth int
	skipsubs := (len(SepsGroupers) > 0 && !deep)
	for i := 0; i < len(me); i++ {
		if d := me[i].sepsDepthIncrement(skipsubs); d != 0 {
			depth += d
			continue
		}
		if depth == 0 && i > 0 {
			if diff := me[i].Pos.Off0 - (me[i-1].Pos.Off0 + len(me[i-1].Lexeme)); diff > 0 {
				pref, suff, didBreak = me[:i], me[i:], true
				return
			}
		}
	}
	suff = me
	return
}

// Cliques records in `nums` any `Token` in `me` that begins a sequence of
// non-white-space-separated lexemes and its number of `Tokens` If no such
// sequence exists or if it would equal `me` entirely, `nums` will be `nil`,
// else any value in it will be `> 1`. If `isBreaker` isn't `nil`, it can
// identify language-specific tokens that break up this logic, such as `,`.
func (me Tokens) Cliques(isBreaker func(idxCur int, idxLast int) bool) (nums map[*Token]int) {
	if len(me) <= 1 {
		return
	}
	var depth, startfrom int
	var wascomment, isbreaker, wasbreaker bool
	d0, skipsubs, idxlast := true, (len(SepsGroupers) > 0), len(me)-1
	for i := range me {
		var iscomment bool
		issepish := me[i].Kind == TOKEN_SEPISH
		if d0 {
			iscomment, isbreaker =
				me[i].Kind == TOKEN_COMMENT, isBreaker != nil && isBreaker(i, idxlast)
			if i > 0 {
				diff := me[i].Pos.Off0 - (me[i-1].Pos.Off0 + len(me[i-1].Lexeme))
				if diff > 0 || wascomment || iscomment || wasbreaker || isbreaker {
					if n := i - startfrom; n > 1 {
						if nums == nil {
							nums = make(map[*Token]int, len(me)/2)
						}
						nums[&me[startfrom]] = n
					}
					startfrom = i
				}
			}
		}
		if wasbreaker = isbreaker; !issepish {
			wascomment = iscomment
		} else {
			depth += me[i].sepsDepthIncrement(skipsubs)
			d0 = (depth == 0)
		}
	}
	if startfrom > 0 {
		if n := len(me) - startfrom; n > 1 {
			if nums == nil {
				nums = make(map[*Token]int, 1)
			}
			nums[&me[startfrom]] = n
		}
	}
	return
}

// BreakOnLeadingComments returns the `leadingComments` (which could be empty) as well as the `rest`.
func (me Tokens) BreakOnLeadingComments() (leadingComments Tokens, rest Tokens) {
	if restfrom := len(me); restfrom > 0 {
		for i := range me {
			if me[i].Kind != TOKEN_COMMENT {
				restfrom = i
				break
			}
		}
		leadingComments, rest = me[:restfrom], me[restfrom:]
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
		iscomment := me[i].Kind == TOKEN_COMMENT
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
		if me[i].Kind == TOKEN_SEPISH {
			if me[i].Lexeme[0] == sepOpen {
				numUnclosed++
			} else if me[i].Lexeme[0] == sepClose {
				if numUnclosed--; numUnclosed == 0 {
					sub, tail = me[1:i], me[i+1:]
					return
				}
			}
		}
	}
	return
}

// Chunked splits `me` into `chunks` separated by `TokenLexeme` occurrences of `byOrig`,
// stopping at the first occurrence of `stopChunkingOn` (if specified).
func (me Tokens) Chunked(byOrig string, stopChunkingOn string) (chunks []Tokens) {
	var depth int
	var startfrom int
	hasignore, skipsubs := (stopChunkingOn != ""), (len(SepsGroupers) > 0)
	for i := range me {
		if depth == 0 {
			if me[i].Lexeme == byOrig {
				chunks, startfrom = append(chunks, me[startfrom:i]), i+1
			} else if hasignore && me[i].Lexeme == stopChunkingOn {
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

func (me Tokens) ChunkedByIndent(moveCommentOnlyChunks bool, ifMovePreferNextInsteadOfPrev bool) (chunks []Tokens) {
	var chunkfrom int
	if len(me) == 1 || !me.MultipleLines() {
		return []Tokens{me}
	}

	lineind := me[chunkfrom].LineIndent
	for i, ip := 1, 0; i < len(me); i, ip = i+1, ip+1 {
		lastln := me[ip].Ln1
		if me[ip].MultiLine() {
			lastln = lastln + me[ip].NumLFs()
		}
		if me[i].Ln1 != lastln {
			if me[i].LineIndent <= lineind {
				lineind = me[i].LineIndent
				chunkfrom, chunks = i, append(chunks, me[chunkfrom:i])
			}
		}
	}
	if chunkfrom < len(me) {
		chunks = append(chunks, me[chunkfrom:])
	}

	if moveCommentOnlyChunks {
		for i := 0; i < len(chunks); i++ {
			if cur := chunks[i]; cur.ConsistsOnlyOfCommentsOrNothing() {
				pref, suff := chunks[:i], chunks[i+1:]
				if pl := len(pref) - 1; (ifMovePreferNextInsteadOfPrev || pl < 0) && len(suff) != 0 {
					suff[0] = append(cur, suff[0]...)
				} else if pl >= 0 {
					pref[pl] = append(pref[pl], cur...)
				} else {
					continue
				}
				chunks, i = append(pref, suff...), i-1
			}
		}
	}
	return
}

// IsAnyOneOf calls `Token.IsAnyOneOf` is `me` has only one `Token`.
func (me Tokens) IsAnyOneOf(any ...string) bool {
	return len(me) == 1 && me[0].IsAnyOneOf(any...)
}

func (me Tokens) MultipleLines() bool {
	if len(me) > 1 && me[len(me)-1].Ln1 != me[0].Ln1 {
		return true
	}
	for i := range me {
		if me[i].MultiLine() {
			return true
		}
	}
	return false
}

func (me Tokens) Orig() (s string) {
	var prev *Token
	for i := range me {
		if prev != nil {
			s += strings.Repeat(" ", me[i].Pos.Off0-(prev.Pos.Off0+len(prev.Lexeme)))
		}
		prev, s = &me[i], s+me[i].Lexeme
	}
	return
}

// String returns a representation of `Tokens` handy for troubleshooting / diagnostics.
func (me Tokens) String() string {
	if len(me) == 0 {
		return ""
	}
	var buf bytes.Buffer
	for i := 0; i < len(me); i++ {
		buf.WriteString("·")
		buf.WriteString(me[i].String())
	}
	return buf.String()[2:]
}
