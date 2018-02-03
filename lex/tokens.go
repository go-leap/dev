package udevlex

type Tokens []IToken

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
