package udevlex

import (
	"strings"
)

// Error holds a message obtained via `Scanner.Error`, plus additional positional details.
type Error struct {
	Msg string
	Pos
}

// Error implements Go's standard `error` interface.
func (me *Error) Error() string {
	return me.Pos.String() + ": " + me.Msg
}

func SepsGrouperCloserForOpener(opener byte) (closer byte) {
	if i := strings.IndexByte(SepsGroupers, opener); i >= 0 {
		return SepsGroupers[len(SepsGroupers)-(i+1)]
	}
	return
}
