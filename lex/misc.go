package udevlex

import (
	"text/scanner"
)

func Err(pos *scanner.Position, msg string) *Error {
	return &Error{Pos: *pos, Msg: msg}
}

// Error holds a message returned by `Error`, plus additional positional details.
type Error struct {
	Msg string
	Pos scanner.Position
}

// At returns `Pos` (but allows for callers' interface shenanigans).
func (me *Error) At() *scanner.Position {
	return &me.Pos
}

// Error implements Go's standard `error` interface.
func (me *Error) Error() string {
	return me.Pos.String() + ": " + me.Msg
}
