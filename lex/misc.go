package udevlex

import (
	"text/scanner"
)

// Err returns a newly constructed `Error` with the given `Msg` and `Pos`.
func Err(pos *scanner.Position, msg string) *Error {
	return &Error{Pos: *pos, Msg: msg}
}

// Error holds a message obtained via `Scanner.Error`, plus additional positional details.
type Error struct {
	Msg string
	Pos scanner.Position
}

// Error implements Go's standard `error` interface.
func (me *Error) Error() string {
	return me.Pos.String() + ": " + me.Msg
}
