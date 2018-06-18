package udevlex

import (
	"text/scanner"
)

func Err(pos *scanner.Position, msg string) *Error {
	return &Error{Pos: *pos, msg: msg}
}

// Error holds a message returned by `Error`, plus additional positional details.
type Error struct {
	msg string
	Pos scanner.Position
}

// Error implements Go's standard `error` interface.
func (this *Error) Error() string { return this.msg }
