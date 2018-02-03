package udevlex

import (
	"text/scanner"
)

func Err(pos *scanner.Position, msg string) *Error {
	return &Error{Pos: *pos, msg: msg}
}

// Error holds a message returned by `Error` and `String`, plus additional positional details.
type Error struct {
	msg string
	Pos scanner.Position
}

// Error implements the `error` interface.
func (me *Error) Error() string { return me.msg }

type IPos interface {
	Pos() *TokenMeta
}

// Pos returns the last in `tokens`, or `fallback`, or a new `TokenMeta` at position 1,1 for `fallbackFilePath`.
func Pos(tokens Tokens, fallback IPos, fallbackFilePath string) IPos {
	if l := len(tokens); l > 0 {
		return tokens[l-1]
	}
	if fallback != nil {
		return fallback
	}
	return &TokenMeta{Position: scanner.Position{Line: 1, Column: 1, Filename: fallbackFilePath}}
}
