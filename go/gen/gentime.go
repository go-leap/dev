package udevgogen

// USUALLY serves as a codegen-time readability wrapper for `GEN_BYCASE` callers.
type USUALLY ISyn

// UNLESS serves as a codegen-time readability wrapper for `GEN_BYCASE` callers.
type UNLESS map[bool]ISyn

// GEN_IF returns either none, all, or one of `stmts` depending on `check` and as follows:
//
// - if there are 2 `stmts` and _each one_ is a `Syns`, they're **then/else**-like and one returns
//
// - otherwise: if `check` is `true`, all `stmts` are returned, else `nil` is returned
func GEN_IF(check bool, stmts ...ISyn) Syns {
	if len(stmts) == 2 {
		if otherwise, okelse := stmts[1].(Syns); okelse {
			if then, okthen := stmts[0].(Syns); okthen {
				if check {
					return then
				}
				return otherwise
			}
		}
	}
	if check {
		return stmts
	}
	return nil
}

// GEN_BYCASE is like a codegen-time `switch..case` construct:
// it returns `unless[true]` if present, else `byDefault`.
func GEN_BYCASE(byDefault USUALLY, unless UNLESS) ISyn {
	if then, ok := unless[true]; ok {
		return then
	}
	return byDefault
}

// GEN_FOR is a codegen-time iterating `Syns`-builder. Traversing `sl` with the
// given `step` skip-length, it calls `each` once per iteration with a fresh
// sub-slice of `sl` that has a `len` of usually `step` and always greater than zero (but
// it might be less than `step` in the very last iteration depending on the `len` of `sl`).
func GEN_FOR(sl []Any, step int, each func([]Any) ISyn) (yield Syns) {
	for i := 0; i < len(sl); i += step {
		idx := i + step
		if idx > len(sl) {
			idx = len(sl)
		}
		yield = append(yield, each(sl[i:idx]))
	}
	return
}
