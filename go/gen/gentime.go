package udevgogen

// USUALLY serves as a codegen-time readability wrapper for `GEN_BYCASE` callers.
type USUALLY ISyn

// UNLESS serves as a codegen-time readability wrapper for `GEN_BYCASE` callers.
type UNLESS map[bool]ISyn

// GEN_MAYBE returns `maybe` if not `nil`, otherwise an empty `Syns`.
func GEN_MAYBE(maybe ISyn) ISyn {
	if maybe == nil {
		return Syns(nil)
	}
	return maybe
}

// GEN_EITHER returns `then` if `check`, else `otherwise`.
func GEN_EITHER(check bool, then ISyn, otherwise ISyn) ISyn {
	if check {
		return then
	}
	return otherwise
}

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

// GEN_FOR_IN is a codegen-time iterating `Syns`-builder. It calls `do` once per
// iteration with the current index, which is equal to `start` in the very first
// iteration, never less-than zero and always less-than `whileLessThan`.
func GEN_FOR(start int, whileLessThan int, incrementBy int, do func(int) ISyn) (yield Syns) {
	yield = make(Syns, 0, (whileLessThan-start)/incrementBy)
	for i := start; (i >= 0) && (i < whileLessThan); i += incrementBy {
		yield = append(yield, do(i))
	}
	return
}

// GEN_FOR_IN is a codegen-time iterating `Syns`-builder. Traversing `sl` with the given `step`
// skip-length, it calls `each` once per iteration with the current index into `sl` and the next
// sub-slice of `sl` (at that index) that has a `len` of usually `step` and always greater than zero
// (but it might be less than `step` in the very last iteration depending on the `len` of `sl`).
func GEN_FOR_IN(sl []IAny, step int, each func(int, []IAny) ISyn) (yield Syns) {
	yield = make(Syns, 0, len(sl)/step)
	for i := 0; i < len(sl); i += step {
		max := i + step
		if max > len(sl) {
			max = len(sl)
		}
		yield = append(yield, each(i, sl[i:max]))
	}
	return
}
