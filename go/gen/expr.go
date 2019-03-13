package udevgogen

// IExprDottish is implemented by `ISyn`s that also wish
// to offer dot-accessor syntax over the `D` constructor.
type IExprDottish interface {
	ISyn

	C(string, ...IAny) *ExprCall
	D(...IAny) OpDot
}

// IExprDeclish is implemented by `IExprVarish`s that also wish
// to offer dot-accessor syntax over the `Decl` constructor.
type IExprDeclish interface {
	IExprVarish

	// expr.Let(foo) == Decl(expr, ISyn(foo))
	Let(IAny) OpDecl
}

// IExprVarish is implemented by `IExprContainish`s that also wish
// to offer dot-accessor syntax over the `Addr`, `Set` constructors.
//
// All `ISyn`s among the `Any`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprVarish interface {
	IExprContainish

	// expr.Addr() == Addr(expr)
	Addr() OpAddr
	// expr.Set(foo) == Set(expr, ISyn(foo))
	Set(IAny) OpSet
	// expr.Incr1() == Set(expr, Add(expr, L(1)))
	Incr1() OpSet
	// expr.Decr1() == Set(expr, Sub(expr, L(1)))
	Decr1() OpSet
}

// IExprContainish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `At`, `Sl`, `Deref` constructors.
type IExprContainish interface {
	ISyn

	// expr.At(foo) == At(expr, ISyn(foo))
	At(IAny) OpIdx
	// expr.Deref() == Deref(expr)
	Deref() OpDeref
	// expr.Sl(foo,bar) == At(expr, Sl(Syn(foo), Syn(bar)))
	Sl(IAny, IAny) OpIdx
}

// IExprEqualish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `Eq` and `Neq` constructors.
//
// All `ISyn`s among the `Any`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprEqualish interface {
	ISyn

	// expr.Eq(foo) == Eq(expr, ISyn(foo))
	Eq(IAny) OpEq
	// expr.Neq(foo) == Neq(expr, ISyn(foo))
	Neq(IAny) OpNeq
}

// IExprBoolish is implemented by `IExprEqualish`s that also wish to
// offer dot-accessor syntax over the `And`, `Or`, `Not` constructors.
//
// All `ISyn`s among the `Any`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprBoolish interface {
	IExprEqualish

	// expr.And(foo) == And(expr, ISyn(foo))
	And(IAny) OpAnd
	// expr.Or(foo) == Or(expr, ISyn(foo))
	Or(IAny) OpOr
	// expr.Not() == Not(expr)
	Not() OpNot
}

// IExprOrdish is implemented by `IExprEqualish`s that also wish to offer
// dot-accessor syntax over the `Geq`, `Gt`, `Leq`, `Lt` constructors.
//
// All methods return the appropriate operator types at all times,
// ie. `Geq` always returns an `OpGeq`, `Lt` always an `OpLt` etc.
//
// All `ISyn`s among the `Any`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprOrdish interface {
	IExprEqualish

	// expr.Geq(foo) == Geq(expr, ISyn(foo))
	Geq(IAny) IExprBoolish
	// expr.Leq(foo) == Leq(expr, ISyn(foo))
	Leq(IAny) IExprBoolish
	// expr.Gt(foo) == Gt(expr, ISyn(foo))
	Gt(IAny) IExprBoolish
	// expr.Lt(foo) == Lt(expr, ISyn(foo))
	Lt(IAny) IExprBoolish
}

// IExprOrdish is implemented by `IExprOrdish`s that also wish to offer dot-accessor
// syntax over the `Add`, `Sub`, `Mul`, `Div`, `Mod`, `Neg` constructors.
//
// All `ISyn`s among the `Any`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprNumerish interface {
	IExprOrdish

	// expr.Plus(foo) == Add(expr, ISyn(foo))
	Plus(IAny) OpAdd
	// expr.Minus(foo) == Sub(expr, ISyn(foo))
	Minus(IAny) OpSub
	// expr.Times(foo) == Mul(expr, ISyn(foo))
	Times(IAny) OpMul
	// expr.Div(foo) == Div(expr, ISyn(foo))
	Div(IAny) OpDiv
	// expr.Mod(foo) == Mod(expr, ISyn(foo))
	Mod(IAny) OpMod
	// expr.Neg() == Neg(expr)
	Neg() OpSub
}

// IExprCallish is implemented by `ISyn`s that also wish to
// offer dot-accessor syntax over the `Call` constructor.
//
// All `ISyn`s among the `Any`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprCallish interface {
	ISyn

	// expr.Of(foos...) == Call(expr, Syns(foos)...)
	Of(...IAny) *ExprCall
}

// C implements `IExprDottish`.
func (me Named) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall {
	return Call(D(me, N(dotCalleeName)), SynsFrom(nil, dotCallArgs...)...)
}

// D implements `IExprDottish`.
func (me Named) D(operands ...IAny) OpDot {
	return D(append([]ISyn{me}, synsFrom(true, operands...)...)...)
}

// Set implements `IExprVarish`.
func (me Named) Set(operand IAny) OpSet { return Set(me, synFrom(operand)) }

// Let implements `IExprVarish`.
func (me Named) Let(operand IAny) OpDecl { return Decl(me, synFrom(operand)) }

// Addr implements `IExprVarish`.
func (me Named) Addr() OpAddr { return Addr(me) }

// Deref implements `IExprContainish`.
func (me Named) Deref() OpDeref { return Deref(me) }

// And implements `IExprBoolish`.
func (me Named) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me Named) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (me Named) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me Named) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (me Named) Geq(operand IAny) IExprBoolish { return Geq(me, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (me Named) Gt(operand IAny) IExprBoolish { return Gt(me, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (me Named) Leq(operand IAny) IExprBoolish { return Leq(me, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (me Named) Lt(operand IAny) IExprBoolish { return Lt(me, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (me Named) Plus(operand IAny) OpAdd { return Add(me, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (me Named) Minus(operand IAny) OpSub { return Sub(me, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (me Named) Decr1() OpSet { return Set(me, Sub(me, L(1))) }

// Incr1 implements `IExprVarish`.
func (me Named) Incr1() OpSet { return Set(me, Add(me, L(1))) }

// Times implements `IExprNumerish`.
func (me Named) Times(operand IAny) OpMul { return Mul(me, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (me Named) Div(operand IAny) OpDiv { return Div(me, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (me Named) Mod(operand IAny) OpMod { return Mod(me, synFrom(operand)) }

// At implements `IExprContainish`.
func (me Named) At(operand IAny) OpIdx { return At(me, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (me Named) Sl(startIndex IAny, stopIndex IAny) OpIdx {
	return At(me, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (me Named) Not() OpNot { return Not(me) }

// Neg implements `IExprNumerish`.
func (me Named) Neg() OpSub { return Neg(me) }

// Of implements `IExprCallish`.
func (me Named) Of(args ...IAny) *ExprCall { return Call(me, SynsFrom(nil, args...)...) }

// C implements `IExprDottish`.
func (me *ExprCall) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall {
	return Call(D(me, N(dotCalleeName)), SynsFrom(nil, dotCallArgs...)...)
}

// D implements `IExprDottish`.
func (me *ExprCall) D(operands ...IAny) OpDot {
	return D(append([]ISyn{me}, synsFrom(true, operands...)...)...)
}

// Deref implements `IExprContainish`.
func (me *ExprCall) Deref() OpDeref { return Deref(me) }

// And implements `IExprBoolish`.
func (me *ExprCall) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me *ExprCall) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (me *ExprCall) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me *ExprCall) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (me *ExprCall) Geq(operand IAny) IExprBoolish { return Geq(me, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (me *ExprCall) Gt(operand IAny) IExprBoolish { return Gt(me, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (me *ExprCall) Leq(operand IAny) IExprBoolish { return Leq(me, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (me *ExprCall) Lt(operand IAny) IExprBoolish { return Lt(me, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (me *ExprCall) Plus(operand IAny) OpAdd { return Add(me, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (me *ExprCall) Minus(operand IAny) OpSub { return Sub(me, synFrom(operand)) }

// Times implements `IExprNumerish`.
func (me *ExprCall) Times(operand IAny) OpMul { return Mul(me, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (me *ExprCall) Div(operand IAny) OpDiv { return Div(me, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (me *ExprCall) Mod(operand IAny) OpMod { return Mod(me, synFrom(operand)) }

// At implements `IExprContainish`.
func (me *ExprCall) At(operand IAny) OpIdx { return At(me, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (me *ExprCall) Sl(startIndex IAny, stopIndex IAny) OpIdx {
	return At(me, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (me *ExprCall) Not() OpNot { return Not(me) }

// Neg implements `IExprNumerish`.
func (me *ExprCall) Neg() OpSub { return Neg(me) }

// Of implements `IExprCallish`.
func (me *ExprCall) Of(args ...IAny) *ExprCall { return Call(me, SynsFrom(nil, args...)...) }

// Eq implements `IExprEqualish`.
func (me OpGeq) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpGeq) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// And implements `IExprBoolish`.
func (me OpGeq) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpGeq) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (me OpGeq) Not() OpNot { return Not(me) }

// Eq implements `IExprEqualish`.
func (me OpLeq) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpLeq) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// And implements `IExprBoolish`.
func (me OpLeq) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpLeq) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (me OpLeq) Not() OpNot { return Not(me) }

// Eq implements `IExprEqualish`.
func (me OpGt) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpGt) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// And implements `IExprBoolish`.
func (me OpGt) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpGt) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (me OpGt) Not() OpNot { return Not(me) }

// Eq implements `IExprEqualish`.
func (me OpLt) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpLt) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// And implements `IExprBoolish`.
func (me OpLt) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpLt) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (me OpLt) Not() OpNot { return Not(me) }

// Eq implements `IExprEqualish`.
func (me OpEq) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpEq) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// And implements `IExprBoolish`.
func (me OpEq) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpEq) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (me OpEq) Not() OpNot { return Not(me) }

// Eq implements `IExprEqualish`.
func (me OpNeq) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpNeq) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// And implements `IExprBoolish`.
func (me OpNeq) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpNeq) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (me OpNeq) Not() OpNot { return Not(me) }

// Set implements `IExprVarish`.
func (me OpComma) Set(operand IAny) OpSet { return Set(me, synFrom(operand)) }

// Let implements `IExprVarish`.
func (me OpComma) Let(operand IAny) OpDecl { return Decl(me, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (me OpOr) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpOr) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// And implements `IExprBoolish`.
func (me OpOr) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpOr) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (me OpOr) Not() OpNot { return Not(me) }

// Eq implements `IExprEqualish`.
func (me OpAnd) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpAnd) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// And implements `IExprBoolish`.
func (me OpAnd) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpAnd) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (me OpAnd) Not() OpNot { return Not(me) }

// Eq implements `IExprEqualish`.
func (me OpNot) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpNot) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// And implements `IExprBoolish`.
func (me OpNot) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpNot) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (me OpNot) Not() OpNot { return Not(me) }

// C implements `IExprDottish`.
func (me OpIdx) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall {
	return Call(D(me, N(dotCalleeName)), SynsFrom(nil, dotCallArgs...)...)
}

// D implements `IExprDottish`.
func (me OpIdx) D(operands ...IAny) OpDot {
	return D(append([]ISyn{me}, synsFrom(true, operands...)...)...)
}

// Set implements `IExprVarish`.
func (me OpIdx) Set(operand IAny) OpSet { return Set(me, synFrom(operand)) }

// Addr implements `IExprVarish`.
func (me OpIdx) Addr() OpAddr { return Addr(me) }

// Deref implements `IExprContainish`.
func (me OpIdx) Deref() OpDeref { return Deref(me) }

// And implements `IExprBoolish`.
func (me OpIdx) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpIdx) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (me OpIdx) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpIdx) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (me OpIdx) Geq(operand IAny) IExprBoolish { return Geq(me, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (me OpIdx) Gt(operand IAny) IExprBoolish { return Gt(me, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (me OpIdx) Leq(operand IAny) IExprBoolish { return Leq(me, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (me OpIdx) Lt(operand IAny) IExprBoolish { return Lt(me, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (me OpIdx) Plus(operand IAny) OpAdd { return Add(me, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (me OpIdx) Minus(operand IAny) OpSub { return Sub(me, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (me OpIdx) Decr1() OpSet { return Set(me, Sub(me, L(1))) }

// Incr1 implements `IExprVarish`.
func (me OpIdx) Incr1() OpSet { return Set(me, Add(me, L(1))) }

// Times implements `IExprNumerish`.
func (me OpIdx) Times(operand IAny) OpMul { return Mul(me, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (me OpIdx) Div(operand IAny) OpDiv { return Div(me, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (me OpIdx) Mod(operand IAny) OpMod { return Mod(me, synFrom(operand)) }

// At implements `IExprContainish`.
func (me OpIdx) At(operand IAny) OpIdx { return At(me, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (me OpIdx) Sl(startIndex IAny, stopIndex IAny) OpIdx {
	return At(me, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (me OpIdx) Not() OpNot { return Not(me) }

// Neg implements `IExprNumerish`.
func (me OpIdx) Neg() OpSub { return Neg(me) }

// Of implements `IExprCallish`.
func (me OpIdx) Of(args ...IAny) *ExprCall { return Call(me, SynsFrom(nil, args...)...) }

// C implements `IExprDottish`.
func (me OpDot) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall {
	return Call(D(me, N(dotCalleeName)), SynsFrom(nil, dotCallArgs...)...)
}

// D implements `IExprDottish`.
func (me OpDot) D(operands ...IAny) OpDot {
	return D(append([]ISyn{me}, synsFrom(true, operands...)...)...)
}

// Set implements `IExprVarish`.
func (me OpDot) Set(operand IAny) OpSet { return Set(me, synFrom(operand)) }

// Addr implements `IExprVarish`.
func (me OpDot) Addr() OpAddr { return Addr(me) }

// Deref implements `IExprContainish`.
func (me OpDot) Deref() OpDeref { return Deref(me) }

// And implements `IExprBoolish`.
func (me OpDot) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpDot) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (me OpDot) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpDot) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (me OpDot) Geq(operand IAny) IExprBoolish { return Geq(me, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (me OpDot) Gt(operand IAny) IExprBoolish { return Gt(me, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (me OpDot) Leq(operand IAny) IExprBoolish { return Leq(me, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (me OpDot) Lt(operand IAny) IExprBoolish { return Lt(me, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (me OpDot) Plus(operand IAny) OpAdd { return Add(me, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (me OpDot) Minus(operand IAny) OpSub { return Sub(me, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (me OpDot) Decr1() OpSet { return Set(me, Sub(me, L(1))) }

// Incr1 implements `IExprVarish`.
func (me OpDot) Incr1() OpSet { return Set(me, Add(me, L(1))) }

// Times implements `IExprNumerish`.
func (me OpDot) Times(operand IAny) OpMul { return Mul(me, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (me OpDot) Div(operand IAny) OpDiv { return Div(me, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (me OpDot) Mod(operand IAny) OpMod { return Mod(me, synFrom(operand)) }

// At implements `IExprContainish`.
func (me OpDot) At(operand IAny) OpIdx { return At(me, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (me OpDot) Sl(startIndex IAny, stopIndex IAny) OpIdx {
	return At(me, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (me OpDot) Not() OpNot { return Not(me) }

// Neg implements `IExprNumerish`.
func (me OpDot) Neg() OpSub { return Neg(me) }

// Of implements `IExprCallish`.
func (me OpDot) Of(args ...IAny) *ExprCall { return Call(me, SynsFrom(nil, args...)...) }

// C implements `IExprDottish`.
func (me OpDeref) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall {
	return Call(D(me, N(dotCalleeName)), SynsFrom(nil, dotCallArgs...)...)
}

// D implements `IExprDottish`.
func (me OpDeref) D(operands ...IAny) OpDot {
	return D(append([]ISyn{me}, synsFrom(true, operands...)...)...)
}

// Set implements `IExprVarish`.
func (me OpDeref) Set(operand IAny) OpSet { return Set(me, synFrom(operand)) }

// Deref implements `IExprContainish`.
func (me OpDeref) Deref() OpDeref { return Deref(me) }

// And implements `IExprBoolish`.
func (me OpDeref) And(operand IAny) OpAnd { return And(me, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (me OpDeref) Or(operand IAny) OpOr { return Or(me, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (me OpDeref) Eq(operand IAny) OpEq { return Eq(me, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (me OpDeref) Neq(operand IAny) OpNeq { return Neq(me, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (me OpDeref) Geq(operand IAny) IExprBoolish { return Geq(me, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (me OpDeref) Gt(operand IAny) IExprBoolish { return Gt(me, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (me OpDeref) Leq(operand IAny) IExprBoolish { return Leq(me, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (me OpDeref) Lt(operand IAny) IExprBoolish { return Lt(me, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (me OpDeref) Plus(operand IAny) OpAdd { return Add(me, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (me OpDeref) Minus(operand IAny) OpSub { return Sub(me, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (me OpDeref) Decr1() OpSet { return Set(me, Sub(me, L(1))) }

// Incr1 implements `IExprVarish`.
func (me OpDeref) Incr1() OpSet { return Set(me, Add(me, L(1))) }

// Times implements `IExprNumerish`.
func (me OpDeref) Times(operand IAny) OpMul { return Mul(me, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (me OpDeref) Div(operand IAny) OpDiv { return Div(me, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (me OpDeref) Mod(operand IAny) OpMod { return Mod(me, synFrom(operand)) }

// At implements `IExprContainish`.
func (me OpDeref) At(operand IAny) OpIdx { return At(me, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (me OpDeref) Sl(startIndex IAny, stopIndex IAny) OpIdx {
	return At(me, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (me OpDeref) Not() OpNot { return Not(me) }

// Neg implements `IExprNumerish`.
func (me OpDeref) Neg() OpSub { return Neg(me) }

// Of implements `IExprCallish`.
func (me OpDeref) Of(args ...IAny) *ExprCall { return Call(me, SynsFrom(nil, args...)...) }

func (me OpAdd) Plus(operand IAny) OpAdd {
	return Add(append(me.Operands, synFrom(operand))...)
}
