package udevgogen

// IExprVarish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `Addr`, `Set`, `Decl` constructors.
//
// All `ISyn`s among the `Any`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprVarish interface {
	IExprContainish

	// expr.Addr() == Addr(expr)
	Addr() OpAddr
	// expr.Set(foo) == Set(expr, ISyn(foo))
	Set(Any) OpSet
	// expr.Let(foo) == Decl(expr, ISyn(foo))
	Let(Any) OpDecl
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
	At(Any) OpIdx
	// expr.Deref() == Deref(expr)
	Deref() OpDeref
	// expr.Sl(foo,bar) == At(expr, Sl(Syn(foo), Syn(bar)))
	Sl(Any, Any) OpIdx
}

// IExprEqualish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `Eq` and `Neq` constructors.
//
// All `ISyn`s among the `Any`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprEqualish interface {
	ISyn

	// expr.Eq(foo) == Eq(expr, ISyn(foo))
	Eq(Any) OpEq
	// expr.Neq(foo) == Neq(expr, ISyn(foo))
	Neq(Any) OpNeq
}

// IExprBoolish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `And`, `Or`, `Not` constructors.
//
// All `ISyn`s among the `Any`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprBoolish interface {
	IExprEqualish

	// expr.And(foo) == And(expr, ISyn(foo))
	And(Any) OpAnd
	// expr.Or(foo) == Or(expr, ISyn(foo))
	Or(Any) OpOr
	// expr.Not() == Not(expr)
	Not() OpNot
}

// IExprOrdish is implemented by `ISyn`s that also wish to offer
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
	Geq(Any) IExprBoolish
	// expr.Leq(foo) == Leq(expr, ISyn(foo))
	Leq(Any) IExprBoolish
	// expr.Gt(foo) == Gt(expr, ISyn(foo))
	Gt(Any) IExprBoolish
	// expr.Lt(foo) == Lt(expr, ISyn(foo))
	Lt(Any) IExprBoolish
}

// IExprOrdish is implemented by `ISyn`s that also wish to offer dot-accessor
// syntax over the `Add`, `Sub`, `Mul`, `Div`, `Mod`, `Neg` constructors.
//
// All `ISyn`s among the `Any`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprNumerish interface {
	IExprOrdish

	// expr.Plus(foo) == Add(expr, ISyn(foo))
	Plus(Any) OpAdd
	// expr.Minus(foo) == Sub(expr, ISyn(foo))
	Minus(Any) OpSub
	// expr.Times(foo) == Mul(expr, ISyn(foo))
	Times(Any) OpMul
	// expr.Div(foo) == Div(expr, ISyn(foo))
	Div(Any) OpDiv
	// expr.Mod(foo) == Mod(expr, ISyn(foo))
	Mod(Any) OpMod
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
	Of(...Any) *ExprCall
}

// Set implements `IExprVar`.
func (this Named) Set(operand Any) OpSet { return Set(this, synFrom(operand)) }

// Let implements `IExprVar`.
func (this Named) Let(operand Any) OpDecl { return Decl(this, synFrom(operand)) }

// Addr implements `IExprVarish`.
func (this Named) Addr() OpAddr { return Addr(this) }

// Deref implements `IExprContainish`.
func (this Named) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this Named) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this Named) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this Named) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this Named) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (this Named) Geq(operand Any) IExprBoolish { return Geq(this, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (this Named) Gt(operand Any) IExprBoolish { return Gt(this, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (this Named) Leq(operand Any) IExprBoolish { return Leq(this, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (this Named) Lt(operand Any) IExprBoolish { return Lt(this, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (this Named) Plus(operand Any) OpAdd { return Add(this, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (this Named) Minus(operand Any) OpSub { return Sub(this, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (this Named) Decr1() OpSet { return Set(this, Sub(this, L(1))) }

// Incr1 implements `IExprVarish`.
func (this Named) Incr1() OpSet { return Set(this, Add(this, L(1))) }

// Times implements `IExprNumerish`.
func (this Named) Times(operand Any) OpMul { return Mul(this, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (this Named) Div(operand Any) OpDiv { return Div(this, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (this Named) Mod(operand Any) OpMod { return Mod(this, synFrom(operand)) }

// At implements `IExprContainish`.
func (this Named) At(operand Any) OpIdx { return At(this, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (this Named) Sl(startIndex Any, stopIndex Any) OpIdx {
	return At(this, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (this Named) Not() OpNot { return Not(this) }

// Neg implements `IExprNumerish`.
func (this Named) Neg() OpSub { return Neg(this) }

// Of implements `IExprCallish`.
func (this Named) Of(args ...Any) *ExprCall { return Call(this, synsFrom(nil, args...)...) }

// Deref implements `IExprContainish`.
func (this *ExprCall) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this *ExprCall) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this *ExprCall) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this *ExprCall) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this *ExprCall) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (this *ExprCall) Geq(operand Any) IExprBoolish { return Geq(this, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (this *ExprCall) Gt(operand Any) IExprBoolish { return Gt(this, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (this *ExprCall) Leq(operand Any) IExprBoolish { return Leq(this, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (this *ExprCall) Lt(operand Any) IExprBoolish { return Lt(this, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (this *ExprCall) Plus(operand Any) OpAdd { return Add(this, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (this *ExprCall) Minus(operand Any) OpSub { return Sub(this, synFrom(operand)) }

// Times implements `IExprNumerish`.
func (this *ExprCall) Times(operand Any) OpMul { return Mul(this, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (this *ExprCall) Div(operand Any) OpDiv { return Div(this, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (this *ExprCall) Mod(operand Any) OpMod { return Mod(this, synFrom(operand)) }

// At implements `IExprContainish`.
func (this *ExprCall) At(operand Any) OpIdx { return At(this, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (this *ExprCall) Sl(startIndex Any, stopIndex Any) OpIdx {
	return At(this, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (this *ExprCall) Not() OpNot { return Not(this) }

// Neg implements `IExprNumerish`.
func (this *ExprCall) Neg() OpSub { return Neg(this) }

// Of implements `IExprCallish`.
func (this *ExprCall) Of(args ...Any) *ExprCall { return Call(this, synsFrom(nil, args...)...) }

// Eq implements `IExprEqualish`.
func (this OpGeq) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpGeq) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpGeq) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpGeq) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpGeq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpLeq) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpLeq) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpLeq) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpLeq) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpLeq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpGt) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpGt) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpGt) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpGt) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpGt) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpLt) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpLt) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpLt) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpLt) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpLt) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpEq) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpEq) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpEq) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpEq) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpEq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpNeq) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpNeq) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpNeq) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpNeq) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpNeq) Not() OpNot { return Not(this) }

// Set implements `IExprVar`.
func (this OpComma) Set(operand Any) OpSet { return Set(this, synFrom(operand)) }

// Let implements `IExprVar`.
func (this OpComma) Let(operand Any) OpDecl { return Decl(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this OpOr) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpOr) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpOr) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpOr) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpOr) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpAnd) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpAnd) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpAnd) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpAnd) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpAnd) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpNot) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpNot) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpNot) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpNot) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpNot) Not() OpNot { return Not(this) }

// Set implements `IExprVar`.
func (this OpIdx) Set(operand Any) OpSet { return Set(this, synFrom(operand)) }

// Addr implements `IExprVarish`.
func (this OpIdx) Addr() OpAddr { return Addr(this) }

// Deref implements `IExprContainish`.
func (this OpIdx) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this OpIdx) And(operand Any) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpIdx) Or(operand Any) OpOr { return Or(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this OpIdx) Eq(operand Any) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpIdx) Neq(operand Any) OpNeq { return Neq(this, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (this OpIdx) Geq(operand Any) IExprBoolish { return Geq(this, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (this OpIdx) Gt(operand Any) IExprBoolish { return Gt(this, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (this OpIdx) Leq(operand Any) IExprBoolish { return Leq(this, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (this OpIdx) Lt(operand Any) IExprBoolish { return Lt(this, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (this OpIdx) Plus(operand Any) OpAdd { return Add(this, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (this OpIdx) Minus(operand Any) OpSub { return Sub(this, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (this OpIdx) Decr1() OpSet { return Set(this, Sub(this, L(1))) }

// Incr1 implements `IExprVarish`.
func (this OpIdx) Incr1() OpSet { return Set(this, Add(this, L(1))) }

// Times implements `IExprNumerish`.
func (this OpIdx) Times(operand Any) OpMul { return Mul(this, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (this OpIdx) Div(operand Any) OpDiv { return Div(this, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (this OpIdx) Mod(operand Any) OpMod { return Mod(this, synFrom(operand)) }

// At implements `IExprContainish`.
func (this OpIdx) At(operand Any) OpIdx { return At(this, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (this OpIdx) Sl(startIndex Any, stopIndex Any) OpIdx {
	return At(this, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (this OpIdx) Not() OpNot { return Not(this) }

// Neg implements `IExprNumerish`.
func (this OpIdx) Neg() OpSub { return Neg(this) }

// Of implements `IExprCallish`.
func (this OpIdx) Of(args ...Any) *ExprCall { return Call(this, synsFrom(nil, args...)...) }
