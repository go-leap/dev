package udevgogen

// IExprDottish is implemented by `ISyn`s that also wish
// to offer dot-accessor syntax over the `D` constructor.
type IExprDottish interface {
	ISyn

	C(string, ...IAny) *ExprCall
	D() OpDot
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
func (this Named) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall {
	return Call(D(this, N(dotCalleeName)), synsFrom(nil, dotCallArgs...)...)
}

// D implements `IExprDottish`.
func (this Named) D(operands ...IAny) OpDot {
	return D(append([]ISyn{this}, synsFrom(nil, operands...)...)...)
}

// Set implements `IExprVar`.
func (this Named) Set(operand IAny) OpSet { return Set(this, synFrom(operand)) }

// Let implements `IExprVar`.
func (this Named) Let(operand IAny) OpDecl { return Decl(this, synFrom(operand)) }

// Addr implements `IExprVarish`.
func (this Named) Addr() OpAddr { return Addr(this) }

// Deref implements `IExprContainish`.
func (this Named) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this Named) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this Named) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this Named) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this Named) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (this Named) Geq(operand IAny) IExprBoolish { return Geq(this, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (this Named) Gt(operand IAny) IExprBoolish { return Gt(this, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (this Named) Leq(operand IAny) IExprBoolish { return Leq(this, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (this Named) Lt(operand IAny) IExprBoolish { return Lt(this, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (this Named) Plus(operand IAny) OpAdd { return Add(this, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (this Named) Minus(operand IAny) OpSub { return Sub(this, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (this Named) Decr1() OpSet { return Set(this, Sub(this, L(1))) }

// Incr1 implements `IExprVarish`.
func (this Named) Incr1() OpSet { return Set(this, Add(this, L(1))) }

// Times implements `IExprNumerish`.
func (this Named) Times(operand IAny) OpMul { return Mul(this, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (this Named) Div(operand IAny) OpDiv { return Div(this, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (this Named) Mod(operand IAny) OpMod { return Mod(this, synFrom(operand)) }

// At implements `IExprContainish`.
func (this Named) At(operand IAny) OpIdx { return At(this, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (this Named) Sl(startIndex IAny, stopIndex IAny) OpIdx {
	return At(this, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (this Named) Not() OpNot { return Not(this) }

// Neg implements `IExprNumerish`.
func (this Named) Neg() OpSub { return Neg(this) }

// Of implements `IExprCallish`.
func (this Named) Of(args ...IAny) *ExprCall { return Call(this, synsFrom(nil, args...)...) }

// C implements `IExprDottish`.
func (this *ExprCall) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall {
	return Call(D(this, N(dotCalleeName)), synsFrom(nil, dotCallArgs...)...)
}

// D implements `IExprDottish`.
func (this *ExprCall) D(operands ...IAny) OpDot {
	return D(append([]ISyn{this}, synsFrom(nil, operands...)...)...)
}

// Deref implements `IExprContainish`.
func (this *ExprCall) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this *ExprCall) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this *ExprCall) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this *ExprCall) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this *ExprCall) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (this *ExprCall) Geq(operand IAny) IExprBoolish { return Geq(this, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (this *ExprCall) Gt(operand IAny) IExprBoolish { return Gt(this, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (this *ExprCall) Leq(operand IAny) IExprBoolish { return Leq(this, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (this *ExprCall) Lt(operand IAny) IExprBoolish { return Lt(this, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (this *ExprCall) Plus(operand IAny) OpAdd { return Add(this, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (this *ExprCall) Minus(operand IAny) OpSub { return Sub(this, synFrom(operand)) }

// Times implements `IExprNumerish`.
func (this *ExprCall) Times(operand IAny) OpMul { return Mul(this, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (this *ExprCall) Div(operand IAny) OpDiv { return Div(this, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (this *ExprCall) Mod(operand IAny) OpMod { return Mod(this, synFrom(operand)) }

// At implements `IExprContainish`.
func (this *ExprCall) At(operand IAny) OpIdx { return At(this, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (this *ExprCall) Sl(startIndex IAny, stopIndex IAny) OpIdx {
	return At(this, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (this *ExprCall) Not() OpNot { return Not(this) }

// Neg implements `IExprNumerish`.
func (this *ExprCall) Neg() OpSub { return Neg(this) }

// Of implements `IExprCallish`.
func (this *ExprCall) Of(args ...IAny) *ExprCall { return Call(this, synsFrom(nil, args...)...) }

// Eq implements `IExprEqualish`.
func (this OpGeq) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpGeq) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpGeq) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpGeq) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpGeq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpLeq) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpLeq) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpLeq) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpLeq) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpLeq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpGt) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpGt) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpGt) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpGt) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpGt) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpLt) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpLt) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpLt) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpLt) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpLt) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpEq) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpEq) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpEq) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpEq) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpEq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpNeq) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpNeq) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpNeq) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpNeq) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpNeq) Not() OpNot { return Not(this) }

// Set implements `IExprVar`.
func (this OpComma) Set(operand IAny) OpSet { return Set(this, synFrom(operand)) }

// Let implements `IExprVar`.
func (this OpComma) Let(operand IAny) OpDecl { return Decl(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this OpOr) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpOr) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpOr) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpOr) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpOr) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpAnd) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpAnd) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpAnd) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpAnd) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpAnd) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpNot) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpNot) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpNot) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpNot) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpNot) Not() OpNot { return Not(this) }

// C implements `IExprDottish`.
func (this OpIdx) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall {
	return Call(D(this, N(dotCalleeName)), synsFrom(nil, dotCallArgs...)...)
}

// D implements `IExprDottish`.
func (this OpIdx) D(operands ...IAny) OpDot {
	return D(append([]ISyn{this}, synsFrom(nil, operands...)...)...)
}

// Set implements `IExprVar`.
func (this OpIdx) Set(operand IAny) OpSet { return Set(this, synFrom(operand)) }

// Addr implements `IExprVarish`.
func (this OpIdx) Addr() OpAddr { return Addr(this) }

// Deref implements `IExprContainish`.
func (this OpIdx) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this OpIdx) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpIdx) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this OpIdx) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpIdx) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (this OpIdx) Geq(operand IAny) IExprBoolish { return Geq(this, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (this OpIdx) Gt(operand IAny) IExprBoolish { return Gt(this, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (this OpIdx) Leq(operand IAny) IExprBoolish { return Leq(this, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (this OpIdx) Lt(operand IAny) IExprBoolish { return Lt(this, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (this OpIdx) Plus(operand IAny) OpAdd { return Add(this, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (this OpIdx) Minus(operand IAny) OpSub { return Sub(this, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (this OpIdx) Decr1() OpSet { return Set(this, Sub(this, L(1))) }

// Incr1 implements `IExprVarish`.
func (this OpIdx) Incr1() OpSet { return Set(this, Add(this, L(1))) }

// Times implements `IExprNumerish`.
func (this OpIdx) Times(operand IAny) OpMul { return Mul(this, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (this OpIdx) Div(operand IAny) OpDiv { return Div(this, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (this OpIdx) Mod(operand IAny) OpMod { return Mod(this, synFrom(operand)) }

// At implements `IExprContainish`.
func (this OpIdx) At(operand IAny) OpIdx { return At(this, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (this OpIdx) Sl(startIndex IAny, stopIndex IAny) OpIdx {
	return At(this, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (this OpIdx) Not() OpNot { return Not(this) }

// Neg implements `IExprNumerish`.
func (this OpIdx) Neg() OpSub { return Neg(this) }

// Of implements `IExprCallish`.
func (this OpIdx) Of(args ...IAny) *ExprCall { return Call(this, synsFrom(nil, args...)...) }

// C implements `IExprDottish`.
func (this OpDot) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall {
	return Call(D(this, N(dotCalleeName)), synsFrom(nil, dotCallArgs...)...)
}

// D implements `IExprDottish`.
func (this OpDot) D(operands ...IAny) OpDot {
	return D(append([]ISyn{this}, synsFrom(nil, operands...)...)...)
}

// Set implements `IExprVar`.
func (this OpDot) Set(operand IAny) OpSet { return Set(this, synFrom(operand)) }

// Addr implements `IExprVarish`.
func (this OpDot) Addr() OpAddr { return Addr(this) }

// Deref implements `IExprContainish`.
func (this OpDot) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this OpDot) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpDot) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this OpDot) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpDot) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (this OpDot) Geq(operand IAny) IExprBoolish { return Geq(this, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (this OpDot) Gt(operand IAny) IExprBoolish { return Gt(this, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (this OpDot) Leq(operand IAny) IExprBoolish { return Leq(this, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (this OpDot) Lt(operand IAny) IExprBoolish { return Lt(this, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (this OpDot) Plus(operand IAny) OpAdd { return Add(this, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (this OpDot) Minus(operand IAny) OpSub { return Sub(this, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (this OpDot) Decr1() OpSet { return Set(this, Sub(this, L(1))) }

// Incr1 implements `IExprVarish`.
func (this OpDot) Incr1() OpSet { return Set(this, Add(this, L(1))) }

// Times implements `IExprNumerish`.
func (this OpDot) Times(operand IAny) OpMul { return Mul(this, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (this OpDot) Div(operand IAny) OpDiv { return Div(this, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (this OpDot) Mod(operand IAny) OpMod { return Mod(this, synFrom(operand)) }

// At implements `IExprContainish`.
func (this OpDot) At(operand IAny) OpIdx { return At(this, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (this OpDot) Sl(startIndex IAny, stopIndex IAny) OpIdx {
	return At(this, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (this OpDot) Not() OpNot { return Not(this) }

// Neg implements `IExprNumerish`.
func (this OpDot) Neg() OpSub { return Neg(this) }

// Of implements `IExprCallish`.
func (this OpDot) Of(args ...IAny) *ExprCall { return Call(this, synsFrom(nil, args...)...) }

// C implements `IExprDottish`.
func (this OpDeref) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall {
	return Call(D(this, N(dotCalleeName)), synsFrom(nil, dotCallArgs...)...)
}

// D implements `IExprDottish`.
func (this OpDeref) D(operands ...IAny) OpDot {
	return D(append([]ISyn{this}, synsFrom(nil, operands...)...)...)
}

// Set implements `IExprVar`.
func (this OpDeref) Set(operand IAny) OpSet { return Set(this, synFrom(operand)) }

// Deref implements `IExprContainish`.
func (this OpDeref) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this OpDeref) And(operand IAny) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpDeref) Or(operand IAny) OpOr { return Or(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this OpDeref) Eq(operand IAny) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpDeref) Neq(operand IAny) OpNeq { return Neq(this, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (this OpDeref) Geq(operand IAny) IExprBoolish { return Geq(this, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (this OpDeref) Gt(operand IAny) IExprBoolish { return Gt(this, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (this OpDeref) Leq(operand IAny) IExprBoolish { return Leq(this, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (this OpDeref) Lt(operand IAny) IExprBoolish { return Lt(this, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (this OpDeref) Plus(operand IAny) OpAdd { return Add(this, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (this OpDeref) Minus(operand IAny) OpSub { return Sub(this, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (this OpDeref) Decr1() OpSet { return Set(this, Sub(this, L(1))) }

// Incr1 implements `IExprVarish`.
func (this OpDeref) Incr1() OpSet { return Set(this, Add(this, L(1))) }

// Times implements `IExprNumerish`.
func (this OpDeref) Times(operand IAny) OpMul { return Mul(this, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (this OpDeref) Div(operand IAny) OpDiv { return Div(this, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (this OpDeref) Mod(operand IAny) OpMod { return Mod(this, synFrom(operand)) }

// At implements `IExprContainish`.
func (this OpDeref) At(operand IAny) OpIdx { return At(this, synFrom(operand)) }

// Sl implements `IExprContainish`.
func (this OpDeref) Sl(startIndex IAny, stopIndex IAny) OpIdx {
	return At(this, Sl(synFrom(startIndex), synFrom(stopIndex)))
}

// Not implements `IExprBoolish`.
func (this OpDeref) Not() OpNot { return Not(this) }

// Neg implements `IExprNumerish`.
func (this OpDeref) Neg() OpSub { return Neg(this) }

// Of implements `IExprCallish`.
func (this OpDeref) Of(args ...IAny) *ExprCall { return Call(this, synsFrom(nil, args...)...) }
