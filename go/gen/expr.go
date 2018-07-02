package udevgogen

// IExprVarish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `Addr`, `Set`, `Decl` constructors.
//
// All `ISyn`s among the `interface{}`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprVarish interface {
	IExprContainish

	Addr() OpAddr
	Set(interface{}) OpSet
	Let(interface{}) OpDecl
	Incr1() OpSet
	Decr1() OpSet
}

// IExprContainish is implemented by `ISyn`s that also wish to
// offer dot-accessor syntax over the `I` and `Deref` constructors.
type IExprContainish interface {
	ISyn

	At(interface{}) OpIdx
	Deref() OpDeref
}

// IExprEqualish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `Eq` and `Neq` constructors.
//
// All `ISyn`s among the `interface{}`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprEqualish interface {
	ISyn

	Eq(interface{}) OpEq
	Neq(interface{}) OpNeq
}

// IExprBoolish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `And`, `Or`, `Not` constructors.
//
// All `ISyn`s among the `interface{}`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprBoolish interface {
	IExprEqualish

	And(interface{}) OpAnd
	Or(interface{}) OpOr
	Not() OpNot
}

// IExprOrdish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `Geq`, `Gt`, `Leq`, `Lt` constructors.
//
// All methods return the appropriate operator types at all times,
// ie. `Geq` always returns an `OpGeq`, `Lt` always an `OpLt` etc.
//
// All `ISyn`s among the `interface{}`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprOrdish interface {
	IExprEqualish

	Geq(interface{}) IExprBoolish
	Leq(interface{}) IExprBoolish
	Gt(interface{}) IExprBoolish
	Lt(interface{}) IExprBoolish
}

// IExprOrdish is implemented by `ISyn`s that also wish to offer dot-accessor
// syntax over the `Add`, `Sub`, `Mul`, `Div`, `Mod`, `Neg` constructors.
//
// All `ISyn`s among the `interface{}`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprNumerish interface {
	IExprOrdish

	Plus(interface{}) OpAdd
	Minus(interface{}) OpSub
	Times(interface{}) OpMul
	Div(interface{}) OpDiv
	Mod(interface{}) OpMod
	Neg() OpSub
}

// IExprCallish is implemented by `ISyn`s that also wish to
// offer dot-accessor syntax over the `Call` constructor.
//
// All `ISyn`s among the `interface{}`-typed operand arguments
// are used directly, any others are converted into `ExprLit`s.
type IExprCallish interface {
	ISyn

	Call(...interface{}) *ExprCall
}

// Set implements `IExprVar`.
func (this Named) Set(operand interface{}) OpSet { return Set(this, synFrom(operand)) }

// Let implements `IExprVar`.
func (this Named) Let(operand interface{}) OpDecl { return Decl(this, synFrom(operand)) }

// Addr implements `IExprVarish`.
func (this Named) Addr() OpAddr { return Addr(this) }

// Deref implements `IExprContainish`.
func (this Named) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this Named) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this Named) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this Named) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this Named) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (this Named) Geq(operand interface{}) IExprBoolish { return Geq(this, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (this Named) Gt(operand interface{}) IExprBoolish { return Gt(this, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (this Named) Leq(operand interface{}) IExprBoolish { return Leq(this, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (this Named) Lt(operand interface{}) IExprBoolish { return Lt(this, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (this Named) Plus(operand interface{}) OpAdd { return Add(this, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (this Named) Minus(operand interface{}) OpSub { return Sub(this, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (this Named) Decr1() OpSet { return Set(this, Sub(this, L(1))) }

// Incr1 implements `IExprVarish`.
func (this Named) Incr1() OpSet { return Set(this, Add(this, L(1))) }

// Times implements `IExprNumerish`.
func (this Named) Times(operand interface{}) OpMul { return Mul(this, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (this Named) Div(operand interface{}) OpDiv { return Div(this, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (this Named) Mod(operand interface{}) OpMod { return Mod(this, synFrom(operand)) }

// At implements `IExprContainish`.
func (this Named) At(operand interface{}) OpIdx { return I(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this Named) Not() OpNot { return Not(this) }

// Neg implements `IExprNumerish`.
func (this Named) Neg() OpSub { return Neg(this) }

// Call implements `IExprCallish`.
func (this Named) Call(args ...interface{}) *ExprCall { return Call(this, synsFrom(nil, args...)...) }

// Deref implements `IExprContainish`.
func (this *ExprCall) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this *ExprCall) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this *ExprCall) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this *ExprCall) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this *ExprCall) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (this *ExprCall) Geq(operand interface{}) IExprBoolish { return Geq(this, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (this *ExprCall) Gt(operand interface{}) IExprBoolish { return Gt(this, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (this *ExprCall) Leq(operand interface{}) IExprBoolish { return Leq(this, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (this *ExprCall) Lt(operand interface{}) IExprBoolish { return Lt(this, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (this *ExprCall) Plus(operand interface{}) OpAdd { return Add(this, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (this *ExprCall) Minus(operand interface{}) OpSub { return Sub(this, synFrom(operand)) }

// Times implements `IExprNumerish`.
func (this *ExprCall) Times(operand interface{}) OpMul { return Mul(this, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (this *ExprCall) Div(operand interface{}) OpDiv { return Div(this, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (this *ExprCall) Mod(operand interface{}) OpMod { return Mod(this, synFrom(operand)) }

// At implements `IExprContainish`.
func (this *ExprCall) At(operand interface{}) OpIdx { return I(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this *ExprCall) Not() OpNot { return Not(this) }

// Neg implements `IExprNumerish`.
func (this *ExprCall) Neg() OpSub { return Neg(this) }

// Call implements `IExprCallish`.
func (this *ExprCall) Call(args ...interface{}) *ExprCall {
	return Call(this, synsFrom(nil, args...)...)
}

// Eq implements `IExprEqualish`.
func (this OpGeq) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpGeq) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpGeq) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpGeq) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpGeq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpLeq) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpLeq) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpLeq) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpLeq) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpLeq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpGt) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpGt) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpGt) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpGt) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpGt) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpLt) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpLt) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpLt) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpLt) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpLt) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpEq) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpEq) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpEq) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpEq) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpEq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpNeq) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpNeq) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpNeq) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpNeq) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpNeq) Not() OpNot { return Not(this) }

// Set implements `IExprVar`.
func (this OpComma) Set(operand interface{}) OpSet { return Set(this, synFrom(operand)) }

// Let implements `IExprVar`.
func (this OpComma) Let(operand interface{}) OpDecl { return Decl(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this OpOr) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpOr) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpOr) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpOr) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpOr) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpAnd) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpAnd) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpAnd) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpAnd) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpAnd) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpNot) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpNot) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// And implements `IExprBoolish`.
func (this OpNot) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpNot) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpNot) Not() OpNot { return Not(this) }

// Set implements `IExprVar`.
func (this OpIdx) Set(operand interface{}) OpSet { return Set(this, synFrom(operand)) }

// Addr implements `IExprVarish`.
func (this OpIdx) Addr() OpAddr { return Addr(this) }

// Deref implements `IExprContainish`.
func (this OpIdx) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this OpIdx) And(operand interface{}) OpAnd { return And(this, synFrom(operand)) }

// Or implements `IExprBoolish`.
func (this OpIdx) Or(operand interface{}) OpOr { return Or(this, synFrom(operand)) }

// Eq implements `IExprEqualish`.
func (this OpIdx) Eq(operand interface{}) OpEq { return Eq(this, synFrom(operand)) }

// Neq implements `IExprEqualish`.
func (this OpIdx) Neq(operand interface{}) OpNeq { return Neq(this, synFrom(operand)) }

// Geq implements `IExprOrdish`.
func (this OpIdx) Geq(operand interface{}) IExprBoolish { return Geq(this, synFrom(operand)) }

// Gt implements `IExprOrdish`.
func (this OpIdx) Gt(operand interface{}) IExprBoolish { return Gt(this, synFrom(operand)) }

// Leq implements `IExprOrdish`.
func (this OpIdx) Leq(operand interface{}) IExprBoolish { return Leq(this, synFrom(operand)) }

// Lt implements `IExprOrdish`.
func (this OpIdx) Lt(operand interface{}) IExprBoolish { return Lt(this, synFrom(operand)) }

// Plus implements `IExprNumerish`.
func (this OpIdx) Plus(operand interface{}) OpAdd { return Add(this, synFrom(operand)) }

// Minus implements `IExprNumerish`.
func (this OpIdx) Minus(operand interface{}) OpSub { return Sub(this, synFrom(operand)) }

// Decr1 implements `IExprVarish`.
func (this OpIdx) Decr1() OpSet { return Set(this, Sub(this, L(1))) }

// Incr1 implements `IExprVarish`.
func (this OpIdx) Incr1() OpSet { return Set(this, Add(this, L(1))) }

// Times implements `IExprNumerish`.
func (this OpIdx) Times(operand interface{}) OpMul { return Mul(this, synFrom(operand)) }

// Div implements `IExprNumerish`.
func (this OpIdx) Div(operand interface{}) OpDiv { return Div(this, synFrom(operand)) }

// Mod implements `IExprNumerish`.
func (this OpIdx) Mod(operand interface{}) OpMod { return Mod(this, synFrom(operand)) }

// At implements `IExprContainish`.
func (this OpIdx) At(operand interface{}) OpIdx { return I(this, synFrom(operand)) }

// Not implements `IExprBoolish`.
func (this OpIdx) Not() OpNot { return Not(this) }

// Neg implements `IExprNumerish`.
func (this OpIdx) Neg() OpSub { return Neg(this) }

// Call implements `IExprCallish`.
func (this OpIdx) Call(args ...interface{}) *ExprCall { return Call(this, synsFrom(nil, args...)...) }
