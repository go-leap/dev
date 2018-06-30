package udevgogen

// IExprAssignish is implemented by `ISyn`s that wish to offer
// dot-accessor syntax over the `Set` and `Decl` constructors.
type IExprAssignish interface {
	ISyn

	SetTo(ISyn) OpSet
	Decl(ISyn) OpDecl
}

// IExprVarish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `I`, `Addr`, `Deref` constructors.
type IExprVarish interface {
	IExprAssignish

	At(ISyn) OpIdx
	Addr() OpAddr
	Deref() OpDeref
}

// IExprEqualish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `Eq` and `Neq` constructors.
type IExprEqualish interface {
	ISyn

	Eq(ISyn) OpEq
	Neq(ISyn) OpNeq
}

// IExprBoolish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `And`, `Or`, `Not` constructors.
type IExprBoolish interface {
	IExprEqualish

	And(ISyn) OpAnd
	Or(ISyn) OpOr
	Not() OpNot
}

// IExprOrdish is implemented by `ISyn`s that also wish to offer
// dot-accessor syntax over the `Geq`, `Gt`, `Leq`, `Lt` constructors.
type IExprOrdish interface {
	IExprEqualish

	Geq(ISyn) OpGeq
	Leq(ISyn) OpLeq
	Gt(ISyn) OpGt
	Lt(ISyn) OpLt
}

// IExprOrdish is implemented by `ISyn`s that also wish to offer dot-accessor
// syntax over the `Add`, `Sub`, `Mul`, `Div`, `Mod`, `Neg` constructors.
type IExprNumerish interface {
	IExprOrdish

	Add(ISyn) OpAdd
	Sub(ISyn) OpSub
	Mul(ISyn) OpMul
	Div(ISyn) OpDiv
	Mod(ISyn) OpMod
	Neg() OpSub
}

// IExprCallish is implemented by `ISyn`s that also wish to
// offer dot-accessor syntax over the `Call` constructor.
type IExprCallish interface {
	ISyn

	Call(...ISyn) *ExprCall
}

// SetTo implements `IExprAssignish`
func (this Named) SetTo(operand ISyn) OpSet { return Set(this, operand) }

// Decl implements `IExprAssignish`
func (this Named) Decl(operand ISyn) OpDecl { return Decl(this, operand) }

// Addr implements `IExprVarish`.
func (this Named) Addr() OpAddr { return Addr(this) }

// Deref implements `IExprVarish`.
func (this Named) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this Named) And(operand ISyn) OpAnd { return And(this, operand) }

// Or implements `IExprBoolish`.
func (this Named) Or(operand ISyn) OpOr { return Or(this, operand) }

// Eq implements `IExprEqualish`.
func (this Named) Eq(operand ISyn) OpEq { return Eq(this, operand) }

// Neq implements `IExprEqualish`.
func (this Named) Neq(operand ISyn) OpNeq { return Neq(this, operand) }

// Geq implements `IExprOrdish`.
func (this Named) Geq(operand ISyn) OpGeq { return Geq(this, operand) }

// Gt implements `IExprOrdish`.
func (this Named) Gt(operand ISyn) OpGt { return Gt(this, operand) }

// Leq implements `IExprOrdish`.
func (this Named) Leq(operand ISyn) OpLeq { return Leq(this, operand) }

// Lt implements `IExprOrdish`.
func (this Named) Lt(operand ISyn) OpLt { return Lt(this, operand) }

// Add implements `IExprNumerish`.
func (this Named) Add(operand ISyn) OpAdd { return Add(this, operand) }

// Sub implements `IExprNumerish`.
func (this Named) Sub(operand ISyn) OpSub { return Sub(this, operand) }

// Mul implements `IExprNumerish`.
func (this Named) Mul(operand ISyn) OpMul { return Mul(this, operand) }

// Div implements `IExprNumerish`.
func (this Named) Div(operand ISyn) OpDiv { return Div(this, operand) }

// Mod implements `IExprNumerish`.
func (this Named) Mod(operand ISyn) OpMod { return Mod(this, operand) }

// At implements `IExprVarish`.
func (this Named) At(operand ISyn) OpIdx { return I(this, operand) }

// Not implements `IExprBoolish`.
func (this Named) Not() OpNot { return Not(this) }
func (this Named) Neg() OpSub { return Neg(this) }

// Call implements `IExprCallish`.
func (this Named) Call(args ...ISyn) *ExprCall { return Call(this, args...) }

// Deref implements `IExprVarish`.
func (this *ExprCall) Deref() OpDeref { return Deref(this) }

// And implements `IExprBoolish`.
func (this *ExprCall) And(operand ISyn) OpAnd { return And(this, operand) }

// Or implements `IExprBoolish`.
func (this *ExprCall) Or(operand ISyn) OpOr { return Or(this, operand) }

// Eq implements `IExprEqualish`.
func (this *ExprCall) Eq(operand ISyn) OpEq { return Eq(this, operand) }

// Neq implements `IExprEqualish`.
func (this *ExprCall) Neq(operand ISyn) OpNeq { return Neq(this, operand) }

// Geq implements `IExprOrdish`.
func (this *ExprCall) Geq(operand ISyn) OpGeq { return Geq(this, operand) }

// Gt implements `IExprOrdish`.
func (this *ExprCall) Gt(operand ISyn) OpGt { return Gt(this, operand) }

// Leq implements `IExprOrdish`.
func (this *ExprCall) Leq(operand ISyn) OpLeq { return Leq(this, operand) }

// Lt implements `IExprOrdish`.
func (this *ExprCall) Lt(operand ISyn) OpLt { return Lt(this, operand) }

// Add implements `IExprNumerish`.
func (this *ExprCall) Add(operand ISyn) OpAdd { return Add(this, operand) }

// Sub implements `IExprNumerish`.
func (this *ExprCall) Sub(operand ISyn) OpSub { return Sub(this, operand) }

// Mul implements `IExprNumerish`.
func (this *ExprCall) Mul(operand ISyn) OpMul { return Mul(this, operand) }

// Div implements `IExprNumerish`.
func (this *ExprCall) Div(operand ISyn) OpDiv { return Div(this, operand) }

// Mod implements `IExprNumerish`.
func (this *ExprCall) Mod(operand ISyn) OpMod { return Mod(this, operand) }

// At implements `IExprVarish`.
func (this *ExprCall) At(operand ISyn) OpIdx { return I(this, operand) }

// Not implements `IExprBoolish`.
func (this *ExprCall) Not() OpNot { return Not(this) }
func (this *ExprCall) Neg() OpSub { return Neg(this) }

// Call implements `IExprCallish`.
func (this *ExprCall) Call(args ...ISyn) *ExprCall { return Call(this, args...) }

// Eq implements `IExprEqualish`.
func (this OpGeq) Eq(operand ISyn) OpEq { return Eq(this, operand) }

// Neq implements `IExprEqualish`.
func (this OpGeq) Neq(operand ISyn) OpNeq { return Neq(this, operand) }

// And implements `IExprBoolish`.
func (this OpGeq) And(operand ISyn) OpAnd { return And(this, operand) }

// Or implements `IExprBoolish`.
func (this OpGeq) Or(operand ISyn) OpOr { return Or(this, operand) }

// Not implements `IExprBoolish`.
func (this OpGeq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpLeq) Eq(operand ISyn) OpEq { return Eq(this, operand) }

// Neq implements `IExprEqualish`.
func (this OpLeq) Neq(operand ISyn) OpNeq { return Neq(this, operand) }

// And implements `IExprBoolish`.
func (this OpLeq) And(operand ISyn) OpAnd { return And(this, operand) }

// Or implements `IExprBoolish`.
func (this OpLeq) Or(operand ISyn) OpOr { return Or(this, operand) }

// Not implements `IExprBoolish`.
func (this OpLeq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpGt) Eq(operand ISyn) OpEq { return Eq(this, operand) }

// Neq implements `IExprEqualish`.
func (this OpGt) Neq(operand ISyn) OpNeq { return Neq(this, operand) }

// And implements `IExprBoolish`.
func (this OpGt) And(operand ISyn) OpAnd { return And(this, operand) }

// Or implements `IExprBoolish`.
func (this OpGt) Or(operand ISyn) OpOr { return Or(this, operand) }

// Not implements `IExprBoolish`.
func (this OpGt) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpLt) Eq(operand ISyn) OpEq { return Eq(this, operand) }

// Neq implements `IExprEqualish`.
func (this OpLt) Neq(operand ISyn) OpNeq { return Neq(this, operand) }

// And implements `IExprBoolish`.
func (this OpLt) And(operand ISyn) OpAnd { return And(this, operand) }

// Or implements `IExprBoolish`.
func (this OpLt) Or(operand ISyn) OpOr { return Or(this, operand) }

// Not implements `IExprBoolish`.
func (this OpLt) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpEq) Eq(operand ISyn) OpEq { return Eq(this, operand) }

// Neq implements `IExprEqualish`.
func (this OpEq) Neq(operand ISyn) OpNeq { return Neq(this, operand) }

// And implements `IExprBoolish`.
func (this OpEq) And(operand ISyn) OpAnd { return And(this, operand) }

// Or implements `IExprBoolish`.
func (this OpEq) Or(operand ISyn) OpOr { return Or(this, operand) }

// Not implements `IExprBoolish`.
func (this OpEq) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpNeq) Eq(operand ISyn) OpEq { return Eq(this, operand) }

// Neq implements `IExprEqualish`.
func (this OpNeq) Neq(operand ISyn) OpNeq { return Neq(this, operand) }

// And implements `IExprBoolish`.
func (this OpNeq) And(operand ISyn) OpAnd { return And(this, operand) }

// Or implements `IExprBoolish`.
func (this OpNeq) Or(operand ISyn) OpOr { return Or(this, operand) }

// Not implements `IExprBoolish`.
func (this OpNeq) Not() OpNot { return Not(this) }

// SetTo implements `IExprAssignish`
func (this OpComma) SetTo(operand ISyn) OpSet { return Set(this, operand) }

// Decl implements `IExprAssignish`
func (this OpComma) Decl(operand ISyn) OpDecl { return Decl(this, operand) }

// Eq implements `IExprEqualish`.
func (this OpOr) Eq(operand ISyn) OpEq { return Eq(this, operand) }

// Neq implements `IExprEqualish`.
func (this OpOr) Neq(operand ISyn) OpNeq { return Neq(this, operand) }

// And implements `IExprBoolish`.
func (this OpOr) And(operand ISyn) OpAnd { return And(this, operand) }

// Or implements `IExprBoolish`.
func (this OpOr) Or(operand ISyn) OpOr { return Or(this, operand) }

// Not implements `IExprBoolish`.
func (this OpOr) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpAnd) Eq(operand ISyn) OpEq { return Eq(this, operand) }

// Neq implements `IExprEqualish`.
func (this OpAnd) Neq(operand ISyn) OpNeq { return Neq(this, operand) }

// And implements `IExprBoolish`.
func (this OpAnd) And(operand ISyn) OpAnd { return And(this, operand) }

// Or implements `IExprBoolish`.
func (this OpAnd) Or(operand ISyn) OpOr { return Or(this, operand) }

// Not implements `IExprBoolish`.
func (this OpAnd) Not() OpNot { return Not(this) }

// Eq implements `IExprEqualish`.
func (this OpNot) Eq(operand ISyn) OpEq { return Eq(this, operand) }

// Neq implements `IExprEqualish`.
func (this OpNot) Neq(operand ISyn) OpNeq { return Neq(this, operand) }

// And implements `IExprBoolish`.
func (this OpNot) And(operand ISyn) OpAnd { return And(this, operand) }

// Or implements `IExprBoolish`.
func (this OpNot) Or(operand ISyn) OpOr { return Or(this, operand) }

// Not implements `IExprBoolish`.
func (this OpNot) Not() OpNot { return Not(this) }
