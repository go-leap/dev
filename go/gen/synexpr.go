package udevgogen

type IExpr interface {
	ISyn
	Geq(ISyn) OpGeq
	Leq(ISyn) OpLeq
	Gt(ISyn) OpGt
	Lt(ISyn) OpLt
	Set(ISyn) OpSet
	Decl(ISyn) OpDecl
	And(ISyn) OpAnd
	Or(ISyn) OpOr
	Eq(ISyn) OpEq
	Neq(ISyn) OpNeq
	Add(ISyn) OpAdd
	Sub(ISyn) OpSub
	Mul(ISyn) OpMul
	Div(ISyn) OpDiv
	Idx(ISyn) OpIdx
	Addr() OpAddr
	Deref() OpDeref
	Not() OpNot
	Neg() OpSub
}

func (this Named) SetTo(operand ISyn) OpSet { return Set(this, operand) }
func (this Named) Decl(operand ISyn) OpDecl { return Decl(this, operand) }
func (this Named) And(operand ISyn) OpAnd   { return And(this, operand) }
func (this Named) Or(operand ISyn) OpOr     { return Or(this, operand) }
func (this Named) Eq(operand ISyn) OpEq     { return Eq(this, operand) }
func (this Named) Neq(operand ISyn) OpNeq   { return Neq(this, operand) }
func (this Named) Geq(operand ISyn) OpGeq   { return Geq(this, operand) }
func (this Named) Gt(operand ISyn) OpGt     { return Gt(this, operand) }
func (this Named) Leq(operand ISyn) OpLeq   { return Leq(this, operand) }
func (this Named) Lt(operand ISyn) OpLt     { return Lt(this, operand) }
func (this Named) Add(operand ISyn) OpAdd   { return Add(this, operand) }
func (this Named) Sub(operand ISyn) OpSub   { return Sub(this, operand) }
func (this Named) Mul(operand ISyn) OpMul   { return Mul(this, operand) }
func (this Named) Div(operand ISyn) OpDiv   { return Div(this, operand) }
func (this Named) Idx(operand ISyn) OpIdx   { return I(this, operand) }
func (this Named) Addr() OpAddr             { return Addr(this) }
func (this Named) Deref() OpDeref           { return Deref(this) }
func (this Named) Not() OpNot               { return Not(this) }
func (this Named) Neg() OpSub               { return Neg(this) }
