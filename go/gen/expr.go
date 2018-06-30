package udevgogen

type IExprAssignish interface {
	ISyn

	SetTo(ISyn) OpSet
	Decl(ISyn) OpDecl
}

type IExprVarish interface {
	IExprAssignish

	At(ISyn) OpIdx
	Addr() OpAddr
	Deref() OpDeref
}

type IExprEqualish interface {
	ISyn

	Eq(ISyn) OpEq
	Neq(ISyn) OpNeq
}

type IExprBoolish interface {
	IExprEqualish

	And(ISyn) OpAnd
	Or(ISyn) OpOr
	Not() OpNot
}

type IExprNumerish interface {
	IExprEqualish

	Geq(ISyn) OpGeq
	Leq(ISyn) OpLeq
	Gt(ISyn) OpGt
	Lt(ISyn) OpLt

	Add(ISyn) OpAdd
	Sub(ISyn) OpSub
	Mul(ISyn) OpMul
	Div(ISyn) OpDiv
	Neg() OpSub
}

type IExprCallish interface {
	ISyn

	Call(...ISyn) *ExprCall
}

func (this Named) SetTo(operand ISyn) OpSet    { return Set(this, operand) }
func (this Named) Decl(operand ISyn) OpDecl    { return Decl(this, operand) }
func (this Named) Addr() OpAddr                { return Addr(this) }
func (this Named) Deref() OpDeref              { return Deref(this) }
func (this Named) And(operand ISyn) OpAnd      { return And(this, operand) }
func (this Named) Or(operand ISyn) OpOr        { return Or(this, operand) }
func (this Named) Eq(operand ISyn) OpEq        { return Eq(this, operand) }
func (this Named) Neq(operand ISyn) OpNeq      { return Neq(this, operand) }
func (this Named) Geq(operand ISyn) OpGeq      { return Geq(this, operand) }
func (this Named) Gt(operand ISyn) OpGt        { return Gt(this, operand) }
func (this Named) Leq(operand ISyn) OpLeq      { return Leq(this, operand) }
func (this Named) Lt(operand ISyn) OpLt        { return Lt(this, operand) }
func (this Named) Add(operand ISyn) OpAdd      { return Add(this, operand) }
func (this Named) Sub(operand ISyn) OpSub      { return Sub(this, operand) }
func (this Named) Mul(operand ISyn) OpMul      { return Mul(this, operand) }
func (this Named) Div(operand ISyn) OpDiv      { return Div(this, operand) }
func (this Named) At(operand ISyn) OpIdx       { return I(this, operand) }
func (this Named) Not() OpNot                  { return Not(this) }
func (this Named) Neg() OpSub                  { return Neg(this) }
func (this Named) Call(args ...ISyn) *ExprCall { return Call(this, args...) }

func (this *ExprCall) Deref() OpDeref              { return Deref(this) }
func (this *ExprCall) And(operand ISyn) OpAnd      { return And(this, operand) }
func (this *ExprCall) Or(operand ISyn) OpOr        { return Or(this, operand) }
func (this *ExprCall) Eq(operand ISyn) OpEq        { return Eq(this, operand) }
func (this *ExprCall) Neq(operand ISyn) OpNeq      { return Neq(this, operand) }
func (this *ExprCall) Geq(operand ISyn) OpGeq      { return Geq(this, operand) }
func (this *ExprCall) Gt(operand ISyn) OpGt        { return Gt(this, operand) }
func (this *ExprCall) Leq(operand ISyn) OpLeq      { return Leq(this, operand) }
func (this *ExprCall) Lt(operand ISyn) OpLt        { return Lt(this, operand) }
func (this *ExprCall) Add(operand ISyn) OpAdd      { return Add(this, operand) }
func (this *ExprCall) Sub(operand ISyn) OpSub      { return Sub(this, operand) }
func (this *ExprCall) Mul(operand ISyn) OpMul      { return Mul(this, operand) }
func (this *ExprCall) Div(operand ISyn) OpDiv      { return Div(this, operand) }
func (this *ExprCall) At(operand ISyn) OpIdx       { return I(this, operand) }
func (this *ExprCall) Not() OpNot                  { return Not(this) }
func (this *ExprCall) Neg() OpSub                  { return Neg(this) }
func (this *ExprCall) Call(args ...ISyn) *ExprCall { return Call(this, args...) }

func (this OpGeq) Eq(operand ISyn) OpEq   { return Eq(this, operand) }
func (this OpGeq) Neq(operand ISyn) OpNeq { return Neq(this, operand) }
func (this OpGeq) And(operand ISyn) OpAnd { return And(this, operand) }
func (this OpGeq) Or(operand ISyn) OpOr   { return Or(this, operand) }
func (this OpGeq) Not() OpNot             { return Not(this) }

func (this OpLeq) Eq(operand ISyn) OpEq   { return Eq(this, operand) }
func (this OpLeq) Neq(operand ISyn) OpNeq { return Neq(this, operand) }
func (this OpLeq) And(operand ISyn) OpAnd { return And(this, operand) }
func (this OpLeq) Or(operand ISyn) OpOr   { return Or(this, operand) }
func (this OpLeq) Not() OpNot             { return Not(this) }

func (this OpGt) Eq(operand ISyn) OpEq   { return Eq(this, operand) }
func (this OpGt) Neq(operand ISyn) OpNeq { return Neq(this, operand) }
func (this OpGt) And(operand ISyn) OpAnd { return And(this, operand) }
func (this OpGt) Or(operand ISyn) OpOr   { return Or(this, operand) }
func (this OpGt) Not() OpNot             { return Not(this) }

func (this OpLt) Eq(operand ISyn) OpEq   { return Eq(this, operand) }
func (this OpLt) Neq(operand ISyn) OpNeq { return Neq(this, operand) }
func (this OpLt) And(operand ISyn) OpAnd { return And(this, operand) }
func (this OpLt) Or(operand ISyn) OpOr   { return Or(this, operand) }
func (this OpLt) Not() OpNot             { return Not(this) }

func (this OpEq) Eq(operand ISyn) OpEq   { return Eq(this, operand) }
func (this OpEq) Neq(operand ISyn) OpNeq { return Neq(this, operand) }
func (this OpEq) And(operand ISyn) OpAnd { return And(this, operand) }
func (this OpEq) Or(operand ISyn) OpOr   { return Or(this, operand) }
func (this OpEq) Not() OpNot             { return Not(this) }

func (this OpNeq) Eq(operand ISyn) OpEq   { return Eq(this, operand) }
func (this OpNeq) Neq(operand ISyn) OpNeq { return Neq(this, operand) }
func (this OpNeq) And(operand ISyn) OpAnd { return And(this, operand) }
func (this OpNeq) Or(operand ISyn) OpOr   { return Or(this, operand) }
func (this OpNeq) Not() OpNot             { return Not(this) }

func (this OpComma) SetTo(operand ISyn) OpSet { return Set(this, operand) }
func (this OpComma) Decl(operand ISyn) OpDecl { return Decl(this, operand) }

func (this OpOr) Eq(operand ISyn) OpEq   { return Eq(this, operand) }
func (this OpOr) Neq(operand ISyn) OpNeq { return Neq(this, operand) }
func (this OpOr) And(operand ISyn) OpAnd { return And(this, operand) }
func (this OpOr) Or(operand ISyn) OpOr   { return Or(this, operand) }
func (this OpOr) Not() OpNot             { return Not(this) }

func (this OpAnd) Eq(operand ISyn) OpEq   { return Eq(this, operand) }
func (this OpAnd) Neq(operand ISyn) OpNeq { return Neq(this, operand) }
func (this OpAnd) And(operand ISyn) OpAnd { return And(this, operand) }
func (this OpAnd) Or(operand ISyn) OpOr   { return Or(this, operand) }
func (this OpAnd) Not() OpNot             { return Not(this) }

func (this OpNot) Eq(operand ISyn) OpEq   { return Eq(this, operand) }
func (this OpNot) Neq(operand ISyn) OpNeq { return Neq(this, operand) }
func (this OpNot) And(operand ISyn) OpAnd { return And(this, operand) }
func (this OpNot) Or(operand ISyn) OpOr   { return Or(this, operand) }
func (this OpNot) Not() OpNot             { return Not(this) }
