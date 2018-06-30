package udevgogen

func (this Named) At(idxs ...ISyn) OpIdx                { return I(append(Syns{this}, idxs...)...) }
func (this *ExprCall) Defer() StmtDefer                 { return Defer(this) }
func (this *ExprCall) Go() StmtGo                       { return Go(this) }
func (this PkgName) C(n string, args ...ISyn) *ExprCall { return C.D(string(this), n, args...) }

// Method constructs a `SynFunc` with the given `name` and `args` plus `this` as its method `Recv`.
func (this NamedTyped) Method(name string, args ...NamedTyped) *SynFunc {
	return Fn(this, name, TdFunc(args))
}

// Method constructs a `SynFunc` with the given `name` and `args` plus a `this`-typed method `Recv` also named `"this"`.
func (this *TypeRef) Method(name string, args ...NamedTyped) *SynFunc {
	return V.This.T(this).Method(name, args...)
}

func (this *TypeRef) Conv(expr ISyn) *ExprCall { return Call(this, expr) }

// Args sets `this.Type.Func.Args` and returns `this`.
func (this *SynFunc) Args(args ...NamedTyped) *SynFunc {
	this.Type.Func.Args = args
	return this
}

// Arg adds to `this.Type.Func.Args` and returns `this`.
func (this *SynFunc) Arg(name string, typeRef *TypeRef) *SynFunc {
	this.Type.Func.Args.Add(name, typeRef)
	return this
}

// Code adds to `this.SynBlock.Body` and returns `this`.
func (this *SynFunc) Code(stmts ...ISyn) *SynFunc {
	this.Add(stmts...)
	return this
}

// Doc adds to `this.Docs` and returns `this`.
func (this *SynFunc) Doc(docCommentLines ...string) *SynFunc {
	this.Docs.Add(docCommentLines...)
	return this
}

// N sets `this.Named.Name` and returns `this`.
func (this *SynFunc) N(name string) *SynFunc {
	this.Named.Name = name
	return this
}

// Rets sets `this.Type.Func.Rets` and returns `this`.
func (this *SynFunc) Rets(rets ...NamedTyped) *SynFunc {
	this.Type.Func.Rets = rets
	return this
}

// Ret adds to `this.Type.Func.Rets` and returns `this`.
func (this *SynFunc) Ret(name string, typeRef *TypeRef) *SynFunc {
	this.Type.Func.Rets.Add(name, typeRef)
	return this
}

// Sig sets `this.Type.Func` to `sig` and returns `this`.
func (this *SynFunc) Sig(sig *TypeFunc) *SynFunc {
	this.Type.Func = sig
	return this
}

func (this *StmtSwitch) Case(cond ISyn, thens ...ISyn) *StmtSwitch {
	this.Cases.Add(cond, thens...)
	return this
}

func (this *StmtSwitch) DefaultCase(stmts ...ISyn) *StmtSwitch {
	this.Default.Body = stmts
	return this
}

type IDotsAssignish interface {
	ISyn

	SetTo(ISyn) OpSet
	Decl(ISyn) OpDecl
}

type IDotsVarish interface {
	IDotsAssignish

	Idx(ISyn) OpIdx
	Addr() OpAddr
	Deref() OpDeref
}

type IDotsEquality interface {
	ISyn

	Eq(ISyn) OpEq
	Neq(ISyn) OpNeq
}

type IDotsBoolish interface {
	IDotsEquality

	And(ISyn) OpAnd
	Or(ISyn) OpOr
	Not() OpNot
}

type IDotsNumerish interface {
	IDotsEquality

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

type IDotsCallish interface {
	ISyn

	Call(...ISyn) *ExprCall
}

func (this Named) SetTo(operand ISyn) OpSet    { return Set(this, operand) }
func (this Named) Decl(operand ISyn) OpDecl    { return Decl(this, operand) }
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
func (this Named) Idx(operand ISyn) OpIdx      { return I(this, operand) }
func (this Named) Addr() OpAddr                { return Addr(this) }
func (this Named) Deref() OpDeref              { return Deref(this) }
func (this Named) Not() OpNot                  { return Not(this) }
func (this Named) Neg() OpSub                  { return Neg(this) }
func (this Named) Call(args ...ISyn) *ExprCall { return Call(this, args...) }

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
