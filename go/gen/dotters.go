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

type ISynDot interface {
	ISyn
	Geq(ISyn) OpGeq
	Leq(ISyn) OpLeq
	Gt(ISyn) OpGt
	Lt(ISyn) OpLt
	SetTo(ISyn) OpSet
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

func (this OpGeq) SetTo(operand ISyn) OpSet    { return Set(this, operand) }
func (this OpGeq) Decl(operand ISyn) OpDecl    { return Decl(this, operand) }
func (this OpGeq) And(operand ISyn) OpAnd      { return And(this, operand) }
func (this OpGeq) Or(operand ISyn) OpOr        { return Or(this, operand) }
func (this OpGeq) Eq(operand ISyn) OpEq        { return Eq(this, operand) }
func (this OpGeq) Neq(operand ISyn) OpNeq      { return Neq(this, operand) }
func (this OpGeq) Geq(operand ISyn) OpGeq      { return Geq(this, operand) }
func (this OpGeq) Gt(operand ISyn) OpGt        { return Gt(this, operand) }
func (this OpGeq) Leq(operand ISyn) OpLeq      { return Leq(this, operand) }
func (this OpGeq) Lt(operand ISyn) OpLt        { return Lt(this, operand) }
func (this OpGeq) Add(operand ISyn) OpAdd      { return Add(this, operand) }
func (this OpGeq) Sub(operand ISyn) OpSub      { return Sub(this, operand) }
func (this OpGeq) Mul(operand ISyn) OpMul      { return Mul(this, operand) }
func (this OpGeq) Div(operand ISyn) OpDiv      { return Div(this, operand) }
func (this OpGeq) Idx(operand ISyn) OpIdx      { return I(this, operand) }
func (this OpGeq) Addr() OpAddr                { return Addr(this) }
func (this OpGeq) Deref() OpDeref              { return Deref(this) }
func (this OpGeq) Not() OpNot                  { return Not(this) }
func (this OpGeq) Neg() OpSub                  { return Neg(this) }
func (this OpGeq) Call(args ...ISyn) *ExprCall { return Call(this, args...) }

func (this OpLeq) SetTo(operand ISyn) OpSet    { return Set(this, operand) }
func (this OpLeq) Decl(operand ISyn) OpDecl    { return Decl(this, operand) }
func (this OpLeq) And(operand ISyn) OpAnd      { return And(this, operand) }
func (this OpLeq) Or(operand ISyn) OpOr        { return Or(this, operand) }
func (this OpLeq) Eq(operand ISyn) OpEq        { return Eq(this, operand) }
func (this OpLeq) Neq(operand ISyn) OpNeq      { return Neq(this, operand) }
func (this OpLeq) Geq(operand ISyn) OpGeq      { return Geq(this, operand) }
func (this OpLeq) Gt(operand ISyn) OpGt        { return Gt(this, operand) }
func (this OpLeq) Leq(operand ISyn) OpLeq      { return Leq(this, operand) }
func (this OpLeq) Lt(operand ISyn) OpLt        { return Lt(this, operand) }
func (this OpLeq) Add(operand ISyn) OpAdd      { return Add(this, operand) }
func (this OpLeq) Sub(operand ISyn) OpSub      { return Sub(this, operand) }
func (this OpLeq) Mul(operand ISyn) OpMul      { return Mul(this, operand) }
func (this OpLeq) Div(operand ISyn) OpDiv      { return Div(this, operand) }
func (this OpLeq) Idx(operand ISyn) OpIdx      { return I(this, operand) }
func (this OpLeq) Addr() OpAddr                { return Addr(this) }
func (this OpLeq) Deref() OpDeref              { return Deref(this) }
func (this OpLeq) Not() OpNot                  { return Not(this) }
func (this OpLeq) Neg() OpSub                  { return Neg(this) }
func (this OpLeq) Call(args ...ISyn) *ExprCall { return Call(this, args...) }

func (this OpGt) SetTo(operand ISyn) OpSet    { return Set(this, operand) }
func (this OpGt) Decl(operand ISyn) OpDecl    { return Decl(this, operand) }
func (this OpGt) And(operand ISyn) OpAnd      { return And(this, operand) }
func (this OpGt) Or(operand ISyn) OpOr        { return Or(this, operand) }
func (this OpGt) Eq(operand ISyn) OpEq        { return Eq(this, operand) }
func (this OpGt) Neq(operand ISyn) OpNeq      { return Neq(this, operand) }
func (this OpGt) Geq(operand ISyn) OpGeq      { return Geq(this, operand) }
func (this OpGt) Gt(operand ISyn) OpGt        { return Gt(this, operand) }
func (this OpGt) Leq(operand ISyn) OpLeq      { return Leq(this, operand) }
func (this OpGt) Lt(operand ISyn) OpLt        { return Lt(this, operand) }
func (this OpGt) Add(operand ISyn) OpAdd      { return Add(this, operand) }
func (this OpGt) Sub(operand ISyn) OpSub      { return Sub(this, operand) }
func (this OpGt) Mul(operand ISyn) OpMul      { return Mul(this, operand) }
func (this OpGt) Div(operand ISyn) OpDiv      { return Div(this, operand) }
func (this OpGt) Idx(operand ISyn) OpIdx      { return I(this, operand) }
func (this OpGt) Addr() OpAddr                { return Addr(this) }
func (this OpGt) Deref() OpDeref              { return Deref(this) }
func (this OpGt) Not() OpNot                  { return Not(this) }
func (this OpGt) Neg() OpSub                  { return Neg(this) }
func (this OpGt) Call(args ...ISyn) *ExprCall { return Call(this, args...) }

func (this OpLt) SetTo(operand ISyn) OpSet    { return Set(this, operand) }
func (this OpLt) Decl(operand ISyn) OpDecl    { return Decl(this, operand) }
func (this OpLt) And(operand ISyn) OpAnd      { return And(this, operand) }
func (this OpLt) Or(operand ISyn) OpOr        { return Or(this, operand) }
func (this OpLt) Eq(operand ISyn) OpEq        { return Eq(this, operand) }
func (this OpLt) Neq(operand ISyn) OpNeq      { return Neq(this, operand) }
func (this OpLt) Geq(operand ISyn) OpGeq      { return Geq(this, operand) }
func (this OpLt) Gt(operand ISyn) OpGt        { return Gt(this, operand) }
func (this OpLt) Leq(operand ISyn) OpLeq      { return Leq(this, operand) }
func (this OpLt) Lt(operand ISyn) OpLt        { return Lt(this, operand) }
func (this OpLt) Add(operand ISyn) OpAdd      { return Add(this, operand) }
func (this OpLt) Sub(operand ISyn) OpSub      { return Sub(this, operand) }
func (this OpLt) Mul(operand ISyn) OpMul      { return Mul(this, operand) }
func (this OpLt) Div(operand ISyn) OpDiv      { return Div(this, operand) }
func (this OpLt) Idx(operand ISyn) OpIdx      { return I(this, operand) }
func (this OpLt) Addr() OpAddr                { return Addr(this) }
func (this OpLt) Deref() OpDeref              { return Deref(this) }
func (this OpLt) Not() OpNot                  { return Not(this) }
func (this OpLt) Neg() OpSub                  { return Neg(this) }
func (this OpLt) Call(args ...ISyn) *ExprCall { return Call(this, args...) }

func (this OpComma) SetTo(operand ISyn) OpSet    { return Set(this, operand) }
func (this OpComma) Decl(operand ISyn) OpDecl    { return Decl(this, operand) }
func (this OpComma) And(operand ISyn) OpAnd      { return And(this, operand) }
func (this OpComma) Or(operand ISyn) OpOr        { return Or(this, operand) }
func (this OpComma) Eq(operand ISyn) OpEq        { return Eq(this, operand) }
func (this OpComma) Neq(operand ISyn) OpNeq      { return Neq(this, operand) }
func (this OpComma) Geq(operand ISyn) OpGeq      { return Geq(this, operand) }
func (this OpComma) Gt(operand ISyn) OpGt        { return Gt(this, operand) }
func (this OpComma) Leq(operand ISyn) OpLeq      { return Leq(this, operand) }
func (this OpComma) Lt(operand ISyn) OpLt        { return Lt(this, operand) }
func (this OpComma) Add(operand ISyn) OpAdd      { return Add(this, operand) }
func (this OpComma) Sub(operand ISyn) OpSub      { return Sub(this, operand) }
func (this OpComma) Mul(operand ISyn) OpMul      { return Mul(this, operand) }
func (this OpComma) Div(operand ISyn) OpDiv      { return Div(this, operand) }
func (this OpComma) Idx(operand ISyn) OpIdx      { return I(this, operand) }
func (this OpComma) Addr() OpAddr                { return Addr(this) }
func (this OpComma) Deref() OpDeref              { return Deref(this) }
func (this OpComma) Not() OpNot                  { return Not(this) }
func (this OpComma) Neg() OpSub                  { return Neg(this) }
func (this OpComma) Call(args ...ISyn) *ExprCall { return Call(this, args...) }
