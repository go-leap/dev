package udevgogen

// A is merely a handy convenience short-hand to create a slice of `ISyn`s,
// as sometimes needed for listing arguments, operands, or statements.
func A(argsOrOperandsOrStmts ...ISyn) []ISyn { return argsOrOperandsOrStmts }

// N constructs a `Named`.
func N(name string) Named { return Named{Name: name} }

// Nt constructs a `NamedTyped`.
func Nt(name string, t *TypeRef) NamedTyped { return NamedTyped{Type: t, Named: Named{Name: name}} }

// L constructs an `ExprLit`.
func L(lit interface{}) ExprLit { return ExprLit{Val: lit} }

// Add constructs an `OpAdd`.
func Add(operands ...ISyn) OpAdd { return OpAdd{Op: Op{Operands: operands}} }

// Addr constructs an `OpAddr`.
func Addr(operands ...ISyn) OpAddr { return OpAddr{Op: Op{Operands: operands}} }

// And constructs an `OpAnd`.
func And(operands ...ISyn) OpAnd { return OpAnd{Op: Op{Operands: operands}} }

// C constructs an `OpComma`.
func C(operands ...ISyn) OpComma { return OpComma{Op: Op{Operands: operands}} }

// D constructs an `OpDot`.
func D(operands ...ISyn) OpDot { return OpDot{Op: Op{Operands: operands}} }

// Decl constructs an `OpDecl`.
func Decl(operands ...ISyn) OpDecl { return OpDecl{Op: Op{Operands: operands}} }

// Deref constructs an `OpDeref`.
func Deref(operands ...ISyn) OpDeref { return OpDeref{Op: Op{Operands: operands}} }

// Div constructs an `OpDiv`.
func Div(operands ...ISyn) OpDiv { return OpDiv{Op: Op{Operands: operands}} }

// Eq constructs an `OpEq`.
func Eq(operands ...ISyn) OpEq { return OpEq{Op: Op{Operands: operands}} }

// Geq constructs an `OpGeq`.
func Geq(operands ...ISyn) OpGeq { return OpGeq{Op: Op{Operands: operands}} }

// Gt constructs an `OpGt`.
func Gt(operands ...ISyn) OpGt { return OpGt{Op: Op{Operands: operands}} }

// I constructs an `OpIdx`.
func I(operands ...ISyn) OpIdx { return OpIdx{Op: Op{Operands: operands}} }

// Leq constructs an `OpLeq`.
func Leq(operands ...ISyn) OpLeq { return OpLeq{Op: Op{Operands: operands}} }

// Lt constructs an `OpLt`.
func Lt(operands ...ISyn) OpLt { return OpLt{Op: Op{Operands: operands}} }

// Mul constructs an `OpMul`.
func Mul(operands ...ISyn) OpMul { return OpMul{Op: Op{Operands: operands}} }

// Neg constructs an unary `OpSub` to represent the given `operand`'s negation.
func Neg(operand ISyn) OpSub { return OpSub{Op: Op{Operands: []ISyn{operand}}} }

// Neq constructs an `OpNeq`.
func Neq(operands ...ISyn) OpNeq { return OpNeq{Op: Op{Operands: operands}} }

// Not constructs an `OpNot`.
func Not(operands ...ISyn) OpNot { return OpNot{Op: Op{Operands: operands}} }

// Or constructs an `OpOr`.
func Or(operands ...ISyn) OpOr { return OpOr{Op: Op{Operands: operands}} }

// Set constructs an `OpSet`.
func Set(operands ...ISyn) OpSet { return OpSet{Op: Op{Operands: operands}} }

// Sub constructs an `OpSub`.
func Sub(operands ...ISyn) OpSub { return OpSub{Op: Op{Operands: operands}} }

// TDecl constructs a named `TypeDecl` of the specified underlying type.
func TDecl(name string, typeRef *TypeRef, isAlias bool) (this TypeDecl) {
	this.IsAlias, this.Name, this.Type = isAlias, name, typeRef
	return
}

// TdFunc constructs a `TypeFunc`,
func TdFunc(args NamedsTypeds, rets ...NamedTyped) *TypeFunc {
	return &TypeFunc{Args: args, Rets: rets}
}

// TdInterface constructs a `TypeInterface`.
func TdInterface(embeds []*TypeRef, methods ...NamedTyped) *TypeInterface {
	return &TypeInterface{Embeds: embeds, Methods: methods}
}

// TdStruct constructs a `TypeStruct`.
func TdStruct(fields ...SynStructField) *TypeStruct {
	return &TypeStruct{Fields: fields}
}

// TdStructFld constructs a `SynStructField` for `TypeStruct`s.
func TdStructFld(name string, typeRef *TypeRef, tags map[string]string) (fld SynStructField) {
	fld.Tags, fld.Name, fld.Type = tags, name, typeRef
	return
}

// TrFunc constructs a `TypeRef` referring to the specified unnamed `func(..)(..)` signature.
func TrFunc(typeFunc *TypeFunc) *TypeRef { return &TypeRef{Func: typeFunc} }

// TrInterface constructs a `TypeRef` referring to the specified unnamed `interface{..}`.
func TrInterface(typeIface *TypeInterface) *TypeRef { return &TypeRef{Interface: typeIface} }

// TrStruct constructs a `TypeRef` referring to the specified unnamed `struct{..}`.
func TrStruct(typeStruct *TypeStruct) *TypeRef { return &TypeRef{Struct: typeStruct} }

// TrPtr constructs a `TypeRef` referring to a pointer to the specified type.
func TrPtr(typeRef *TypeRef) *TypeRef { return &TypeRef{Ptr: typeRef} }

// TrSlice constructs a `TypeRef` referring to a slice of the specified type.
func TrSlice(typeRef *TypeRef) *TypeRef { return &TypeRef{Slice: typeRef} }

// TrNamed constructs a `TypeRef` referring to the specified named type.
func TrNamed(pkgName string, typeName string) (this *TypeRef) {
	this = &TypeRef{}
	this.Named.PkgName, this.Named.TypeName = pkgName, typeName
	return
}

// TrMap constructs a `TypeRef` referring to a map with the specified key and value types.
func TrMap(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.Map.Key, this.Map.Val = keyType, valType
	return
}

// Block constructs a `SynBlock`.
func Block(body ...ISyn) (this SynBlock) {
	this.Body = body
	return
}

// Call constructs an `ExprCall`.
func Call(callee ISyn, args ...ISyn) *ExprCall {
	return &ExprCall{Callee: callee, Args: args}
}

// Const constructs a `StmtConst`.
func Const(name string, maybeType *TypeRef, exprLit ExprLit) (this *StmtConst) {
	this = &StmtConst{Expr: exprLit}
	this.Name, this.Type = name, maybeType
	return
}

// Defer constructs a `StmtDefer`.
func Defer(call *ExprCall) (this StmtDefer) {
	this.Expr = call
	return
}

// File constructs a `SourceFile`.
func File(pkgName string, topLevelDecls ...ISyn) *SourceFile {
	return &SourceFile{PkgName: pkgName, SynBlock: SynBlock{Body: topLevelDecls}}
}

// ForLoop constructs a `StmtFor` that emits a classical `for` (not `range`) loop.
func ForLoop(maybeInit ISyn, maybeCond ISyn, maybeEach ISyn, body ...ISyn) (this *StmtFor) {
	this = &StmtFor{}
	this.Body, this.Loop.Init, this.Loop.Cond, this.Loop.Each = body, maybeInit, maybeCond, maybeEach
	return
}

// ForRange constructs a `StmtFor` that emits a `for .. range` loop.
func ForRange(maybeIdx Named, maybeVal Named, iteree ISyn, body ...ISyn) (this *StmtFor) {
	this = &StmtFor{}
	this.Body, this.Range.Idx, this.Range.Val, this.Range.Iteree = body, maybeIdx, maybeVal, iteree
	return
}

// Fn constructs a `SynFunc`. If `maybeRecv` is given, it will represent a method of that type.
func Fn(maybeRecv NamedTyped, name string, sig *TypeFunc, body ...ISyn) (this *SynFunc) {
	this = &SynFunc{Recv: maybeRecv}
	this.Body, this.Name, this.Type = body, name, TrFunc(sig)
	return
}

// Go constructs a `StmtGo`.
func Go(call *ExprCall) (this StmtGo) {
	this.Expr = call
	return
}

// If constructs a simple `StmtIf` with a single condition
// and `then` branch (plus initially empty `else` branch).
func If(cond ISyn, thens ...ISyn) *StmtIf {
	return &StmtIf{IfThens: []SynCond{{Cond: cond, SynBlock: SynBlock{Body: thens}}}}
}

// Ifs constructs a more complex `StmtIf` than `If` does,
// with `ifThensAndMaybeAnElse` containing 0 or more alternating
// pairs of `if` (or `else if`) conditions and corresponding
// `then` branches (each a `SynBlock`), plus optionally a final
// `else` branch (also a `SynBlock`).
func Ifs(ifThensAndMaybeAnElse ...ISyn) (this *StmtIf) {
	this = &StmtIf{}
	if l := len(ifThensAndMaybeAnElse); l%2 != 0 {
		if block, ok := ifThensAndMaybeAnElse[l-1].(SynBlock); ok {
			this.Else.Body = block.Body
		}
		ifThensAndMaybeAnElse = ifThensAndMaybeAnElse[:l-1]
	}
	for i := 1; i < len(ifThensAndMaybeAnElse); i += 2 {
		var body []ISyn
		if block, ok := ifThensAndMaybeAnElse[i].(SynBlock); ok {
			body = block.Body
		}
		this.IfThens = append(this.IfThens, SynCond{Cond: ifThensAndMaybeAnElse[i-1], SynBlock: SynBlock{Body: body}})
	}
	return
}

// Ret constructs a `StmtRet`.
func Ret(retExpr ISyn) (this StmtRet) {
	this.Expr = retExpr
	return
}

// Switch constructs a `StmtSwitch`.
func Switch(maybeCond ISyn, caseCondsAndBlocksPlusMaybeDefaultBlock ...ISyn) (this *StmtSwitch) {
	this = &StmtSwitch{Scrutinee: maybeCond}
	if l := len(caseCondsAndBlocksPlusMaybeDefaultBlock); l%2 != 0 {
		if block, ok := caseCondsAndBlocksPlusMaybeDefaultBlock[l-1].(SynBlock); ok {
			this.Default.Body = block.Body
		}
		caseCondsAndBlocksPlusMaybeDefaultBlock = caseCondsAndBlocksPlusMaybeDefaultBlock[:l-1]
	}
	for i := 1; i < len(caseCondsAndBlocksPlusMaybeDefaultBlock); i += 2 {
		var body []ISyn
		if block, ok := caseCondsAndBlocksPlusMaybeDefaultBlock[i].(SynBlock); ok {
			body = block.Body
		}
		this.Cases = append(this.Cases, SynCond{Cond: caseCondsAndBlocksPlusMaybeDefaultBlock[i-1], SynBlock: SynBlock{Body: body}})
	}
	return
}

// Var constructs a `StmtVar`.
func Var(name string, maybeType *TypeRef, maybeExpr ISyn) (this *StmtVar) {
	this = &StmtVar{Expr: maybeExpr}
	this.Name, this.Type = name, maybeType
	return
}

// Cond constructs a `SynCond` as used in `StmtIf`s and `StmtSwitch`es.
func Cond(cond ISyn, thens ...ISyn) (this SynCond) {
	this.Cond, this.Body = cond, thens
	return
}