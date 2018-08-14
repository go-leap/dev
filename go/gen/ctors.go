package udevgogen

// N constructs a `Named`.
func N(name string) Named { return Named{Name: name} }

// Names constructs an `OpComma` of `Named` operands.
func Names(names ...string) OpComma {
	operands := make(Syns, len(names))
	for i := range names {
		operands[i] = Named{Name: names[i]}
	}
	return OpComma{Op: Op{Operands: operands}}
}

// L constructs an `ExprLit`.
func L(lit IAny) ExprLit { return ExprLit{Val: lit} }

// Lits constructs an `OpComma` of `ExprLit` operands.
func Lits(lits ...IAny) OpComma {
	operands := make(Syns, len(lits))
	for i := range lits {
		operands[i] = ExprLit{Val: lits[i]}
	}
	return OpComma{Op: Op{Operands: operands}}
}

// Label constructs a `StmtLabel` with the given `name` and associated code `SynBlock`.
func Label(name string, stmts ...ISyn) *StmtLabel {
	return &StmtLabel{Named: Named{Name: name}, SynBlock: SynBlock{Body: stmts}}
}

// GoTo constructs a `StmtGoTo`.
func GoTo(name string) StmtGoTo {
	return StmtGoTo{Name: name}
}

// Add constructs an `OpAdd`.
func Add(operands ...ISyn) OpAdd { return OpAdd{Op: Op{Operands: operands}} }

// Addr constructs an `OpAddr`.
func Addr(operands ...ISyn) OpAddr { return OpAddr{Op: Op{Operands: operands}} }

// And constructs an `OpAnd`.
func And(operands ...ISyn) OpAnd {
	if len(operands) == 2 {
		if opand, ok := operands[0].(OpAnd); ok {
			opand.Operands = append(opand.Operands, operands[1])
			return opand
		}
	}
	return OpAnd{Op: Op{Operands: operands}}
}

// Tup constructs an `OpComma`.
func Tup(operands ...ISyn) OpComma { return OpComma{Op: Op{Operands: operands}} }

// Sl constructs an `OpColon`.
func Sl(operands ...ISyn) OpColon {
	for i := range operands {
		if lit, okl := operands[i].(ExprLit); okl {
			if intlit, oki := lit.Val.(int); (oki && intlit == -1) || lit.Val == nil {
				operands[i] = nil
			}
		}
	}
	return OpColon{Op: Op{Operands: operands}}
}

// D constructs an `OpDot`.
func D(operands ...ISyn) OpDot { return OpDot{Op: Op{Operands: operands}} }

// Decl constructs an `OpDecl`.
func Decl(operands ...ISyn) OpDecl { return OpDecl{Op: Op{Operands: operands}} }

// Deref constructs an `OpDeref`.
func Deref(operands ...ISyn) OpDeref { return OpDeref{Op: Op{Operands: operands}} }

// Div constructs an `OpDiv`.
func Div(operands ...ISyn) OpDiv { return OpDiv{Op: Op{Operands: operands}} }

// Mod constructs an `OpMod`.
func Mod(operands ...ISyn) OpMod { return OpMod{Op: Op{Operands: operands}} }

// Eq constructs an `OpEq`.
func Eq(operands ...ISyn) OpEq { return OpEq{Op: Op{Operands: operands}} }

// Geq constructs an `OpGeq`.
func Geq(operands ...ISyn) OpGeq { return OpGeq{Op: Op{Operands: operands}} }

// Gt constructs an `OpGt`.
func Gt(operands ...ISyn) OpGt { return OpGt{Op: Op{Operands: operands}} }

// At constructs an `OpIdx`.
func At(operands ...ISyn) OpIdx { return OpIdx{Op: Op{Operands: operands}} }

// Leq constructs an `OpLeq`.
func Leq(operands ...ISyn) OpLeq { return OpLeq{Op: Op{Operands: operands}} }

// Lt constructs an `OpLt`.
func Lt(operands ...ISyn) OpLt { return OpLt{Op: Op{Operands: operands}} }

// Mul constructs an `OpMul`.
func Mul(operands ...ISyn) OpMul { return OpMul{Op: Op{Operands: operands}} }

// Neg constructs an unary `OpSub` to represent the given `operand`'s negation.
func Neg(operand ISyn) OpSub { return OpSub{Op: Op{Operands: Syns{operand}}} }

// Neq constructs an `OpNeq`.
func Neq(operands ...ISyn) OpNeq { return OpNeq{Op: Op{Operands: operands}} }

// Not constructs an `OpNot`.
func Not(operands ...ISyn) OpNot { return OpNot{Op: Op{Operands: operands}} }

// Or constructs an `OpOr`.
func Or(operands ...ISyn) OpOr {
	if len(operands) == 2 {
		if opor, ok := operands[0].(OpOr); ok {
			opor.Operands = append(opor.Operands, operands[1])
			return opor
		}
	}
	return OpOr{Op: Op{Operands: operands}}
}

// Set constructs an `OpSet`.
func Set(operands ...ISyn) OpSet { return OpSet{Op: Op{Operands: operands}} }

// Sub constructs an `OpSub`.
func Sub(operands ...ISyn) OpSub { return OpSub{Op: Op{Operands: operands}} }

// TDecl constructs a named `TypeDecl` of the specified underlying type.
func TDecl(name string, typeRef *TypeRef, isAlias bool) (this *TypeDecl) {
	this = &TypeDecl{IsAlias: isAlias}
	this.Name, this.Type = name, typeRef
	return
}

// TdFunc constructs an initially-empty (arg-less and return-less) `TypeFunc`,
func TdFunc(args ...NamedTyped) *TypeFunc {
	return &TypeFunc{Args: args}
}

// TdFn constructs a `TypeFunc`,
func TdFn(args NamedsTypeds, rets ...NamedTyped) *TypeFunc {
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

// TFunc constructs a `TypeRef` referring to the specified unnamed `func(..)(..)` signature.
func TFunc(typeFunc *TypeFunc) *TypeRef { return &TypeRef{Func: typeFunc} }

// TInterface constructs a `TypeRef` referring to the specified unnamed `interface{..}`.
func TInterface(typeIface *TypeInterface) *TypeRef { return &TypeRef{Interface: typeIface} }

// TStruct constructs a `TypeRef` referring to the specified unnamed `struct{..}`.
func TStruct(typeStruct *TypeStruct) *TypeRef { return &TypeRef{Struct: typeStruct} }

// TPointer constructs a `TypeRef` referring to a pointer to the specified type.
func TPointer(typeRef *TypeRef) *TypeRef {
	var tref TypeRef
	tref.Pointer.Of = typeRef
	return &tref
}

// TArray constructs a `TypeRef` referring to an array of the specified type.
func TArray(numElems ISyn, typeRef *TypeRef) *TypeRef {
	var tref TypeRef
	tref.ArrOrSlice.Of, tref.ArrOrSlice.IsFixedLen = typeRef, numElems
	return &tref
}

// TChan constructs a `TypeRef` referring to the specified channel. TODO: TypeRef.emitTo implementation!
func TChan(of *TypeRef, dirRecv bool, dirSend bool) *TypeRef {
	var tref TypeRef
	tref.Chan.Of, tref.Chan.DirRecv, tref.Chan.DirSend = of, dirRecv, dirSend
	return &tref
}

// TSlice constructs a `TypeRef` referring to a slice of the specified type.
func TSlice(typeRef *TypeRef) *TypeRef {
	var tref TypeRef
	tref.ArrOrSlice.Of = typeRef
	return &tref
}

// TFrom constructs a `TypeRef` referring to the specified named type exported from the given package.
func TFrom(pkgName PkgName, typeName string) (this *TypeRef) {
	this = &TypeRef{}
	this.Named.PkgName, this.Named.TypeName = string(pkgName), typeName
	return
}

// TFrom constructs a `TypeRef` referring to the specified named type in the local package.
func TLocal(typeName string) (this *TypeRef) {
	this = &TypeRef{}
	this.Named.TypeName = typeName
	return
}

// TMap constructs a `TypeRef` referring to a map with the specified key and value types.
func TMap(ofKey *TypeRef, toVal *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.Map.OfKey, this.Map.ToVal = ofKey, toVal
	return
}

// Block constructs a `SynBlock`.
func Block(body ...ISyn) (this SynBlock) {
	this.Body = body
	return
}

// Then simply returns `body`, just like `Else` does: it's only readability sugar for use in `If` (or `GEN_IF`) calls.
func Then(body ...ISyn) Syns {
	return body
}

// Else simply returns `body`, just like `Then` does: it's only readability sugar for use in `If` (or `GEN_IF`) calls.
func Else(body ...ISyn) Syns {
	return body
}

// Call constructs an `ExprCall`.
func Call(callee ISyn, args ...ISyn) *ExprCall {
	return &ExprCall{Callee: callee, Args: args}
}

func C(callee IAny, args ...IAny) *ExprCall {
	var syn ISyn
	switch c := callee.(type) {
	case ISyn:
		syn = c
	case string:
		syn = N(c)
	}
	return &ExprCall{Callee: syn, Args: SynsFrom(nil, args...)}
}

// Const constructs a `StmtConst`.
func Const(name string, maybeType *TypeRef, expr ISyn) (this *StmtConst) {
	this = &StmtConst{Expr: expr}
	this.Name, this.Type = name, maybeType
	return
}

// Defer constructs a `StmtDefer`.
func Defer(call *ExprCall) (this StmtDefer) {
	this.Expr = call
	return
}

// File constructs a `SourceFile`.
func File(pkgName string, allocBodyCap int, topLevelDecls ...ISyn) *SourceFile {
	if allocBodyCap < len(topLevelDecls) {
		allocBodyCap = len(topLevelDecls)
	}
	return &SourceFile{PkgName: pkgName, SynBlock: SynBlock{Body: append(make(Syns, 0, allocBodyCap), topLevelDecls...)}}
}

// For constructs a `StmtFor` that emits a classical `for` (not `range`) loop.
func For(maybeInit ISyn, maybeCond ISyn, maybeStep ISyn, body ...ISyn) (this *StmtFor) {
	this = &StmtFor{}
	this.Body, this.Loop.Init, this.Loop.Cond, this.Loop.Step = body, maybeInit, maybeCond, maybeStep
	return
}

// ForEach constructs a `StmtFor` that emits a `for .. range` loop.
func ForEach(maybeIdx Named, maybeVal Named, iteree ISyn, body ...ISyn) (this *StmtFor) {
	this = &StmtFor{}
	this.Body, this.Range.Key, this.Range.Val, this.Range.Over = body, maybeIdx, maybeVal, iteree
	return
}

// Go constructs a `StmtGo`.
func Go(call *ExprCall) (this StmtGo) {
	this.Expr = call
	return
}

// If constructs a `StmtIf` with `ifThensAndMaybeAnElse` containing
// 0 or more alternating-pairs of `if` conditions and corresponding
// `then` branches, plus optionally a final `else` branch.
//
// Should any of the `if` conditions be `nil`, then `this` will return as `nil`.
func If(ifThensAndMaybeAnElse ...ISyn) (this *StmtIf) {
	if len(ifThensAndMaybeAnElse) > 0 || ifThensAndMaybeAnElse[0] != nil {
		this = &StmtIf{IfThens: make(SynCases, 0, 1+len(ifThensAndMaybeAnElse)/2)}
		if l := len(ifThensAndMaybeAnElse); l%2 != 0 {
			this.Else.Body = SynsFrom(ifThensAndMaybeAnElse[l-1])
			ifThensAndMaybeAnElse = ifThensAndMaybeAnElse[:l-1]
		}
		for i := 1; i < len(ifThensAndMaybeAnElse); i += 2 {
			cond, body := ifThensAndMaybeAnElse[i-1], SynsFrom(ifThensAndMaybeAnElse[i])
			this.IfThens = append(this.IfThens, SynCase{Cond: cond, SynBlock: SynBlock{Body: body}})
		}
	}
	return
}

// Ret constructs a `StmtRet`.
// To have it generate `return nil`, your `retExpr` should equal
// `B.Nil` (ie. an `ExprLit` with no `Val` set). If `nil` is passed
// for `retExpr`, this generates an empty `return;` statement.
func Ret(retExpr ISyn) (this StmtRet) {
	this.Expr = retExpr
	return
}

// Switch constructs a `StmtSwitch`.
func Switch(maybeScrutinee ISyn, caseCondsAndBlocksPlusMaybeDefaultBlock ...ISyn) (this *StmtSwitch) {
	this = &StmtSwitch{Scrutinee: maybeScrutinee, Cases: make(SynCases, 0, 1+len(caseCondsAndBlocksPlusMaybeDefaultBlock)/2)}
	if l := len(caseCondsAndBlocksPlusMaybeDefaultBlock); l%2 != 0 {
		this.Default.Body = SynsFrom(caseCondsAndBlocksPlusMaybeDefaultBlock[l-1])
		caseCondsAndBlocksPlusMaybeDefaultBlock = caseCondsAndBlocksPlusMaybeDefaultBlock[:l-1]
	}
	for i := 1; i < len(caseCondsAndBlocksPlusMaybeDefaultBlock); i += 2 {
		body := SynsFrom(caseCondsAndBlocksPlusMaybeDefaultBlock[i])
		this.Cases = append(this.Cases, SynCase{Cond: caseCondsAndBlocksPlusMaybeDefaultBlock[i-1], SynBlock: SynBlock{Body: body}})
	}
	return
}

// Var constructs a `StmtVar`.
func Var(name string, maybeType *TypeRef, maybeExpr ISyn) (this *StmtVar) {
	this = &StmtVar{Expr: maybeExpr}
	this.Name, this.Type = name, maybeType
	return
}

// Case constructs a `SynCase` as used in `StmtIf`s and `StmtSwitch`es.
func Case(cond ISyn, thens ...ISyn) *SynCase {
	return &SynCase{Cond: cond, SynBlock: SynBlock{Body: thens}}
}

// Fn constructs a `SynFunc`. If `maybeRecv` has a `Type` set, `this` represents a method of that type.
func Fn(maybeRecv NamedTyped, name string, sig *TypeFunc, body ...ISyn) (this *SynFunc) {
	this = &SynFunc{Recv: maybeRecv}
	this.Body, this.Named.Name, this.Type = body, name, TFunc(sig)
	return
}

// Func constructs a `SynFunc` with the given `name` and `args`.
func Func(name string, args ...NamedTyped) *SynFunc {
	return &SynFunc{NamedTyped: NamedTyped{Named: Named{Name: name}, Type: TFunc(TdFn(args))}}
}

func synFrom(any IAny) ISyn {
	if syn, ok := any.(ISyn); ok {
		return syn
	}
	return ExprLit{Val: any}
}

func synsFrom(preferNamesOverStringLits bool, things ...IAny) Syns {
	if preferNamesOverStringLits {
		for i := range things {
			switch s := things[i].(type) {
			case string:
				things[i] = N(s)
			}
		}
	}
	return SynsFrom(nil, things...)
}

func SynsFrom(eitherSyn ISyn, orThings ...IAny) Syns {
	if eitherSyn == nil {
		var syns Syns
		if len(orThings) > 0 {
			syns = make(Syns, len(orThings))
			for i, any := range orThings {
				if syn, ok := any.(ISyn); ok {
					syns[i] = syn
				} else {
					syns[i] = ExprLit{Val: any}
				}
			}
		}
		return syns
	}
	if block, ok := eitherSyn.(SynBlock); ok {
		return block.Body
	}
	if syns, ok := eitherSyn.(Syns); ok {
		return syns
	}
	return Syns{eitherSyn}
}
