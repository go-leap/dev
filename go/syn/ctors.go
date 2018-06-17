package udevgosyn

var (
	// keyword singletons
	K struct {
		Break    StmtBreak
		Continue StmtContinue
		Ret      StmtRet
	}

	// built-ins
	B struct {
		Nil     ExprNil
		Append  Named
		Cap     Named
		Close   Named
		Complex Named
		Copy    Named
		Delete  Named
		Imag    Named
		Len     Named
		Make    Named
		New     Named
		Panic   Named
		Print   Named
		Println Named
		Real    Named
		Recover Named
	}

	// common vars
	V struct {
		Err  Named
		Ret  Named
		This Named
	}

	// common type-refs
	T struct {
		Bool *TypeRef
	}
)

func init() {
	V.Err.Name, V.Ret.Name, V.This.Name = "err", "ret", "this"
	B.Append.Name, B.Cap.Name, B.Close.Name, B.Complex.Name, B.Copy.Name, B.Delete.Name, B.Imag.Name, B.Len.Name, B.Make.Name, B.New.Name, B.Panic.Name, B.Print.Name, B.Println.Name, B.Real.Name, B.Recover.Name = "append", "cap", "close", "complex", "copy", "delete", "imag", "len", "make", "new", "panic", "print", "println", "real", "recover"
	T.Bool = TrpBool()
}

func A(argsOrOperandsOrStmts ...ISyn) []ISyn { return argsOrOperandsOrStmts }
func N(name string) Named                    { return Named{Name: name} }
func Nt(name string, t *TypeRef) NamedTyped  { return NamedTyped{Type: t, Named: Named{Name: name}} }
func L(lit interface{}) ExprLit              { return ExprLit{Val: lit} }

func Add(operands ...ISyn) OpAdd     { return OpAdd{Op: Op{Operands: operands}} }
func Addr(operands ...ISyn) OpAddr   { return OpAddr{Op: Op{Operands: operands}} }
func And(operands ...ISyn) OpAnd     { return OpAnd{Op: Op{Operands: operands}} }
func C(operands ...ISyn) OpComma     { return OpComma{Op: Op{Operands: operands}} }
func D(operands ...ISyn) OpDot       { return OpDot{Op: Op{Operands: operands}} }
func Decl(operands ...ISyn) OpDecl   { return OpDecl{Op: Op{Operands: operands}} }
func Deref(operands ...ISyn) OpDeref { return OpDeref{Op: Op{Operands: operands}} }
func Div(operands ...ISyn) OpDiv     { return OpDiv{Op: Op{Operands: operands}} }
func Eq(operands ...ISyn) OpEq       { return OpEq{Op: Op{Operands: operands}} }
func Geq(operands ...ISyn) OpGeq     { return OpGeq{Op: Op{Operands: operands}} }
func Gt(operands ...ISyn) OpGt       { return OpGt{Op: Op{Operands: operands}} }
func I(operands ...ISyn) OpIdx       { return OpIdx{Op: Op{Operands: operands}} }
func Leq(operands ...ISyn) OpLeq     { return OpLeq{Op: Op{Operands: operands}} }
func Lt(operands ...ISyn) OpLt       { return OpLt{Op: Op{Operands: operands}} }
func Mul(operands ...ISyn) OpMul     { return OpMul{Op: Op{Operands: operands}} }
func Neg(operand ISyn) OpSub         { return OpSub{Op: Op{Operands: []ISyn{operand}}} }
func Neq(operands ...ISyn) OpNeq     { return OpNeq{Op: Op{Operands: operands}} }
func Not(operands ...ISyn) OpNot     { return OpNot{Op: Op{Operands: operands}} }
func Or(operands ...ISyn) OpOr       { return OpOr{Op: Op{Operands: operands}} }
func Set(operands ...ISyn) OpSet     { return OpSet{Op: Op{Operands: operands}} }
func Sub(operands ...ISyn) OpSub     { return OpSub{Op: Op{Operands: operands}} }

func TDecl(name string, typeRef *TypeRef, isAlias bool) (this TypeDecl) {
	this.IsAlias, this.Name, this.Type = isAlias, name, typeRef
	return
}
func TdFunc(args NamedsTypeds, rets ...NamedTyped) *TypeFunc {
	return &TypeFunc{Args: args, Rets: rets}
}
func TdInterface(embeds []TypeRef, methods ...NamedTyped) *TypeInterface {
	return &TypeInterface{Embeds: embeds, Methods: methods}
}
func TdStruct(embeds []TypeRef, fields ...SynStructField) *TypeStruct {
	return &TypeStruct{Embeds: embeds, Fields: fields}
}
func TdStructFld(name string, typeRef *TypeRef, tags map[string]string) (fld SynStructField) {
	fld.Tags, fld.Name, fld.Type = tags, name, typeRef
	return
}

func TrFunc(typeFunc *TypeFunc) *TypeRef            { return &TypeRef{Func: typeFunc} }
func TrInterface(typeIface *TypeInterface) *TypeRef { return &TypeRef{Interface: typeIface} }
func TrStruct(typeStruct *TypeStruct) *TypeRef      { return &TypeRef{Struct: typeStruct} }
func TrPtr(typeRef *TypeRef) *TypeRef               { return &TypeRef{Ptr: typeRef} }
func TrSlice(typeRef *TypeRef) *TypeRef             { return &TypeRef{Slice: typeRef} }
func TrNamed(pkgName string, typeName string) (this *TypeRef) {
	this = &TypeRef{}
	this.Named.PkgName, this.Named.TypeName = pkgName, typeName
	return
}
func TrMap(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.Map.Key, this.Map.Val = keyType, valType
	return
}
func TrpBool() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Bool = true
	return
}
func TrpByte() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Byte = true
	return
}
func TrpC128() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Complex128 = true
	return
}
func TrpC64() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Complex64 = true
	return
}
func TrpF32() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Float32 = true
	return
}
func TrpF64() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Float64 = true
	return
}
func TrpInt() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Int = true
	return
}
func TrpI16() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Int16 = true
	return
}
func TrpI32() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Int32 = true
	return
}
func TrpI64() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Int64 = true
	return
}
func TrpI8() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Int8 = true
	return
}
func TrpRune() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Rune = true
	return
}
func TrpStr() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.String = true
	return
}
func TrpUint() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Uint = true
	return
}
func TrpUi16() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Uint16 = true
	return
}
func TrpUi32() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Uint32 = true
	return
}
func TrpUi64() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Uint64 = true
	return
}
func TrpUi8() (this *TypeRef) {
	this = &TypeRef{}
	this.Prim.Uint8 = true
	return
}

func Block(body ...ISyn) (this SynBlock) {
	this.Body = body
	return
}

func Call(callee ISyn, args ...ISyn) *ExprCall {
	return &ExprCall{Callee: callee, Args: args}
}

func Const(name string, maybeType *TypeRef, exprLit ExprLit) (this *StmtConst) {
	this = &StmtConst{Expr: exprLit}
	this.Name, this.Type = name, maybeType
	return
}

func Defer(call *ExprCall) (this StmtDefer) {
	this.Expr = call
	return
}

func File(pkgName string, topLevelDecls ...ISyn) *SynFile {
	return &SynFile{PkgName: pkgName, SynBlock: SynBlock{Body: topLevelDecls}}
}

func ForLoop(maybeInit ISyn, maybeCond ISyn, maybeEach ISyn, body ...ISyn) (this *StmtFor) {
	this = &StmtFor{}
	this.Body, this.Loop.Init, this.Loop.Cond, this.Loop.Each = body, maybeInit, maybeCond, maybeEach
	return
}

func ForRange(maybeIdx Named, maybeVal Named, iteree ISyn, body ...ISyn) (this *StmtFor) {
	this = &StmtFor{}
	this.Body, this.Range.Idx, this.Range.Val, this.Range.Iteree = body, maybeIdx, maybeVal, iteree
	return
}

func Func(maybeRecv NamedTyped, name string, sig *TypeFunc, body ...ISyn) (this *SynFunc) {
	this = &SynFunc{Recv: maybeRecv}
	this.Body, this.Name, this.Type = body, name, TrFunc(sig)
	return
}

func Go(call *ExprCall) (this StmtGo) {
	this.Expr = call
	return
}

func If(cond ISyn, thens ...ISyn) *StmtIf {
	return &StmtIf{IfThens: []SynCond{{Cond: cond, SynBlock: SynBlock{Body: thens}}}}
}

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

func Ret(retExpr ISyn) (this StmtRet) {
	this.Expr = retExpr
	return
}

func Switch(maybeCond ISyn, caseCondsAndBlocksPlusMaybeDefaultBlock ...ISyn) (this *StmtSwitch) {
	this = &StmtSwitch{Cond: maybeCond}
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

func Var(name string, maybeType *TypeRef, maybeExpr ISyn) (this *StmtVar) {
	this = &StmtVar{Expr: maybeExpr}
	this.Name, this.Type = name, maybeType
	return
}
