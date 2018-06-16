package udevgosyn

var (
	Break    = new(StmtBreak)
	Continue = new(StmtContinue)
	Nil      = new(ExprNil)
	B        struct {
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
	V struct {
		Err  Named
		Ret  Named
		This Named
	}
)

func init() {
	V.Err.Name, V.Ret.Name, V.This.Name = "err", "ret", "this"
	B.Append.Name, B.Cap.Name, B.Close.Name, B.Complex.Name, B.Copy.Name, B.Delete.Name, B.Imag.Name, B.Len.Name, B.Make.Name, B.New.Name, B.Panic.Name, B.Print.Name, B.Println.Name, B.Real.Name, B.Recover.Name = "append", "cap", "close", "complex", "copy", "delete", "imag", "len", "make", "new", "panic", "print", "println", "real", "recover"
}

func N(name string) Named       { return Named{Name: name} }
func L(lit interface{}) ExprLit { return ExprLit{Val: lit} }

func Add(operands ...IEmit) OpAdd     { return OpAdd{Op: Op{Operands: operands}} }
func Addr(operands ...IEmit) OpAddr   { return OpAddr{Op: Op{Operands: operands}} }
func And(operands ...IEmit) OpAnd     { return OpAnd{Op: Op{Operands: operands}} }
func C(operands ...IEmit) OpComma     { return OpComma{Op: Op{Operands: operands}} }
func D(operands ...IEmit) OpDot       { return OpDot{Op: Op{Operands: operands}} }
func Decl(operands ...IEmit) OpDecl   { return OpDecl{Op: Op{Operands: operands}} }
func Deref(operands ...IEmit) OpDeref { return OpDeref{Op: Op{Operands: operands}} }
func Div(operands ...IEmit) OpDiv     { return OpDiv{Op: Op{Operands: operands}} }
func Eq(operands ...IEmit) OpEq       { return OpEq{Op: Op{Operands: operands}} }
func Geq(operands ...IEmit) OpGeq     { return OpGeq{Op: Op{Operands: operands}} }
func Gt(operands ...IEmit) OpGt       { return OpGt{Op: Op{Operands: operands}} }
func I(operands ...IEmit) OpIdx       { return OpIdx{Op: Op{Operands: operands}} }
func Leq(operands ...IEmit) OpLeq     { return OpLeq{Op: Op{Operands: operands}} }
func Lt(operands ...IEmit) OpLt       { return OpLt{Op: Op{Operands: operands}} }
func Mul(operands ...IEmit) OpMul     { return OpMul{Op: Op{Operands: operands}} }
func Neg(operand IEmit) OpSub         { return OpSub{Op: Op{Operands: []IEmit{operand}}} }
func Neq(operands ...IEmit) OpNeq     { return OpNeq{Op: Op{Operands: operands}} }
func Not(operands ...IEmit) OpNot     { return OpNot{Op: Op{Operands: operands}} }
func Or(operands ...IEmit) OpOr       { return OpOr{Op: Op{Operands: operands}} }
func Set(operands ...IEmit) OpSet     { return OpSet{Op: Op{Operands: operands}} }
func Sub(operands ...IEmit) OpSub     { return OpSub{Op: Op{Operands: operands}} }

func TDecl(name string, typeRef *TypeRef, isAlias bool) (this TypeDecl) {
	this.IsAlias, this.Name, this.Type = isAlias, name, typeRef
	return
}
func TFunc(args NamedsTypeds, rets ...NamedTyped) *TypeFunc {
	return &TypeFunc{Args: args, Rets: rets}
}
func TInterface(embeds []TypeRef, methods ...NamedTyped) *TypeInterface {
	return &TypeInterface{Embeds: embeds, Methods: methods}
}
func TStruct(embeds []TypeRef, fields ...SynStructField) *TypeStruct {
	return &TypeStruct{Embeds: embeds, Fields: fields}
}
func TStructFld(name string, typeRef *TypeRef, tags map[string]string) (fld SynStructField) {
	fld.Tags, fld.Name, fld.Type = tags, name, typeRef
	return
}

func TrFunc(typeFunc *TypeFunc) *TypeRef        { return &TypeRef{ToFunc: typeFunc} }
func TrIface(typeIface *TypeInterface) *TypeRef { return &TypeRef{ToInterface: typeIface} }
func TrStruct(typeStruct *TypeStruct) *TypeRef  { return &TypeRef{ToStruct: typeStruct} }
func TrPtr(typeRef *TypeRef) *TypeRef           { return &TypeRef{ToPtrOf: typeRef} }
func TrSl(typeRef *TypeRef) *TypeRef            { return &TypeRef{ToSliceOf: typeRef} }
func TrN(pkgName string, typeName string) (this *TypeRef) {
	this = &TypeRef{}
	this.ToNamed.PkgName, this.ToNamed.TypeName = pkgName, typeName
	return
}
func TrMap(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToMapOf.Key, this.ToMapOf.Val = keyType, valType
	return
}
func TrpBool(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Bool = true
	return
}
func TrpByte(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Byte = true
	return
}
func TrpC128(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Complex128 = true
	return
}
func TrpC64(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Complex64 = true
	return
}
func TrpF32(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Float32 = true
	return
}
func TrpF64(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Float64 = true
	return
}
func TrpInt(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Int = true
	return
}
func TrpI16(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Int16 = true
	return
}
func TrpI32(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Int32 = true
	return
}
func TrpI64(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Int64 = true
	return
}
func TrpI8(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Int8 = true
	return
}
func TrpRune(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Rune = true
	return
}
func TrpStr(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.String = true
	return
}
func TrpUint(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Uint = true
	return
}
func TrpUi16(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Uint16 = true
	return
}
func TrpUi32(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Uint32 = true
	return
}
func TrpUi64(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Uint64 = true
	return
}
func TrpUi8(keyType *TypeRef, valType *TypeRef) (this *TypeRef) {
	this = &TypeRef{}
	this.ToPrim.Uint8 = true
	return
}

func Block(body ...IEmit) (this SynBlock) {
	this.Body = body
	return
}

func Call(callee IEmit, args ...IEmit) *ExprCall {
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

func File(pkgName string, topLevelDecls ...IEmit) *SynFile {
	return &SynFile{PkgName: pkgName, SynBlock: SynBlock{Body: topLevelDecls}}
}

func ForLoop(maybeInit IEmit, maybeCond IEmit, maybeEach IEmit, body ...IEmit) (this *StmtFor) {
	this = &StmtFor{}
	this.Body, this.Loop.Init, this.Loop.Cond, this.Loop.Each = body, maybeInit, maybeCond, maybeEach
	return
}

func ForRange(maybeIdx Named, maybeVal Named, iteree IEmit, body ...IEmit) (this *StmtFor) {
	this = &StmtFor{}
	this.Body, this.Range.Idx, this.Range.Val, this.Range.Iteree = body, maybeIdx, maybeVal, iteree
	return
}

func Func(maybeRecv NamedTyped, name string, sig *TypeRef, body ...IEmit) (this *SynFunc) {
	this = &SynFunc{Recv: maybeRecv}
	this.Body, this.Name, this.Type = body, name, sig
	return
}

func Go(call *ExprCall) (this StmtGo) {
	this.Expr = call
	return
}

func If(cond IEmit, thens ...IEmit) *StmtIf {
	return &StmtIf{IfThens: []SynCond{{Cond: cond, SynBlock: SynBlock{Body: thens}}}}
}

func Ifs(ifThensAndMaybeAnElse ...IEmit) (this *StmtIf) {
	this = &StmtIf{}
	if l := len(ifThensAndMaybeAnElse); l%2 != 0 {
		if block, ok := ifThensAndMaybeAnElse[l-1].(SynBlock); ok {
			this.Else.Body = block.Body
		}
		ifThensAndMaybeAnElse = ifThensAndMaybeAnElse[:l-1]
	}
	for i := 1; i < len(ifThensAndMaybeAnElse); i += 2 {
		var body []IEmit
		if block, ok := ifThensAndMaybeAnElse[i].(SynBlock); ok {
			body = block.Body
		}
		this.IfThens = append(this.IfThens, SynCond{Cond: ifThensAndMaybeAnElse[i-1], SynBlock: SynBlock{Body: body}})
	}
	return
}

func Ret(retExpr IEmit) (this StmtRet) {
	this.Expr = retExpr
	return
}

func Switch(maybeCond IEmit, caseCondsAndBlocksPlusMaybeDefaultBlock ...IEmit) (this *StmtSwitch) {
	this = &StmtSwitch{Cond: maybeCond}
	if l := len(caseCondsAndBlocksPlusMaybeDefaultBlock); l%2 != 0 {
		if block, ok := caseCondsAndBlocksPlusMaybeDefaultBlock[l-1].(SynBlock); ok {
			this.Default.Body = block.Body
		}
		caseCondsAndBlocksPlusMaybeDefaultBlock = caseCondsAndBlocksPlusMaybeDefaultBlock[:l-1]
	}
	for i := 1; i < len(caseCondsAndBlocksPlusMaybeDefaultBlock); i += 2 {
		var body []IEmit
		if block, ok := caseCondsAndBlocksPlusMaybeDefaultBlock[i].(SynBlock); ok {
			body = block.Body
		}
		this.Cases = append(this.Cases, SynCond{Cond: caseCondsAndBlocksPlusMaybeDefaultBlock[i-1], SynBlock: SynBlock{Body: body}})
	}
	return
}

func Var(name string, maybeType *TypeRef, maybeExpr IEmit) (this *StmtVar) {
	this = &StmtVar{Expr: maybeExpr}
	this.Name, this.Type = name, maybeType
	return
}