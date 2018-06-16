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

func N(name string) *Named      { return &Named{Name: name} }
func L(lit interface{}) ExprLit { return ExprLit{Val: lit} }

func Add(operands ...IEmit) *OpAdd     { return &OpAdd{Op: Op{Operands: operands}} }
func Addr(operands ...IEmit) *OpAddr   { return &OpAddr{Op: Op{Operands: operands}} }
func And(operands ...IEmit) *OpAnd     { return &OpAnd{Op: Op{Operands: operands}} }
func C(operands ...IEmit) *OpComma     { return &OpComma{Op: Op{Operands: operands}} }
func D(operands ...IEmit) *OpDot       { return &OpDot{Op: Op{Operands: operands}} }
func Decl(operands ...IEmit) *OpDecl   { return &OpDecl{Op: Op{Operands: operands}} }
func Deref(operands ...IEmit) *OpDeref { return &OpDeref{Op: Op{Operands: operands}} }
func Div(operands ...IEmit) *OpDiv     { return &OpDiv{Op: Op{Operands: operands}} }
func Eq(operands ...IEmit) *OpEq       { return &OpEq{Op: Op{Operands: operands}} }
func Geq(operands ...IEmit) *OpGeq     { return &OpGeq{Op: Op{Operands: operands}} }
func Gt(operands ...IEmit) *OpGt       { return &OpGt{Op: Op{Operands: operands}} }
func I(operands ...IEmit) *OpIdx       { return &OpIdx{Op: Op{Operands: operands}} }
func Leq(operands ...IEmit) *OpLeq     { return &OpLeq{Op: Op{Operands: operands}} }
func Lt(operands ...IEmit) *OpLt       { return &OpLt{Op: Op{Operands: operands}} }
func Mul(operands ...IEmit) *OpMul     { return &OpMul{Op: Op{Operands: operands}} }
func Neg(operand IEmit) *OpSub         { return &OpSub{Op: Op{Operands: []IEmit{operand}}} }
func Neq(operands ...IEmit) *OpNeq     { return &OpNeq{Op: Op{Operands: operands}} }
func Not(operands ...IEmit) *OpNot     { return &OpNot{Op: Op{Operands: operands}} }
func Or(operands ...IEmit) *OpOr       { return &OpOr{Op: Op{Operands: operands}} }
func Set(operands ...IEmit) *OpSet     { return &OpSet{Op: Op{Operands: operands}} }
func Sub(operands ...IEmit) *OpSub     { return &OpSub{Op: Op{Operands: operands}} }

func TDef(name string, typeRef *TypeRef, isAlias bool) *TypeDef {
	tdef := &TypeDef{IsAlias: isAlias}
	tdef.Name, tdef.Type = name, typeRef
	return tdef
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
func TrN(pkgName string, typeName string) *TypeRef {
	tref := &TypeRef{}
	tref.ToNamed.PkgName, tref.ToNamed.TypeName = pkgName, typeName
	return tref
}
func TrMap(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToMapOf.Key, tref.ToMapOf.Val = keyType, valType
	return tref
}
func TrpBool(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Bool = true
	return tref
}
func TrpByte(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Byte = true
	return tref
}
func TrpC128(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Complex128 = true
	return tref
}
func TrpC64(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Complex64 = true
	return tref
}
func TrpF32(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Float32 = true
	return tref
}
func TrpF64(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Float64 = true
	return tref
}
func TrpInt(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Int = true
	return tref
}
func TrpI16(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Int16 = true
	return tref
}
func TrpI32(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Int32 = true
	return tref
}
func TrpI64(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Int64 = true
	return tref
}
func TrpI8(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Int8 = true
	return tref
}
func TrpRune(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Rune = true
	return tref
}
func TrpStr(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.String = true
	return tref
}
func TrpUint(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Uint = true
	return tref
}
func TrpUi16(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Uint16 = true
	return tref
}
func TrpUi32(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Uint32 = true
	return tref
}
func TrpUi64(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Uint64 = true
	return tref
}
func TrpUi8(keyType *TypeRef, valType *TypeRef) *TypeRef {
	tref := &TypeRef{}
	tref.ToPrim.Uint8 = true
	return tref
}

func Block(body ...IEmit) *SynBlock {
	return &SynBlock{Body: body}
}

func Call(callee IEmit, args ...IEmit) *ExprCall {
	return &ExprCall{Callee: callee, Args: args}
}

func Const(name string, maybeType *TypeRef, exprLit ExprLit) *StmtConst {
	this := &StmtConst{Expr: exprLit}
	this.Name, this.Type = name, maybeType
	return this
}

func Defer(call *ExprCall) *StmtDefer {
	return &StmtDefer{StmtUnary: StmtUnary{Expr: call}}
}

func File(pkgName string, decls ...IEmit) *SynFile {
	return &SynFile{PkgName: pkgName, SynBlock: SynBlock{Body: decls}}
}

func ForLoop(maybeInit IEmit, maybeCond IEmit, maybeEach IEmit, body ...IEmit) *StmtFor {
	this := &StmtFor{}
	this.Body, this.Loop.Init, this.Loop.Cond, this.Loop.Each = body, maybeInit, maybeCond, maybeEach
	return this
}

func ForRange(maybeIdx *Named, maybeVal *Named, iteree IEmit, body ...IEmit) *StmtFor {
	this := &StmtFor{}
	this.Body, this.Range.Idx, this.Range.Val, this.Range.Iteree = body, maybeIdx, maybeVal, iteree
	return this
}

func Func(maybeRecv *NamedTyped, name string, sig *TypeRef, body ...IEmit) *SynFunc {
	this := &SynFunc{}
	this.Recv, this.Body, this.Name, this.Type = maybeRecv, body, name, sig
	return this
}

func Go(call *ExprCall) *StmtGo {
	return &StmtGo{StmtUnary: StmtUnary{Expr: call}}
}

func If(cond IEmit, thens ...IEmit) *StmtIf {
	return &StmtIf{IfThens: []SynCond{{Cond: cond, SynBlock: SynBlock{Body: thens}}}}
}

func Ifs(ifThensAndMaybeAnElse ...IEmit) *StmtIf {
	this := &StmtIf{}
	if l := len(ifThensAndMaybeAnElse); l%2 != 0 {
		if block, _ := ifThensAndMaybeAnElse[l-1].(*SynBlock); block != nil {
			this.Else.Body = block.Body
		}
		ifThensAndMaybeAnElse = ifThensAndMaybeAnElse[:l-1]
	}
	for i := 1; i < len(ifThensAndMaybeAnElse); i += 2 {
		var body []IEmit
		if block, _ := ifThensAndMaybeAnElse[i].(*SynBlock); block != nil {
			body = block.Body
		}
		this.IfThens = append(this.IfThens, SynCond{Cond: ifThensAndMaybeAnElse[i-1], SynBlock: SynBlock{Body: body}})
	}
	return this
}

func Ret(retExpr IEmit) *StmtRet {
	return &StmtRet{StmtUnary: StmtUnary{Expr: retExpr}}
}

func Switch(maybeCond IEmit, caseCondsAndBlocksPlusMaybeDefaultBlock ...IEmit) *StmtSwitch {
	this := &StmtSwitch{Cond: maybeCond}
	if l := len(caseCondsAndBlocksPlusMaybeDefaultBlock); l%2 != 0 {
		if block, _ := caseCondsAndBlocksPlusMaybeDefaultBlock[l-1].(*SynBlock); block != nil {
			this.Default.Body = block.Body
		}
		caseCondsAndBlocksPlusMaybeDefaultBlock = caseCondsAndBlocksPlusMaybeDefaultBlock[:l-1]
	}
	for i := 1; i < len(caseCondsAndBlocksPlusMaybeDefaultBlock); i += 2 {
		var body []IEmit
		if block, _ := caseCondsAndBlocksPlusMaybeDefaultBlock[i].(*SynBlock); block != nil {
			body = block.Body
		}
		this.Cases = append(this.Cases, SynCond{Cond: caseCondsAndBlocksPlusMaybeDefaultBlock[i-1], SynBlock: SynBlock{Body: body}})
	}
	return this
}

func Var(name string, maybeType *TypeRef, maybeExpr IEmit) *StmtVar {
	this := &StmtVar{Expr: maybeExpr}
	this.Name, this.Type = name, maybeType
	return this
}
