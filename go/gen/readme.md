# udevgogen
--
    import "github.com/go-leap/dev/go/gen"


## Usage

```go
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
```

#### type DocCommentSingleLineParagraphs

```go
type DocCommentSingleLineParagraphs []string
```


#### func (*DocCommentSingleLineParagraphs) Add

```go
func (this *DocCommentSingleLineParagraphs) Add(docCommentLines ...string)
```

#### type ExprCall

```go
type ExprCall struct {
	Callee ISyn
	Args   []ISyn
}
```


#### func  Call

```go
func Call(callee ISyn, args ...ISyn) *ExprCall
```

#### func (*ExprCall) Emit

```go
func (this *ExprCall) Emit(w IWriter)
```

#### type ExprLit

```go
type ExprLit struct {
	Val interface{}
}
```


#### func  L

```go
func L(lit interface{}) ExprLit
```

#### func (ExprLit) Emit

```go
func (this ExprLit) Emit(w IWriter)
```

#### type ExprNil

```go
type ExprNil struct {
}
```


#### func (ExprNil) Emit

```go
func (ExprNil) Emit(w IWriter)
```

#### type ISyn

```go
type ISyn interface {
	Emit(IWriter)
}
```

ISyn can be any legal element in the Abstract Syntax Tree: literals, vars,
consts, type-defs, type-refs, funcs, keywords, operators etc..

#### func  A

```go
func A(argsOrOperandsOrStmts ...ISyn) []ISyn
```

#### type IWriter

```go
type IWriter interface {
	ShouldEmitNoOpFuncBodies() bool
	io.ByteWriter
	io.Writer
	// WriteRune(rune) (int, error)
	WriteString(string) (int, error)
}
```


#### type Named

```go
type Named struct{ Name string }
```


#### func  N

```go
func N(name string) Named
```

#### func (Named) Emit

```go
func (this Named) Emit(w IWriter)
```

#### func (Named) Typed

```go
func (this Named) Typed(typeRef *TypeRef) (nt NamedTyped)
```

#### type NamedTyped

```go
type NamedTyped struct {
	Named
	Type *TypeRef
}
```


#### func  Nt

```go
func Nt(name string, t *TypeRef) NamedTyped
```

#### type NamedsTypeds

```go
type NamedsTypeds []NamedTyped
```


#### type Op

```go
type Op struct {
	Operands []ISyn
}
```


#### type OpAdd

```go
type OpAdd struct{ Op }
```


#### func  Add

```go
func Add(operands ...ISyn) OpAdd
```

#### func (OpAdd) Emit

```go
func (this OpAdd) Emit(w IWriter)
```

#### type OpAddr

```go
type OpAddr struct{ Op }
```


#### func  Addr

```go
func Addr(operands ...ISyn) OpAddr
```

#### func (OpAddr) Emit

```go
func (this OpAddr) Emit(w IWriter)
```

#### type OpAnd

```go
type OpAnd struct{ Op }
```


#### func  And

```go
func And(operands ...ISyn) OpAnd
```

#### func (OpAnd) Emit

```go
func (this OpAnd) Emit(w IWriter)
```

#### type OpComma

```go
type OpComma struct{ Op }
```


#### func  C

```go
func C(operands ...ISyn) OpComma
```

#### func (OpComma) Emit

```go
func (this OpComma) Emit(w IWriter)
```

#### type OpDecl

```go
type OpDecl struct{ Op }
```


#### func  Decl

```go
func Decl(operands ...ISyn) OpDecl
```

#### func (OpDecl) Emit

```go
func (this OpDecl) Emit(w IWriter)
```

#### type OpDeref

```go
type OpDeref struct{ Op }
```


#### func  Deref

```go
func Deref(operands ...ISyn) OpDeref
```

#### func (OpDeref) Emit

```go
func (this OpDeref) Emit(w IWriter)
```

#### type OpDiv

```go
type OpDiv struct{ Op }
```


#### func  Div

```go
func Div(operands ...ISyn) OpDiv
```

#### func (OpDiv) Emit

```go
func (this OpDiv) Emit(w IWriter)
```

#### type OpDot

```go
type OpDot struct{ Op }
```


#### func  D

```go
func D(operands ...ISyn) OpDot
```

#### func (OpDot) Emit

```go
func (this OpDot) Emit(w IWriter)
```

#### type OpEq

```go
type OpEq struct{ Op }
```


#### func  Eq

```go
func Eq(operands ...ISyn) OpEq
```

#### func (OpEq) Emit

```go
func (this OpEq) Emit(w IWriter)
```

#### type OpGeq

```go
type OpGeq struct{ Op }
```


#### func  Geq

```go
func Geq(operands ...ISyn) OpGeq
```

#### func (OpGeq) Emit

```go
func (this OpGeq) Emit(w IWriter)
```

#### type OpGt

```go
type OpGt struct{ Op }
```


#### func  Gt

```go
func Gt(operands ...ISyn) OpGt
```

#### func (OpGt) Emit

```go
func (this OpGt) Emit(w IWriter)
```

#### type OpIdx

```go
type OpIdx struct{ Op }
```


#### func  I

```go
func I(operands ...ISyn) OpIdx
```

#### func (OpIdx) Emit

```go
func (this OpIdx) Emit(w IWriter)
```

#### type OpLeq

```go
type OpLeq struct{ Op }
```


#### func  Leq

```go
func Leq(operands ...ISyn) OpLeq
```

#### func (OpLeq) Emit

```go
func (this OpLeq) Emit(w IWriter)
```

#### type OpLt

```go
type OpLt struct{ Op }
```


#### func  Lt

```go
func Lt(operands ...ISyn) OpLt
```

#### func (OpLt) Emit

```go
func (this OpLt) Emit(w IWriter)
```

#### type OpMul

```go
type OpMul struct{ Op }
```


#### func  Mul

```go
func Mul(operands ...ISyn) OpMul
```

#### func (OpMul) Emit

```go
func (this OpMul) Emit(w IWriter)
```

#### type OpNeq

```go
type OpNeq struct{ Op }
```


#### func  Neq

```go
func Neq(operands ...ISyn) OpNeq
```

#### func (OpNeq) Emit

```go
func (this OpNeq) Emit(w IWriter)
```

#### type OpNot

```go
type OpNot struct{ Op }
```


#### func  Not

```go
func Not(operands ...ISyn) OpNot
```

#### func (OpNot) Emit

```go
func (this OpNot) Emit(w IWriter)
```

#### type OpOr

```go
type OpOr struct{ Op }
```


#### func  Or

```go
func Or(operands ...ISyn) OpOr
```

#### func (OpOr) Emit

```go
func (this OpOr) Emit(w IWriter)
```

#### type OpSet

```go
type OpSet struct{ Op }
```


#### func  Set

```go
func Set(operands ...ISyn) OpSet
```

#### func (OpSet) Emit

```go
func (this OpSet) Emit(w IWriter)
```

#### type OpSub

```go
type OpSub struct{ Op }
```


#### func  Neg

```go
func Neg(operand ISyn) OpSub
```

#### func  Sub

```go
func Sub(operands ...ISyn) OpSub
```

#### func (OpSub) Emit

```go
func (this OpSub) Emit(w IWriter)
```

#### type StmtBreak

```go
type StmtBreak struct{}
```


#### func (StmtBreak) Emit

```go
func (StmtBreak) Emit(w IWriter)
```

#### type StmtConst

```go
type StmtConst struct {
	NamedTyped
	Expr ExprLit
}
```


#### func  Const

```go
func Const(name string, maybeType *TypeRef, exprLit ExprLit) (this *StmtConst)
```

#### func (*StmtConst) Emit

```go
func (this *StmtConst) Emit(w IWriter)
```

#### type StmtContinue

```go
type StmtContinue struct{}
```


#### func (StmtContinue) Emit

```go
func (StmtContinue) Emit(w IWriter)
```

#### type StmtDefer

```go
type StmtDefer struct {
	StmtUnary
}
```


#### func  Defer

```go
func Defer(call *ExprCall) (this StmtDefer)
```

#### func (StmtDefer) Emit

```go
func (this StmtDefer) Emit(w IWriter)
```

#### type StmtFor

```go
type StmtFor struct {
	SynBlock
	Range struct {
		Idx    Named
		Val    Named
		Iteree ISyn
	}
	Loop struct {
		Init ISyn
		Cond ISyn
		Each ISyn
	}
}
```


#### func  ForLoop

```go
func ForLoop(maybeInit ISyn, maybeCond ISyn, maybeEach ISyn, body ...ISyn) (this *StmtFor)
```

#### func  ForRange

```go
func ForRange(maybeIdx Named, maybeVal Named, iteree ISyn, body ...ISyn) (this *StmtFor)
```

#### func (*StmtFor) Emit

```go
func (this *StmtFor) Emit(w IWriter)
```

#### type StmtGo

```go
type StmtGo struct {
	StmtUnary
}
```


#### func  Go

```go
func Go(call *ExprCall) (this StmtGo)
```

#### func (StmtGo) Emit

```go
func (this StmtGo) Emit(w IWriter)
```

#### type StmtIf

```go
type StmtIf struct {
	IfThens []SynCond
	Else    SynBlock
}
```


#### func  If

```go
func If(cond ISyn, thens ...ISyn) *StmtIf
```

#### func  Ifs

```go
func Ifs(ifThensAndMaybeAnElse ...ISyn) (this *StmtIf)
```

#### func (*StmtIf) Emit

```go
func (this *StmtIf) Emit(w IWriter)
```

#### type StmtRet

```go
type StmtRet struct {
	StmtUnary
}
```


#### func  Ret

```go
func Ret(retExpr ISyn) (this StmtRet)
```

#### func (StmtRet) Emit

```go
func (this StmtRet) Emit(w IWriter)
```

#### type StmtSwitch

```go
type StmtSwitch struct {
	Cond    ISyn
	Cases   []SynCond
	Default SynBlock
}
```


#### func  Switch

```go
func Switch(maybeCond ISyn, caseCondsAndBlocksPlusMaybeDefaultBlock ...ISyn) (this *StmtSwitch)
```

#### func (*StmtSwitch) Emit

```go
func (this *StmtSwitch) Emit(w IWriter)
```

#### type StmtUnary

```go
type StmtUnary struct {
	Expr ISyn
}
```


#### type StmtVar

```go
type StmtVar struct {
	NamedTyped
	Expr ISyn
}
```


#### func  Var

```go
func Var(name string, maybeType *TypeRef, maybeExpr ISyn) (this *StmtVar)
```

#### func (*StmtVar) Emit

```go
func (this *StmtVar) Emit(w IWriter)
```

#### type SynBlock

```go
type SynBlock struct {
	Body []ISyn
}
```


#### func  Block

```go
func Block(body ...ISyn) (this SynBlock)
```

#### func (*SynBlock) Add

```go
func (this *SynBlock) Add(stmts ...ISyn)
```

#### func (SynBlock) Emit

```go
func (this SynBlock) Emit(w IWriter)
```

#### type SynCond

```go
type SynCond struct {
	Cond ISyn
	SynBlock
}
```


#### type SynFile

```go
type SynFile struct {
	PkgName string
	SynBlock
}
```


#### func  File

```go
func File(pkgName string, topLevelDecls ...ISyn) *SynFile
```

#### func (*SynFile) Emit

```go
func (this *SynFile) Emit(w IWriter, codeGenCommentNotice string)
```

#### func (*SynFile) I

```go
func (this *SynFile) I(pkgImportPath string) (pkgName string)
```

#### func (*SynFile) Src

```go
func (this *SynFile) Src(codeGenCommentNotice string, emitNoOpFuncBodies bool) (src []byte, err error)
```

#### type SynFunc

```go
type SynFunc struct {
	SynBlock
	NamedTyped
	Recv NamedTyped
	Doc  DocCommentSingleLineParagraphs
}
```


#### func  Fn

```go
func Fn(maybeRecv NamedTyped, name string, sig *TypeFunc, body ...ISyn) (this *SynFunc)
```

#### func (*SynFunc) Emit

```go
func (this *SynFunc) Emit(w IWriter)
```

#### type SynStructField

```go
type SynStructField struct {
	NamedTyped
	Tags map[string]string
}
```


#### func  TdStructFld

```go
func TdStructFld(name string, typeRef *TypeRef, tags map[string]string) (fld SynStructField)
```

#### func (*SynStructField) Emit

```go
func (this *SynStructField) Emit(w IWriter)
```

#### type TypeDecl

```go
type TypeDecl struct {
	NamedTyped
	IsAlias bool
}
```


#### func  TDecl

```go
func TDecl(name string, typeRef *TypeRef, isAlias bool) (this TypeDecl)
```

#### func (TypeDecl) Emit

```go
func (this TypeDecl) Emit(w IWriter)
```

#### type TypeFunc

```go
type TypeFunc struct {
	Args NamedsTypeds
	Rets NamedsTypeds
}
```


#### func  TdFunc

```go
func TdFunc(args NamedsTypeds, rets ...NamedTyped) *TypeFunc
```

#### func (*TypeFunc) Emit

```go
func (this *TypeFunc) Emit(w IWriter)
```

#### type TypeInterface

```go
type TypeInterface struct {
	Embeds  []TypeRef
	Methods NamedsTypeds
}
```


#### func  TdInterface

```go
func TdInterface(embeds []TypeRef, methods ...NamedTyped) *TypeInterface
```

#### func (*TypeInterface) Emit

```go
func (this *TypeInterface) Emit(w IWriter)
```

#### type TypeRef

```go
type TypeRef struct {
	Slice *TypeRef
	Ptr   *TypeRef
	Map   struct {
		Key *TypeRef
		Val *TypeRef
	}
	Func      *TypeFunc
	Interface *TypeInterface
	Struct    *TypeStruct
	Named     struct {
		PkgName  string
		TypeName string
	}
	Prim struct {
		Bool       bool
		Byte       bool
		Complex64  bool
		Complex128 bool
		Float32    bool
		Float64    bool
		Int8       bool
		Int16      bool
		Int32      bool
		Int64      bool
		Int        bool
		Uint       bool
		Uint8      bool
		Uint16     bool
		Uint32     bool
		Uint64     bool
		Rune       bool
		String     bool
	}
}
```


#### func  TrFunc

```go
func TrFunc(typeFunc *TypeFunc) *TypeRef
```

#### func  TrInterface

```go
func TrInterface(typeIface *TypeInterface) *TypeRef
```

#### func  TrMap

```go
func TrMap(keyType *TypeRef, valType *TypeRef) (this *TypeRef)
```

#### func  TrNamed

```go
func TrNamed(pkgName string, typeName string) (this *TypeRef)
```

#### func  TrPtr

```go
func TrPtr(typeRef *TypeRef) *TypeRef
```

#### func  TrSlice

```go
func TrSlice(typeRef *TypeRef) *TypeRef
```

#### func  TrStruct

```go
func TrStruct(typeStruct *TypeStruct) *TypeRef
```

#### func  TrpBool

```go
func TrpBool() (this *TypeRef)
```

#### func  TrpByte

```go
func TrpByte() (this *TypeRef)
```

#### func  TrpC128

```go
func TrpC128() (this *TypeRef)
```

#### func  TrpC64

```go
func TrpC64() (this *TypeRef)
```

#### func  TrpF32

```go
func TrpF32() (this *TypeRef)
```

#### func  TrpF64

```go
func TrpF64() (this *TypeRef)
```

#### func  TrpI16

```go
func TrpI16() (this *TypeRef)
```

#### func  TrpI32

```go
func TrpI32() (this *TypeRef)
```

#### func  TrpI64

```go
func TrpI64() (this *TypeRef)
```

#### func  TrpI8

```go
func TrpI8() (this *TypeRef)
```

#### func  TrpInt

```go
func TrpInt() (this *TypeRef)
```

#### func  TrpRune

```go
func TrpRune() (this *TypeRef)
```

#### func  TrpStr

```go
func TrpStr() (this *TypeRef)
```

#### func  TrpUi16

```go
func TrpUi16() (this *TypeRef)
```

#### func  TrpUi32

```go
func TrpUi32() (this *TypeRef)
```

#### func  TrpUi64

```go
func TrpUi64() (this *TypeRef)
```

#### func  TrpUi8

```go
func TrpUi8() (this *TypeRef)
```

#### func  TrpUint

```go
func TrpUint() (this *TypeRef)
```

#### func (*TypeRef) Emit

```go
func (this *TypeRef) Emit(w IWriter)
```

#### type TypeStruct

```go
type TypeStruct struct {
	Embeds []TypeRef
	Fields []SynStructField
}
```


#### func  TdStruct

```go
func TdStruct(embeds []TypeRef, fields ...SynStructField) *TypeStruct
```

#### func (*TypeStruct) Emit

```go
func (this *TypeStruct) Emit(w IWriter)
```
