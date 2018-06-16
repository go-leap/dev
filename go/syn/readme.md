# udevgosyn
--
    import "github.com/go-leap/dev/go/syn"


## Usage

```go
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
```

#### type ExprCall

```go
type ExprCall struct {
	Callee IEmit
	Args   []IEmit
}
```


#### func  Call

```go
func Call(callee IEmit, args ...IEmit) *ExprCall
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

#### func (*ExprLit) Emit

```go
func (this *ExprLit) Emit(w IWriter)
```

#### type ExprNil

```go
type ExprNil struct {
}
```


#### func (*ExprNil) Emit

```go
func (this *ExprNil) Emit(w IWriter)
```

#### type IEmit

```go
type IEmit interface {
	Emit(IWriter)
}
```


#### type IWriter

```go
type IWriter interface {
	io.ByteWriter
	io.Writer
	WriteRune(rune) (int, error)
	WriteString(string) (int, error)
}
```


#### type Named

```go
type Named struct{ Name string }
```


#### func  N

```go
func N(name string) *Named
```

#### func (*Named) Emit

```go
func (this *Named) Emit(w IWriter)
```

#### type NamedTyped

```go
type NamedTyped struct {
	Named
	Type *TypeRef
}
```


#### type NamedsTypeds

```go
type NamedsTypeds []NamedTyped
```


#### type Op

```go
type Op struct {
	Operands []IEmit
}
```


#### type OpAdd

```go
type OpAdd struct{ Op }
```


#### func  Add

```go
func Add(operands ...IEmit) *OpAdd
```

#### func (*OpAdd) Emit

```go
func (this *OpAdd) Emit(w IWriter)
```

#### type OpAddr

```go
type OpAddr struct{ Op }
```


#### func  Addr

```go
func Addr(operands ...IEmit) *OpAddr
```

#### func (*OpAddr) Emit

```go
func (this *OpAddr) Emit(w IWriter)
```

#### type OpAnd

```go
type OpAnd struct{ Op }
```


#### func  And

```go
func And(operands ...IEmit) *OpAnd
```

#### func (*OpAnd) Emit

```go
func (this *OpAnd) Emit(w IWriter)
```

#### type OpComma

```go
type OpComma struct{ Op }
```


#### func  C

```go
func C(operands ...IEmit) *OpComma
```

#### func (*OpComma) Emit

```go
func (this *OpComma) Emit(w IWriter)
```

#### type OpDecl

```go
type OpDecl struct{ Op }
```


#### func  Decl

```go
func Decl(operands ...IEmit) *OpDecl
```

#### func (*OpDecl) Emit

```go
func (this *OpDecl) Emit(w IWriter)
```

#### type OpDeref

```go
type OpDeref struct{ Op }
```


#### func  Deref

```go
func Deref(operands ...IEmit) *OpDeref
```

#### func (*OpDeref) Emit

```go
func (this *OpDeref) Emit(w IWriter)
```

#### type OpDiv

```go
type OpDiv struct{ Op }
```


#### func  Div

```go
func Div(operands ...IEmit) *OpDiv
```

#### func (*OpDiv) Emit

```go
func (this *OpDiv) Emit(w IWriter)
```

#### type OpDot

```go
type OpDot struct{ Op }
```


#### func  D

```go
func D(operands ...IEmit) *OpDot
```

#### func (*OpDot) Emit

```go
func (this *OpDot) Emit(w IWriter)
```

#### type OpEq

```go
type OpEq struct{ Op }
```


#### func  Eq

```go
func Eq(operands ...IEmit) *OpEq
```

#### func (*OpEq) Emit

```go
func (this *OpEq) Emit(w IWriter)
```

#### type OpGeq

```go
type OpGeq struct{ Op }
```


#### func  Geq

```go
func Geq(operands ...IEmit) *OpGeq
```

#### func (*OpGeq) Emit

```go
func (this *OpGeq) Emit(w IWriter)
```

#### type OpGt

```go
type OpGt struct{ Op }
```


#### func  Gt

```go
func Gt(operands ...IEmit) *OpGt
```

#### func (*OpGt) Emit

```go
func (this *OpGt) Emit(w IWriter)
```

#### type OpIdx

```go
type OpIdx struct{ Op }
```


#### func  I

```go
func I(operands ...IEmit) *OpIdx
```

#### func (*OpIdx) Emit

```go
func (this *OpIdx) Emit(w IWriter)
```

#### type OpLeq

```go
type OpLeq struct{ Op }
```


#### func  Leq

```go
func Leq(operands ...IEmit) *OpLeq
```

#### func (*OpLeq) Emit

```go
func (this *OpLeq) Emit(w IWriter)
```

#### type OpLt

```go
type OpLt struct{ Op }
```


#### func  Lt

```go
func Lt(operands ...IEmit) *OpLt
```

#### func (*OpLt) Emit

```go
func (this *OpLt) Emit(w IWriter)
```

#### type OpMul

```go
type OpMul struct{ Op }
```


#### func  Mul

```go
func Mul(operands ...IEmit) *OpMul
```

#### func (*OpMul) Emit

```go
func (this *OpMul) Emit(w IWriter)
```

#### type OpNeq

```go
type OpNeq struct{ Op }
```


#### func  Neq

```go
func Neq(operands ...IEmit) *OpNeq
```

#### func (*OpNeq) Emit

```go
func (this *OpNeq) Emit(w IWriter)
```

#### type OpNot

```go
type OpNot struct{ Op }
```


#### func  Not

```go
func Not(operands ...IEmit) *OpNot
```

#### func (*OpNot) Emit

```go
func (this *OpNot) Emit(w IWriter)
```

#### type OpOr

```go
type OpOr struct{ Op }
```


#### func  Or

```go
func Or(operands ...IEmit) *OpOr
```

#### func (*OpOr) Emit

```go
func (this *OpOr) Emit(w IWriter)
```

#### type OpSet

```go
type OpSet struct{ Op }
```


#### func  Set

```go
func Set(operands ...IEmit) *OpSet
```

#### func (*OpSet) Emit

```go
func (this *OpSet) Emit(w IWriter)
```

#### type OpSub

```go
type OpSub struct{ Op }
```


#### func  Neg

```go
func Neg(operand IEmit) *OpSub
```

#### func  Sub

```go
func Sub(operands ...IEmit) *OpSub
```

#### func (*OpSub) Emit

```go
func (this *OpSub) Emit(w IWriter)
```

#### type StmtBreak

```go
type StmtBreak struct{}
```


#### func (*StmtBreak) Emit

```go
func (this *StmtBreak) Emit(w IWriter)
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
func Const(name string, maybeType *TypeRef, exprLit ExprLit) *StmtConst
```

#### func (*StmtConst) Emit

```go
func (this *StmtConst) Emit(w IWriter)
```

#### type StmtContinue

```go
type StmtContinue struct{}
```


#### func (*StmtContinue) Emit

```go
func (this *StmtContinue) Emit(w IWriter)
```

#### type StmtDefer

```go
type StmtDefer struct {
	StmtUnary
}
```


#### func  Defer

```go
func Defer(call *ExprCall) *StmtDefer
```

#### func (*StmtDefer) Emit

```go
func (this *StmtDefer) Emit(w IWriter)
```

#### type StmtFor

```go
type StmtFor struct {
	SynBlock
	Range struct {
		Idx    *Named
		Val    *Named
		Iteree IEmit
	}
	Loop struct {
		Init IEmit
		Cond IEmit
		Each IEmit
	}
}
```


#### func  ForLoop

```go
func ForLoop(maybeInit IEmit, maybeCond IEmit, maybeEach IEmit, body ...IEmit) *StmtFor
```

#### func  ForRange

```go
func ForRange(maybeIdx *Named, maybeVal *Named, iteree IEmit, body ...IEmit) *StmtFor
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
func Go(call *ExprCall) *StmtGo
```

#### func (*StmtGo) Emit

```go
func (this *StmtGo) Emit(w IWriter)
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
func If(cond IEmit, thens ...IEmit) *StmtIf
```

#### func  Ifs

```go
func Ifs(ifThensAndMaybeAnElse ...IEmit) *StmtIf
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
func Ret(retExpr IEmit) *StmtRet
```

#### func (*StmtRet) Emit

```go
func (this *StmtRet) Emit(w IWriter)
```

#### type StmtSwitch

```go
type StmtSwitch struct {
	Cond    IEmit
	Cases   []SynCond
	Default SynBlock
}
```


#### func  Switch

```go
func Switch(maybeCond IEmit, caseCondsAndBlocksPlusMaybeDefaultBlock ...IEmit) *StmtSwitch
```

#### func (*StmtSwitch) Emit

```go
func (this *StmtSwitch) Emit(w IWriter)
```

#### type StmtUnary

```go
type StmtUnary struct {
	Expr IEmit
}
```


#### type StmtVar

```go
type StmtVar struct {
	NamedTyped
	Expr IEmit
}
```


#### func  Var

```go
func Var(name string, maybeType *TypeRef, maybeExpr IEmit) *StmtVar
```

#### func (*StmtVar) Emit

```go
func (this *StmtVar) Emit(w IWriter)
```

#### type SynBlock

```go
type SynBlock struct {
	Body []IEmit
}
```


#### func  Block

```go
func Block(body ...IEmit) *SynBlock
```

#### func (*SynBlock) Emit

```go
func (this *SynBlock) Emit(w IWriter)
```

#### type SynCond

```go
type SynCond struct {
	Cond IEmit
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
func File(pkgName string, decls ...IEmit) *SynFile
```

#### func (*SynFile) Emit

```go
func (this *SynFile) Emit(w IWriter)
```

#### func (*SynFile) I

```go
func (this *SynFile) I(pkgImportPath string) (pkgName string)
```

#### func (*SynFile) Src

```go
func (this *SynFile) Src() (src []byte, err error)
```

#### type SynFunc

```go
type SynFunc struct {
	SynBlock
	NamedTyped
	Recv *NamedTyped
}
```


#### func  Func

```go
func Func(recv *NamedTyped, name string, sig *TypeRef, body ...IEmit) *SynFunc
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


#### func  TStructFld

```go
func TStructFld(name string, typeRef *TypeRef, tags map[string]string) (fld SynStructField)
```

#### func (*SynStructField) Emit

```go
func (this *SynStructField) Emit(w IWriter)
```

#### type TypeDef

```go
type TypeDef struct {
	NamedTyped
	IsAlias bool
}
```


#### func  TDef

```go
func TDef(name string, typeRef *TypeRef, isAlias bool) *TypeDef
```

#### func (*TypeDef) Emit

```go
func (this *TypeDef) Emit(w IWriter)
```

#### type TypeFunc

```go
type TypeFunc struct {
	Args NamedsTypeds
	Rets NamedsTypeds
}
```


#### func  TFunc

```go
func TFunc(args NamedsTypeds, rets ...NamedTyped) *TypeFunc
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


#### func  TInterface

```go
func TInterface(embeds []TypeRef, methods ...NamedTyped) *TypeInterface
```

#### func (*TypeInterface) Emit

```go
func (this *TypeInterface) Emit(w IWriter)
```

#### type TypeRef

```go
type TypeRef struct {
	ToPrim struct {
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
	ToSliceOf *TypeRef
	ToPtrOf   *TypeRef
	ToMapOf   struct {
		Key *TypeRef
		Val *TypeRef
	}
	ToFunc      *TypeFunc
	ToInterface *TypeInterface
	ToStruct    *TypeStruct
	ToNamed     struct {
		PkgName  string
		TypeName string
	}
}
```


#### func  TrFunc

```go
func TrFunc(typeFunc *TypeFunc) *TypeRef
```

#### func  TrIface

```go
func TrIface(typeIface *TypeInterface) *TypeRef
```

#### func  TrMap

```go
func TrMap(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrN

```go
func TrN(pkgName string, typeName string) *TypeRef
```

#### func  TrPtr

```go
func TrPtr(typeRef *TypeRef) *TypeRef
```

#### func  TrSl

```go
func TrSl(typeRef *TypeRef) *TypeRef
```

#### func  TrStruct

```go
func TrStruct(typeStruct *TypeStruct) *TypeRef
```

#### func  TrpBool

```go
func TrpBool(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpByte

```go
func TrpByte(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpC128

```go
func TrpC128(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpC64

```go
func TrpC64(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpF32

```go
func TrpF32(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpF64

```go
func TrpF64(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpI16

```go
func TrpI16(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpI32

```go
func TrpI32(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpI64

```go
func TrpI64(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpI8

```go
func TrpI8(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpInt

```go
func TrpInt(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpRune

```go
func TrpRune(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpStr

```go
func TrpStr(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpUi16

```go
func TrpUi16(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpUi32

```go
func TrpUi32(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpUi64

```go
func TrpUi64(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpUi8

```go
func TrpUi8(keyType *TypeRef, valType *TypeRef) *TypeRef
```

#### func  TrpUint

```go
func TrpUint(keyType *TypeRef, valType *TypeRef) *TypeRef
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


#### func  TStruct

```go
func TStruct(embeds []TypeRef, fields ...SynStructField) *TypeStruct
```

#### func (*TypeStruct) Emit

```go
func (this *TypeStruct) Emit(w IWriter)
```
