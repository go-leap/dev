# udevgosyn
--
    import "github.com/go-leap/dev/go/syn"


## Usage

#### type ExprCall

```go
type ExprCall struct {
	Callee IEmit
	Args   []IEmit
}
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
type Named struct {
	Name string
}
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


#### type OpAddr

```go
type OpAddr struct{ Op }
```


#### func (*OpAddr) Emit

```go
func (this *OpAddr) Emit(w IWriter)
```

#### type OpAnd

```go
type OpAnd struct{ Op }
```


#### func (*OpAnd) Emit

```go
func (this *OpAnd) Emit(w IWriter)
```

#### type OpComma

```go
type OpComma struct{ Op }
```


#### func (*OpComma) Emit

```go
func (this *OpComma) Emit(w IWriter)
```

#### type OpDecl

```go
type OpDecl struct{ Op }
```


#### func (*OpDecl) Emit

```go
func (this *OpDecl) Emit(w IWriter)
```

#### type OpDeref

```go
type OpDeref struct{ Op }
```


#### func (*OpDeref) Emit

```go
func (this *OpDeref) Emit(w IWriter)
```

#### type OpDiv

```go
type OpDiv struct{ Op }
```


#### func (*OpDiv) Emit

```go
func (this *OpDiv) Emit(w IWriter)
```

#### type OpDot

```go
type OpDot struct{ Op }
```


#### func (*OpDot) Emit

```go
func (this *OpDot) Emit(w IWriter)
```

#### type OpEq

```go
type OpEq struct{ Op }
```


#### func (*OpEq) Emit

```go
func (this *OpEq) Emit(w IWriter)
```

#### type OpGeq

```go
type OpGeq struct{ Op }
```


#### func (*OpGeq) Emit

```go
func (this *OpGeq) Emit(w IWriter)
```

#### type OpGt

```go
type OpGt struct{ Op }
```


#### func (*OpGt) Emit

```go
func (this *OpGt) Emit(w IWriter)
```

#### type OpIdx

```go
type OpIdx struct{ Op }
```


#### func (*OpIdx) Emit

```go
func (this *OpIdx) Emit(w IWriter)
```

#### type OpLeq

```go
type OpLeq struct{ Op }
```


#### func (*OpLeq) Emit

```go
func (this *OpLeq) Emit(w IWriter)
```

#### type OpLt

```go
type OpLt struct{ Op }
```


#### func (*OpLt) Emit

```go
func (this *OpLt) Emit(w IWriter)
```

#### type OpMinus

```go
type OpMinus struct{ Op }
```


#### func (*OpMinus) Emit

```go
func (this *OpMinus) Emit(w IWriter)
```

#### type OpMult

```go
type OpMult struct{ Op }
```


#### func (*OpMult) Emit

```go
func (this *OpMult) Emit(w IWriter)
```

#### type OpNeq

```go
type OpNeq struct{ Op }
```


#### func (*OpNeq) Emit

```go
func (this *OpNeq) Emit(w IWriter)
```

#### type OpNot

```go
type OpNot struct{ Op }
```


#### func (*OpNot) Emit

```go
func (this *OpNot) Emit(w IWriter)
```

#### type OpOr

```go
type OpOr struct{ Op }
```


#### func (*OpOr) Emit

```go
func (this *OpOr) Emit(w IWriter)
```

#### type OpPlus

```go
type OpPlus struct{ Op }
```


#### func (*OpPlus) Emit

```go
func (this *OpPlus) Emit(w IWriter)
```

#### type OpSet

```go
type OpSet struct{ Op }
```


#### func (*OpSet) Emit

```go
func (this *OpSet) Emit(w IWriter)
```

#### type StmtConst

```go
type StmtConst struct {
	NamedTyped
	Expr ExprLit
}
```


#### func (*StmtConst) Emit

```go
func (this *StmtConst) Emit(w IWriter)
```

#### type StmtDefer

```go
type StmtDefer struct {
	StmtUnary
}
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


#### type SynCond

```go
type SynCond struct {
	Cond IEmit
	SynBlock
}
```


#### type SynFunc

```go
type SynFunc struct {
	SynBlock
	NamedTyped
	Recv *NamedTyped
}
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


#### func (*TypeStruct) Emit

```go
func (this *TypeStruct) Emit(w IWriter)
```
