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
func (this *ExprCall) Emit(w IEmitter)
```

#### type ExprLit

```go
type ExprLit struct {
	Val interface{}
}
```


#### func (*ExprLit) Emit

```go
func (this *ExprLit) Emit(w IEmitter)
```

#### type ExprNil

```go
type ExprNil struct {
}
```


#### func (*ExprNil) Emit

```go
func (this *ExprNil) Emit(w IEmitter)
```

#### type Func

```go
type Func struct {
	NamedTyped
	Recv *NamedTyped
	Body []IEmit
}
```


#### func (*Func) Emit

```go
func (this *Func) Emit(w IEmitter)
```

#### type IEmit

```go
type IEmit interface {
	Emit(IEmitter)
}
```


#### type IEmitter

```go
type IEmitter interface {
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
func (this *Named) Emit(w IEmitter)
```

#### type NamedTyped

```go
type NamedTyped struct {
	Named
	Type TypeRef
}
```


#### type NamedsTypeds

```go
type NamedsTypeds []*NamedTyped
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
func (this *OpAddr) Emit(w IEmitter)
```

#### type OpAnd

```go
type OpAnd struct{ Op }
```


#### func (*OpAnd) Emit

```go
func (this *OpAnd) Emit(w IEmitter)
```

#### type OpComma

```go
type OpComma struct{ Op }
```


#### func (*OpComma) Emit

```go
func (this *OpComma) Emit(w IEmitter)
```

#### type OpDecl

```go
type OpDecl struct{ Op }
```


#### func (*OpDecl) Emit

```go
func (this *OpDecl) Emit(w IEmitter)
```

#### type OpDeref

```go
type OpDeref struct{ Op }
```


#### func (*OpDeref) Emit

```go
func (this *OpDeref) Emit(w IEmitter)
```

#### type OpDiv

```go
type OpDiv struct{ Op }
```


#### func (*OpDiv) Emit

```go
func (this *OpDiv) Emit(w IEmitter)
```

#### type OpDot

```go
type OpDot struct{ Op }
```


#### func (*OpDot) Emit

```go
func (this *OpDot) Emit(w IEmitter)
```

#### type OpEq

```go
type OpEq struct{ Op }
```


#### func (*OpEq) Emit

```go
func (this *OpEq) Emit(w IEmitter)
```

#### type OpGeq

```go
type OpGeq struct{ Op }
```


#### func (*OpGeq) Emit

```go
func (this *OpGeq) Emit(w IEmitter)
```

#### type OpGt

```go
type OpGt struct{ Op }
```


#### func (*OpGt) Emit

```go
func (this *OpGt) Emit(w IEmitter)
```

#### type OpIdx

```go
type OpIdx struct{ Op }
```


#### func (*OpIdx) Emit

```go
func (this *OpIdx) Emit(w IEmitter)
```

#### type OpLeq

```go
type OpLeq struct{ Op }
```


#### func (*OpLeq) Emit

```go
func (this *OpLeq) Emit(w IEmitter)
```

#### type OpLt

```go
type OpLt struct{ Op }
```


#### func (*OpLt) Emit

```go
func (this *OpLt) Emit(w IEmitter)
```

#### type OpMinus

```go
type OpMinus struct{ Op }
```


#### func (*OpMinus) Emit

```go
func (this *OpMinus) Emit(w IEmitter)
```

#### type OpMult

```go
type OpMult struct{ Op }
```


#### func (*OpMult) Emit

```go
func (this *OpMult) Emit(w IEmitter)
```

#### type OpNeq

```go
type OpNeq struct{ Op }
```


#### func (*OpNeq) Emit

```go
func (this *OpNeq) Emit(w IEmitter)
```

#### type OpNot

```go
type OpNot struct{ Op }
```


#### func (*OpNot) Emit

```go
func (this *OpNot) Emit(w IEmitter)
```

#### type OpOr

```go
type OpOr struct{ Op }
```


#### func (*OpOr) Emit

```go
func (this *OpOr) Emit(w IEmitter)
```

#### type OpPlus

```go
type OpPlus struct{ Op }
```


#### func (*OpPlus) Emit

```go
func (this *OpPlus) Emit(w IEmitter)
```

#### type OpSet

```go
type OpSet struct{ Op }
```


#### func (*OpSet) Emit

```go
func (this *OpSet) Emit(w IEmitter)
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
func (this *StmtConst) Emit(w IEmitter)
```

#### type StmtDefer

```go
type StmtDefer struct {
	StmtUnary
}
```


#### func (*StmtDefer) Emit

```go
func (this *StmtDefer) Emit(w IEmitter)
```

#### type StmtGo

```go
type StmtGo struct {
	StmtUnary
}
```


#### func (*StmtGo) Emit

```go
func (this *StmtGo) Emit(w IEmitter)
```

#### type StmtRet

```go
type StmtRet struct {
	StmtUnary
}
```


#### func (*StmtRet) Emit

```go
func (this *StmtRet) Emit(w IEmitter)
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
func (this *StmtVar) Emit(w IEmitter)
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
func (this *TypeDef) Emit(w IEmitter)
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
func (this *TypeFunc) Emit(w IEmitter)
```

#### type TypeInterface

```go
type TypeInterface struct {
	Embeds  []*TypeRef
	Methods NamedsTypeds
}
```


#### func (*TypeInterface) Emit

```go
func (this *TypeInterface) Emit(w IEmitter)
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
	ToOther struct {
		PkgName  string
		TypeName string
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
}
```


#### func (*TypeRef) Emit

```go
func (this *TypeRef) Emit(w IEmitter)
```

#### type TypeStruct

```go
type TypeStruct struct {
	Embeds []*TypeRef
	Fields NamedsTypeds
}
```


#### func (*TypeStruct) Emit

```go
func (this *TypeStruct) Emit(w IEmitter)
```
