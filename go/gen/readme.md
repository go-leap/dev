# udevgogen
--
    import "github.com/go-leap/dev/go/gen"

Package `github.com/go-leap/dev/go/gen` provides AST nodes for generating Go
code. These are by design simpler and more lightweight than `go/ast`, given that
the latter is designed to represent existing-and-parsed code (keeping track of
much housekeeping), while this package only focuses on ad-hoc assemblage of
newly-to-be-emitted Go sources.

As a noteworthy goodie, all `func`s that have named return values automatically
get a final `return` statement appended to their `Body` at code-gen time, if
they don't already have one.

A small handful of `udevgogen` exports have entirely upper-case names (such as
`GEN_IF`, `GEN_BYCASE`, `GEN_FOR`, `UNLESS`, etc.) All these offer
_codegen-time_ control flow and their usage is showcased throughout the numerous
`github.com/metaleap/go-gent/gents/...` packages. They do incur a slight
overhead (vs. using Go-native control-flows) for the neat readability sugar they
offer. The upper-case names do stand out in real-world multi-line AST
constructions, letting the reader differentiate between say branches / loops
to-be-evaluated at code-gen time vs. to-be-emitted branches/loops belonging to
the currently generated AST nodes.

Likewise, there are numerous interfaces with `IExpr`-prefixed names such as
`IExprBoolish`, `IExprNumerish`, etc (implemented by the various `ISyn` AST-node
implementations provided), and those interfaces offer handy dot-accessor-style
methods over standard constructor funcs --- by way of an illustrative example,
think: `foo.Eq(bar).And(baz.Minus(2).Gt(0))` instead of `And(Eq(foo, bar),
Gt(Sub(baz, L(2)), L(0)))`. All `IExpr‹Foo›ish` implementations are just such
translations under the hood, implying some miniscule (or perhaps sub-nanoscule)
cost there.

## Usage

```go
var (
	// see `PkgImports.Ensure(string) string` for details
	PkgImportNamePrefix               PkgName = "pkg__"
	PkgImportNameTrimPathsPrefixPrior string

	// intended to remain zero-valued (Name="" and Type=nil)
	NoMethodRecv NamedTyped

	// intended to remain zero-valued (Name="")
	None Named

	// singletons for simple (operand-less / arg-less) keywords
	K struct {
		Break    StmtBreak
		Continue StmtContinue
		Return   StmtRet
	}

	// singletons for Go's `builtin` reserved-identifiers
	B struct {
		Nil   ExprLit
		True  ExprLit
		False ExprLit

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

	// singletons for common var names
	Vars struct {
		// `"err"`
		Err NamedTyped
		// `"e"`, suits temporary/intermediate error vars
		E Named
		// `"ok"`, suitable for type-asserts / lookups / predicates
		Ok Named
		// `"r"`, suitable for a func's primary named `return` value
		R Named
		// `"s"`, suitable for a func's only `string` arg
		S Named
		// `"sl"`, suitable a func's only slice arg
		Sl Named
		// `"t"`, suitable for temporary intermediate vars
		T Named
		// `"i"`, suitable for iterations
		I Named
		// `"j"`, suitable for sub-iterations
		J Named
		// `"id"`, suitable for an ID
		Id Named
		// `"ids"`, suitable for IDs
		Ids Named
		// `"k"`, suitable for key-value pairs, eg. in for..range iterations
		K Named
		// `"v"`, for key-value pairs or func args (eg. `v interface{}`)
		V Named
		// `"kv"`, suitable for a key-value pair
		KV Named
		// `"kvs"`, suitable for collection of key-value pairs
		KVs Named
	}

	// singletons for common type-refs
	T struct {
		Bool       *TypeRef
		Byte       *TypeRef
		Complex64  *TypeRef
		Complex128 *TypeRef
		Error      *TypeRef
		Float32    *TypeRef
		Float64    *TypeRef
		Int8       *TypeRef
		Int16      *TypeRef
		Int32      *TypeRef
		Int64      *TypeRef
		Int        *TypeRef
		Uint       *TypeRef
		Uint8      *TypeRef
		Uint16     *TypeRef
		Uint32     *TypeRef
		Uint64     *TypeRef
		Rune       *TypeRef
		String     *TypeRef

		// some not-so-uncommon slices
		SliceOf struct {
			Bytes   *TypeRef
			Ints    *TypeRef
			Strings *TypeRef
		}

		// singletons for empty anonymous-types
		Empty struct {
			Interface *TypeRef
			Struct    *TypeRef
		}
	}

	// singletons for common func sigs
	Sigs struct {
		// func() bool
		NoneToBool TypeFunc

		// func() string
		NoneToString TypeFunc
	}
)
```

```go
var This = Named{"this"}
```

#### type ExprCall

```go
type ExprCall struct {
	Callee         ISyn
	Args           Syns
	LastArgSpreads bool
}
```

ExprCall represents a call to any callable `Callee`, or a type conversion (if
`Callee` effectively names a type).

#### func  C

```go
func C(callee IAny, args ...IAny) *ExprCall
```

#### func  Call

```go
func Call(callee ISyn, args ...ISyn) *ExprCall
```
Call constructs an `ExprCall`.

#### func (*ExprCall) And

```go
func (this *ExprCall) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (*ExprCall) At

```go
func (this *ExprCall) At(operand IAny) OpIdx
```
At implements `IExprContainish`.

#### func (*ExprCall) C

```go
func (this *ExprCall) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall
```
C implements `IExprDottish`.

#### func (*ExprCall) D

```go
func (this *ExprCall) D(operands ...IAny) OpDot
```
D implements `IExprDottish`.

#### func (*ExprCall) Defer

```go
func (this *ExprCall) Defer() StmtDefer
```
Defer constructs a `StmtDefer` of `this` call.

#### func (*ExprCall) Deref

```go
func (this *ExprCall) Deref() OpDeref
```
Deref implements `IExprContainish`.

#### func (*ExprCall) Div

```go
func (this *ExprCall) Div(operand IAny) OpDiv
```
Div implements `IExprNumerish`.

#### func (*ExprCall) Eq

```go
func (this *ExprCall) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (*ExprCall) Geq

```go
func (this *ExprCall) Geq(operand IAny) IExprBoolish
```
Geq implements `IExprOrdish`.

#### func (*ExprCall) Go

```go
func (this *ExprCall) Go() StmtGo
```
Go constructs a `StmtGo` on `this` call.

#### func (*ExprCall) Gt

```go
func (this *ExprCall) Gt(operand IAny) IExprBoolish
```
Gt implements `IExprOrdish`.

#### func (*ExprCall) Leq

```go
func (this *ExprCall) Leq(operand IAny) IExprBoolish
```
Leq implements `IExprOrdish`.

#### func (*ExprCall) Lt

```go
func (this *ExprCall) Lt(operand IAny) IExprBoolish
```
Lt implements `IExprOrdish`.

#### func (*ExprCall) Minus

```go
func (this *ExprCall) Minus(operand IAny) OpSub
```
Minus implements `IExprNumerish`.

#### func (*ExprCall) Mod

```go
func (this *ExprCall) Mod(operand IAny) OpMod
```
Mod implements `IExprNumerish`.

#### func (*ExprCall) Neg

```go
func (this *ExprCall) Neg() OpSub
```
Neg implements `IExprNumerish`.

#### func (*ExprCall) Neq

```go
func (this *ExprCall) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (*ExprCall) Not

```go
func (this *ExprCall) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (*ExprCall) Of

```go
func (this *ExprCall) Of(args ...IAny) *ExprCall
```
Of implements `IExprCallish`.

#### func (*ExprCall) Or

```go
func (this *ExprCall) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### func (*ExprCall) Plus

```go
func (this *ExprCall) Plus(operand IAny) OpAdd
```
Plus implements `IExprNumerish`.

#### func (*ExprCall) Sl

```go
func (this *ExprCall) Sl(startIndex IAny, stopIndex IAny) OpIdx
```
Sl implements `IExprContainish`.

#### func (*ExprCall) Spreads

```go
func (this *ExprCall) Spreads() *ExprCall
```

#### func (*ExprCall) Times

```go
func (this *ExprCall) Times(operand IAny) OpMul
```
Times implements `IExprNumerish`.

#### type ExprLit

```go
type ExprLit struct {
	Val IAny
}
```

ExprLit represents any literal constant value, such as a string, rune, number or
boolean.

#### func  L

```go
func L(lit IAny) ExprLit
```
L constructs an `ExprLit`.

#### type IAny

```go
type IAny = interface{}
```

IAny is opinionated brevity-readability-delight.

#### type IExprBoolish

```go
type IExprBoolish interface {
	IExprEqualish

	// expr.And(foo) == And(expr, ISyn(foo))
	And(IAny) OpAnd
	// expr.Or(foo) == Or(expr, ISyn(foo))
	Or(IAny) OpOr
	// expr.Not() == Not(expr)
	Not() OpNot
}
```

IExprBoolish is implemented by `IExprEqualish`s that also wish to offer
dot-accessor syntax over the `And`, `Or`, `Not` constructors.

All `ISyn`s among the `Any`-typed operand arguments are used directly, any
others are converted into `ExprLit`s.

#### type IExprCallish

```go
type IExprCallish interface {
	ISyn

	// expr.Of(foos...) == Call(expr, Syns(foos)...)
	Of(...IAny) *ExprCall
}
```

IExprCallish is implemented by `ISyn`s that also wish to offer dot-accessor
syntax over the `Call` constructor.

All `ISyn`s among the `Any`-typed operand arguments are used directly, any
others are converted into `ExprLit`s.

#### type IExprContainish

```go
type IExprContainish interface {
	ISyn

	// expr.At(foo) == At(expr, ISyn(foo))
	At(IAny) OpIdx
	// expr.Deref() == Deref(expr)
	Deref() OpDeref
	// expr.Sl(foo,bar) == At(expr, Sl(Syn(foo), Syn(bar)))
	Sl(IAny, IAny) OpIdx
}
```

IExprContainish is implemented by `ISyn`s that also wish to offer dot-accessor
syntax over the `At`, `Sl`, `Deref` constructors.

#### type IExprDeclish

```go
type IExprDeclish interface {
	IExprVarish

	// expr.Let(foo) == Decl(expr, ISyn(foo))
	Let(IAny) OpDecl
}
```

IExprDeclish is implemented by `IExprVarish`s that also wish to offer
dot-accessor syntax over the `Decl` constructor.

#### type IExprDottish

```go
type IExprDottish interface {
	ISyn

	C(string, ...IAny) *ExprCall
	D(...IAny) OpDot
}
```

IExprDottish is implemented by `ISyn`s that also wish to offer dot-accessor
syntax over the `D` constructor.

#### type IExprEqualish

```go
type IExprEqualish interface {
	ISyn

	// expr.Eq(foo) == Eq(expr, ISyn(foo))
	Eq(IAny) OpEq
	// expr.Neq(foo) == Neq(expr, ISyn(foo))
	Neq(IAny) OpNeq
}
```

IExprEqualish is implemented by `ISyn`s that also wish to offer dot-accessor
syntax over the `Eq` and `Neq` constructors.

All `ISyn`s among the `Any`-typed operand arguments are used directly, any
others are converted into `ExprLit`s.

#### type IExprNumerish

```go
type IExprNumerish interface {
	IExprOrdish

	// expr.Plus(foo) == Add(expr, ISyn(foo))
	Plus(IAny) OpAdd
	// expr.Minus(foo) == Sub(expr, ISyn(foo))
	Minus(IAny) OpSub
	// expr.Times(foo) == Mul(expr, ISyn(foo))
	Times(IAny) OpMul
	// expr.Div(foo) == Div(expr, ISyn(foo))
	Div(IAny) OpDiv
	// expr.Mod(foo) == Mod(expr, ISyn(foo))
	Mod(IAny) OpMod
	// expr.Neg() == Neg(expr)
	Neg() OpSub
}
```

IExprOrdish is implemented by `IExprOrdish`s that also wish to offer
dot-accessor syntax over the `Add`, `Sub`, `Mul`, `Div`, `Mod`, `Neg`
constructors.

All `ISyn`s among the `Any`-typed operand arguments are used directly, any
others are converted into `ExprLit`s.

#### type IExprOrdish

```go
type IExprOrdish interface {
	IExprEqualish

	// expr.Geq(foo) == Geq(expr, ISyn(foo))
	Geq(IAny) IExprBoolish
	// expr.Leq(foo) == Leq(expr, ISyn(foo))
	Leq(IAny) IExprBoolish
	// expr.Gt(foo) == Gt(expr, ISyn(foo))
	Gt(IAny) IExprBoolish
	// expr.Lt(foo) == Lt(expr, ISyn(foo))
	Lt(IAny) IExprBoolish
}
```

IExprOrdish is implemented by `IExprEqualish`s that also wish to offer
dot-accessor syntax over the `Geq`, `Gt`, `Leq`, `Lt` constructors.

All methods return the appropriate operator types at all times, ie. `Geq` always
returns an `OpGeq`, `Lt` always an `OpLt` etc.

All `ISyn`s among the `Any`-typed operand arguments are used directly, any
others are converted into `ExprLit`s.

#### type IExprVarish

```go
type IExprVarish interface {
	IExprContainish

	// expr.Addr() == Addr(expr)
	Addr() OpAddr
	// expr.Set(foo) == Set(expr, ISyn(foo))
	Set(IAny) OpSet
	// expr.Incr1() == Set(expr, Add(expr, L(1)))
	Incr1() OpSet
	// expr.Decr1() == Set(expr, Sub(expr, L(1)))
	Decr1() OpSet
}
```

IExprVarish is implemented by `IExprContainish`s that also wish to offer
dot-accessor syntax over the `Addr`, `Set` constructors.

All `ISyn`s among the `Any`-typed operand arguments are used directly, any
others are converted into `ExprLit`s.

#### type ISyn

```go
type ISyn interface {
	// contains filtered or unexported methods
}
```

ISyn implementations represent some (atomic or compound) syntactic entity in the
Abstract Syntax Tree (AST), eg.: literals, vars, consts, type-defs, type-refs,
funcs, calls, keywords, operators, ...

#### func  GEN_BYCASE

```go
func GEN_BYCASE(usually USUALLY, unless UNLESS) ISyn
```
GEN_BYCASE is like a codegen-time `switch..case` construct: it returns
`unless[true]` if present, else `usually`.

#### func  GEN_EITHER

```go
func GEN_EITHER(check bool, then ISyn, otherwise ISyn) ISyn
```
GEN_EITHER returns `then` if `check`, else `otherwise`.

#### func  GEN_MAYBE

```go
func GEN_MAYBE(maybe ISyn) ISyn
```
GEN_MAYBE returns `maybe` if not `nil`, otherwise an empty `Syns`.

#### type Named

```go
type Named struct {
	Name string
}
```

Named emits its `Name` during code-generation as-is, hence used for referring to
named vars, consts, types, funcs etc.

#### func  N

```go
func N(name string) Named
```
N constructs a `Named`.

#### func (Named) Addr

```go
func (this Named) Addr() OpAddr
```
Addr implements `IExprVarish`.

#### func (Named) And

```go
func (this Named) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (Named) At

```go
func (this Named) At(operand IAny) OpIdx
```
At implements `IExprContainish`.

#### func (Named) C

```go
func (this Named) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall
```
C implements `IExprDottish`.

#### func (Named) D

```go
func (this Named) D(operands ...IAny) OpDot
```
D implements `IExprDottish`.

#### func (Named) Decr1

```go
func (this Named) Decr1() OpSet
```
Decr1 implements `IExprVarish`.

#### func (Named) Deref

```go
func (this Named) Deref() OpDeref
```
Deref implements `IExprContainish`.

#### func (Named) Div

```go
func (this Named) Div(operand IAny) OpDiv
```
Div implements `IExprNumerish`.

#### func (Named) Eq

```go
func (this Named) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (Named) Geq

```go
func (this Named) Geq(operand IAny) IExprBoolish
```
Geq implements `IExprOrdish`.

#### func (Named) Gt

```go
func (this Named) Gt(operand IAny) IExprBoolish
```
Gt implements `IExprOrdish`.

#### func (Named) Incr1

```go
func (this Named) Incr1() OpSet
```
Incr1 implements `IExprVarish`.

#### func (Named) Leq

```go
func (this Named) Leq(operand IAny) IExprBoolish
```
Leq implements `IExprOrdish`.

#### func (Named) Let

```go
func (this Named) Let(operand IAny) OpDecl
```
Let implements `IExprVarish`.

#### func (Named) Lt

```go
func (this Named) Lt(operand IAny) IExprBoolish
```
Lt implements `IExprOrdish`.

#### func (Named) Minus

```go
func (this Named) Minus(operand IAny) OpSub
```
Minus implements `IExprNumerish`.

#### func (Named) Mod

```go
func (this Named) Mod(operand IAny) OpMod
```
Mod implements `IExprNumerish`.

#### func (Named) Neg

```go
func (this Named) Neg() OpSub
```
Neg implements `IExprNumerish`.

#### func (Named) Neq

```go
func (this Named) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (Named) Not

```go
func (this Named) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (Named) Of

```go
func (this Named) Of(args ...IAny) *ExprCall
```
Of implements `IExprCallish`.

#### func (Named) OfType

```go
func (this Named) OfType(typeRef *TypeRef) (nt NamedTyped)
```
OfType returns a `NamedTyped` with `this.Name` and `typeRef`.

#### func (Named) Or

```go
func (this Named) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### func (Named) Plus

```go
func (this Named) Plus(operand IAny) OpAdd
```
Plus implements `IExprNumerish`.

#### func (Named) Set

```go
func (this Named) Set(operand IAny) OpSet
```
Set implements `IExprVarish`.

#### func (Named) Sl

```go
func (this Named) Sl(startIndex IAny, stopIndex IAny) OpIdx
```
Sl implements `IExprContainish`.

#### func (Named) Times

```go
func (this Named) Times(operand IAny) OpMul
```
Times implements `IExprNumerish`.

#### type NamedTyped

```go
type NamedTyped struct {
	Named
	Type *TypeRef
}
```

NamedTyped details a `Name` and a `TypeRef`, such as needed for func args,
return values, struct fields etc.

#### func (NamedTyped) Method

```go
func (this NamedTyped) Method(name string, args ...NamedTyped) *SynFunc
```
Method constructs a `SynFunc` with the given `name` and `args` plus `this` as
its method `Recv`.

#### type NamedsTypeds

```go
type NamedsTypeds []NamedTyped
```

NamedsTypeds is a slice of 0-or-more `NamedTyped`s.

#### func (*NamedsTypeds) Add

```go
func (this *NamedsTypeds) Add(name string, typeRef *TypeRef)
```
Add is a convenience short-hand for `append`.

#### func (NamedsTypeds) AllNamed

```go
func (this NamedsTypeds) AllNamed() bool
```
AllNamed returns whether all `NamedTyped`s in `this` have a `Name` set. If
`this` is empty, `false` is returned.

#### func (NamedsTypeds) AllTyped

```go
func (this NamedsTypeds) AllTyped() bool
```
AllTyped returns whether all `NamedTyped`s in `this` have a `Type` set. If
`this` is empty, `false` is returned.

#### func (NamedsTypeds) IfUntypedUse

```go
func (this NamedsTypeds) IfUntypedUse(typeRef *TypeRef) NamedsTypeds
```

#### func (NamedsTypeds) LastEllipsisIfSlice

```go
func (this NamedsTypeds) LastEllipsisIfSlice() (r NamedsTypeds)
```

#### func (NamedsTypeds) Names

```go
func (this NamedsTypeds) Names(strLits bool) []interface{}
```

#### func (NamedsTypeds) Renamed

```go
func (this NamedsTypeds) Renamed(rename func(string) string) (renamed NamedsTypeds)
```

#### func (NamedsTypeds) ToAnys

```go
func (this NamedsTypeds) ToAnys(transform func(*NamedTyped) IAny) (transformed []IAny)
```

#### func (NamedsTypeds) ToSyns

```go
func (this NamedsTypeds) ToSyns(transform func(*NamedTyped) ISyn) (transformed Syns)
```

#### type Op

```go
type Op struct {
	// 1 or more operands: if 1 then
	// unary syntax output, else n-ary
	Operands Syns
}
```

Op is embedded by all spsecific operators such as `OpAdd`, `OpEq`, etc.

#### type OpAdd

```go
type OpAdd struct{ Op }
```

OpAdd represents one or more `+` subtractions.

#### func  Add

```go
func Add(operands ...ISyn) OpAdd
```
Add constructs an `OpAdd`.

#### func (OpAdd) Plus

```go
func (this OpAdd) Plus(operand IAny) OpAdd
```

#### type OpAddr

```go
type OpAddr struct{ Op }
```

OpNot represents Go's unary address-taking `&` operator.

#### func  Addr

```go
func Addr(operands ...ISyn) OpAddr
```
Addr constructs an `OpAddr`.

#### type OpAnd

```go
type OpAnd struct{ Op }
```

OpAnd represents Go's `&&` boolean-or operator.

#### func  And

```go
func And(operands ...ISyn) OpAnd
```
And constructs an `OpAnd`.

#### func (OpAnd) And

```go
func (this OpAnd) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpAnd) Eq

```go
func (this OpAnd) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpAnd) Neq

```go
func (this OpAnd) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpAnd) Not

```go
func (this OpAnd) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpAnd) Or

```go
func (this OpAnd) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### type OpColon

```go
type OpColon struct{ Op }
```

OpColon emits all its operands separated by `:` colons (for sub-slicing).

#### func  Sl

```go
func Sl(operands ...ISyn) OpColon
```
Sl constructs an `OpColon`.

#### type OpComma

```go
type OpComma struct{ Op }
```

OpComma emits all its operands separated by `,` commas.

#### func  Lits

```go
func Lits(lits ...IAny) OpComma
```
Lits constructs an `OpComma` of `ExprLit` operands.

#### func  Names

```go
func Names(names ...string) OpComma
```
Names constructs an `OpComma` of `Named` operands.

#### func  Tup

```go
func Tup(operands ...ISyn) OpComma
```
Tup constructs an `OpComma`.

#### func (OpComma) Let

```go
func (this OpComma) Let(operand IAny) OpDecl
```
Let implements `IExprVarish`.

#### func (OpComma) Set

```go
func (this OpComma) Set(operand IAny) OpSet
```
Set implements `IExprVarish`.

#### type OpDecl

```go
type OpDecl struct{ Op }
```

OpDecl represents Go's `:=` declare-and-initialize operator.

#### func  Decl

```go
func Decl(operands ...ISyn) OpDecl
```
Decl constructs an `OpDecl`.

#### type OpDeref

```go
type OpDeref struct{ Op }
```

OpNot represents Go's unary pointer-dereferencing `*` operator.

#### func  Deref

```go
func Deref(operands ...ISyn) OpDeref
```
Deref constructs an `OpDeref`.

#### func (OpDeref) And

```go
func (this OpDeref) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpDeref) At

```go
func (this OpDeref) At(operand IAny) OpIdx
```
At implements `IExprContainish`.

#### func (OpDeref) C

```go
func (this OpDeref) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall
```
C implements `IExprDottish`.

#### func (OpDeref) D

```go
func (this OpDeref) D(operands ...IAny) OpDot
```
D implements `IExprDottish`.

#### func (OpDeref) Decr1

```go
func (this OpDeref) Decr1() OpSet
```
Decr1 implements `IExprVarish`.

#### func (OpDeref) Deref

```go
func (this OpDeref) Deref() OpDeref
```
Deref implements `IExprContainish`.

#### func (OpDeref) Div

```go
func (this OpDeref) Div(operand IAny) OpDiv
```
Div implements `IExprNumerish`.

#### func (OpDeref) Eq

```go
func (this OpDeref) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpDeref) Geq

```go
func (this OpDeref) Geq(operand IAny) IExprBoolish
```
Geq implements `IExprOrdish`.

#### func (OpDeref) Gt

```go
func (this OpDeref) Gt(operand IAny) IExprBoolish
```
Gt implements `IExprOrdish`.

#### func (OpDeref) Incr1

```go
func (this OpDeref) Incr1() OpSet
```
Incr1 implements `IExprVarish`.

#### func (OpDeref) Leq

```go
func (this OpDeref) Leq(operand IAny) IExprBoolish
```
Leq implements `IExprOrdish`.

#### func (OpDeref) Lt

```go
func (this OpDeref) Lt(operand IAny) IExprBoolish
```
Lt implements `IExprOrdish`.

#### func (OpDeref) Minus

```go
func (this OpDeref) Minus(operand IAny) OpSub
```
Minus implements `IExprNumerish`.

#### func (OpDeref) Mod

```go
func (this OpDeref) Mod(operand IAny) OpMod
```
Mod implements `IExprNumerish`.

#### func (OpDeref) Neg

```go
func (this OpDeref) Neg() OpSub
```
Neg implements `IExprNumerish`.

#### func (OpDeref) Neq

```go
func (this OpDeref) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpDeref) Not

```go
func (this OpDeref) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpDeref) Of

```go
func (this OpDeref) Of(args ...IAny) *ExprCall
```
Of implements `IExprCallish`.

#### func (OpDeref) Or

```go
func (this OpDeref) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### func (OpDeref) Plus

```go
func (this OpDeref) Plus(operand IAny) OpAdd
```
Plus implements `IExprNumerish`.

#### func (OpDeref) Set

```go
func (this OpDeref) Set(operand IAny) OpSet
```
Set implements `IExprVarish`.

#### func (OpDeref) Sl

```go
func (this OpDeref) Sl(startIndex IAny, stopIndex IAny) OpIdx
```
Sl implements `IExprContainish`.

#### func (OpDeref) Times

```go
func (this OpDeref) Times(operand IAny) OpMul
```
Times implements `IExprNumerish`.

#### type OpDiv

```go
type OpDiv struct{ Op }
```

OpDiv represents one or more `/` divisions.

#### func  Div

```go
func Div(operands ...ISyn) OpDiv
```
Div constructs an `OpDiv`.

#### type OpDot

```go
type OpDot struct{ Op }
```

OpDot represents Go's `.` selector operator.

#### func  D

```go
func D(operands ...ISyn) OpDot
```
D constructs an `OpDot`.

#### func (OpDot) Addr

```go
func (this OpDot) Addr() OpAddr
```
Addr implements `IExprVarish`.

#### func (OpDot) And

```go
func (this OpDot) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpDot) At

```go
func (this OpDot) At(operand IAny) OpIdx
```
At implements `IExprContainish`.

#### func (OpDot) C

```go
func (this OpDot) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall
```
C implements `IExprDottish`.

#### func (OpDot) D

```go
func (this OpDot) D(operands ...IAny) OpDot
```
D implements `IExprDottish`.

#### func (OpDot) Decr1

```go
func (this OpDot) Decr1() OpSet
```
Decr1 implements `IExprVarish`.

#### func (OpDot) Deref

```go
func (this OpDot) Deref() OpDeref
```
Deref implements `IExprContainish`.

#### func (OpDot) Div

```go
func (this OpDot) Div(operand IAny) OpDiv
```
Div implements `IExprNumerish`.

#### func (OpDot) Eq

```go
func (this OpDot) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpDot) Geq

```go
func (this OpDot) Geq(operand IAny) IExprBoolish
```
Geq implements `IExprOrdish`.

#### func (OpDot) Gt

```go
func (this OpDot) Gt(operand IAny) IExprBoolish
```
Gt implements `IExprOrdish`.

#### func (OpDot) Incr1

```go
func (this OpDot) Incr1() OpSet
```
Incr1 implements `IExprVarish`.

#### func (OpDot) Leq

```go
func (this OpDot) Leq(operand IAny) IExprBoolish
```
Leq implements `IExprOrdish`.

#### func (OpDot) Lt

```go
func (this OpDot) Lt(operand IAny) IExprBoolish
```
Lt implements `IExprOrdish`.

#### func (OpDot) Minus

```go
func (this OpDot) Minus(operand IAny) OpSub
```
Minus implements `IExprNumerish`.

#### func (OpDot) Mod

```go
func (this OpDot) Mod(operand IAny) OpMod
```
Mod implements `IExprNumerish`.

#### func (OpDot) Neg

```go
func (this OpDot) Neg() OpSub
```
Neg implements `IExprNumerish`.

#### func (OpDot) Neq

```go
func (this OpDot) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpDot) Not

```go
func (this OpDot) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpDot) Of

```go
func (this OpDot) Of(args ...IAny) *ExprCall
```
Of implements `IExprCallish`.

#### func (OpDot) Or

```go
func (this OpDot) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### func (OpDot) Plus

```go
func (this OpDot) Plus(operand IAny) OpAdd
```
Plus implements `IExprNumerish`.

#### func (OpDot) Set

```go
func (this OpDot) Set(operand IAny) OpSet
```
Set implements `IExprVarish`.

#### func (OpDot) Sl

```go
func (this OpDot) Sl(startIndex IAny, stopIndex IAny) OpIdx
```
Sl implements `IExprContainish`.

#### func (OpDot) Times

```go
func (this OpDot) Times(operand IAny) OpMul
```
Times implements `IExprNumerish`.

#### type OpEq

```go
type OpEq struct{ Op }
```

OpEq represents Go's `==` equality comparison operator.

#### func  Eq

```go
func Eq(operands ...ISyn) OpEq
```
Eq constructs an `OpEq`.

#### func (OpEq) And

```go
func (this OpEq) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpEq) Eq

```go
func (this OpEq) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpEq) Neq

```go
func (this OpEq) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpEq) Not

```go
func (this OpEq) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpEq) Or

```go
func (this OpEq) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### type OpGeq

```go
type OpGeq struct{ Op }
```

OpGeq represents Go's `>=` greater-or-equal comparison operator.

#### func  Geq

```go
func Geq(operands ...ISyn) OpGeq
```
Geq constructs an `OpGeq`.

#### func (OpGeq) And

```go
func (this OpGeq) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpGeq) Eq

```go
func (this OpGeq) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpGeq) Neq

```go
func (this OpGeq) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpGeq) Not

```go
func (this OpGeq) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpGeq) Or

```go
func (this OpGeq) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### type OpGt

```go
type OpGt struct{ Op }
```

OpGt represents Go's `>` greater-than comparison operator.

#### func  Gt

```go
func Gt(operands ...ISyn) OpGt
```
Gt constructs an `OpGt`.

#### func (OpGt) And

```go
func (this OpGt) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpGt) Eq

```go
func (this OpGt) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpGt) Neq

```go
func (this OpGt) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpGt) Not

```go
func (this OpGt) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpGt) Or

```go
func (this OpGt) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### type OpIdx

```go
type OpIdx struct{ Op }
```

OpIdx represents one or more `operand0[operand1][operand2]` indexers.

#### func  At

```go
func At(operands ...ISyn) OpIdx
```
At constructs an `OpIdx`.

#### func (OpIdx) Addr

```go
func (this OpIdx) Addr() OpAddr
```
Addr implements `IExprVarish`.

#### func (OpIdx) And

```go
func (this OpIdx) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpIdx) At

```go
func (this OpIdx) At(operand IAny) OpIdx
```
At implements `IExprContainish`.

#### func (OpIdx) C

```go
func (this OpIdx) C(dotCalleeName string, dotCallArgs ...IAny) *ExprCall
```
C implements `IExprDottish`.

#### func (OpIdx) D

```go
func (this OpIdx) D(operands ...IAny) OpDot
```
D implements `IExprDottish`.

#### func (OpIdx) Decr1

```go
func (this OpIdx) Decr1() OpSet
```
Decr1 implements `IExprVarish`.

#### func (OpIdx) Deref

```go
func (this OpIdx) Deref() OpDeref
```
Deref implements `IExprContainish`.

#### func (OpIdx) Div

```go
func (this OpIdx) Div(operand IAny) OpDiv
```
Div implements `IExprNumerish`.

#### func (OpIdx) Eq

```go
func (this OpIdx) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpIdx) Geq

```go
func (this OpIdx) Geq(operand IAny) IExprBoolish
```
Geq implements `IExprOrdish`.

#### func (OpIdx) Gt

```go
func (this OpIdx) Gt(operand IAny) IExprBoolish
```
Gt implements `IExprOrdish`.

#### func (OpIdx) Incr1

```go
func (this OpIdx) Incr1() OpSet
```
Incr1 implements `IExprVarish`.

#### func (OpIdx) Leq

```go
func (this OpIdx) Leq(operand IAny) IExprBoolish
```
Leq implements `IExprOrdish`.

#### func (OpIdx) Lt

```go
func (this OpIdx) Lt(operand IAny) IExprBoolish
```
Lt implements `IExprOrdish`.

#### func (OpIdx) Minus

```go
func (this OpIdx) Minus(operand IAny) OpSub
```
Minus implements `IExprNumerish`.

#### func (OpIdx) Mod

```go
func (this OpIdx) Mod(operand IAny) OpMod
```
Mod implements `IExprNumerish`.

#### func (OpIdx) Neg

```go
func (this OpIdx) Neg() OpSub
```
Neg implements `IExprNumerish`.

#### func (OpIdx) Neq

```go
func (this OpIdx) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpIdx) Not

```go
func (this OpIdx) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpIdx) Of

```go
func (this OpIdx) Of(args ...IAny) *ExprCall
```
Of implements `IExprCallish`.

#### func (OpIdx) Or

```go
func (this OpIdx) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### func (OpIdx) Plus

```go
func (this OpIdx) Plus(operand IAny) OpAdd
```
Plus implements `IExprNumerish`.

#### func (OpIdx) Set

```go
func (this OpIdx) Set(operand IAny) OpSet
```
Set implements `IExprVarish`.

#### func (OpIdx) Sl

```go
func (this OpIdx) Sl(startIndex IAny, stopIndex IAny) OpIdx
```
Sl implements `IExprContainish`.

#### func (OpIdx) Times

```go
func (this OpIdx) Times(operand IAny) OpMul
```
Times implements `IExprNumerish`.

#### type OpLeq

```go
type OpLeq struct{ Op }
```

OpLeq represents Go's `<=` less-or-equal comparison operator.

#### func  Leq

```go
func Leq(operands ...ISyn) OpLeq
```
Leq constructs an `OpLeq`.

#### func (OpLeq) And

```go
func (this OpLeq) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpLeq) Eq

```go
func (this OpLeq) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpLeq) Neq

```go
func (this OpLeq) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpLeq) Not

```go
func (this OpLeq) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpLeq) Or

```go
func (this OpLeq) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### type OpLt

```go
type OpLt struct{ Op }
```

OpLt represents Go's `<` less-than comparison operator.

#### func  Lt

```go
func Lt(operands ...ISyn) OpLt
```
Lt constructs an `OpLt`.

#### func (OpLt) And

```go
func (this OpLt) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpLt) Eq

```go
func (this OpLt) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpLt) Neq

```go
func (this OpLt) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpLt) Not

```go
func (this OpLt) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpLt) Or

```go
func (this OpLt) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### type OpMod

```go
type OpMod struct{ Op }
```

OpMod represents one or more `%` modulos.

#### func  Mod

```go
func Mod(operands ...ISyn) OpMod
```
Mod constructs an `OpMod`.

#### type OpMul

```go
type OpMul struct{ Op }
```

OpMul represents one or more `*` multiplications.

#### func  Mul

```go
func Mul(operands ...ISyn) OpMul
```
Mul constructs an `OpMul`.

#### type OpNeq

```go
type OpNeq struct{ Op }
```

OpNeq represents Go's `!=` inequality comparison operator.

#### func  Neq

```go
func Neq(operands ...ISyn) OpNeq
```
Neq constructs an `OpNeq`.

#### func (OpNeq) And

```go
func (this OpNeq) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpNeq) Eq

```go
func (this OpNeq) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpNeq) Neq

```go
func (this OpNeq) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpNeq) Not

```go
func (this OpNeq) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpNeq) Or

```go
func (this OpNeq) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### type OpNot

```go
type OpNot struct{ Op }
```

OpNot represents Go's unary `!` operator.

#### func  Not

```go
func Not(operands ...ISyn) OpNot
```
Not constructs an `OpNot`.

#### func (OpNot) And

```go
func (this OpNot) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpNot) Eq

```go
func (this OpNot) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpNot) Neq

```go
func (this OpNot) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpNot) Not

```go
func (this OpNot) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpNot) Or

```go
func (this OpNot) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### type OpOr

```go
type OpOr struct{ Op }
```

OpOr represents Go's `||` boolean-or operator.

#### func  Or

```go
func Or(operands ...ISyn) OpOr
```
Or constructs an `OpOr`.

#### func (OpOr) And

```go
func (this OpOr) And(operand IAny) OpAnd
```
And implements `IExprBoolish`.

#### func (OpOr) Eq

```go
func (this OpOr) Eq(operand IAny) OpEq
```
Eq implements `IExprEqualish`.

#### func (OpOr) Neq

```go
func (this OpOr) Neq(operand IAny) OpNeq
```
Neq implements `IExprEqualish`.

#### func (OpOr) Not

```go
func (this OpOr) Not() OpNot
```
Not implements `IExprBoolish`.

#### func (OpOr) Or

```go
func (this OpOr) Or(operand IAny) OpOr
```
Or implements `IExprBoolish`.

#### type OpSet

```go
type OpSet struct{ Op }
```

OpSet represents Go's `=` assignment operator.

#### func  Set

```go
func Set(operands ...ISyn) OpSet
```
Set constructs an `OpSet`.

#### type OpSub

```go
type OpSub struct{ Op }
```

OpSub represents one or more `-` subtractions (or negation if unary).

#### func  Neg

```go
func Neg(operand ISyn) OpSub
```
Neg constructs an unary `OpSub` to represent the given `operand`'s negation.

#### func  Sub

```go
func Sub(operands ...ISyn) OpSub
```
Sub constructs an `OpSub`.

#### type PkgImports

```go
type PkgImports map[string]PkgName
```

PkgImports maps (via its `Ensure` method) package import paths to package import
names.

#### func (*PkgImports) Ensure

```go
func (this *PkgImports) Ensure(pkgImportPath string) (pkgImportName PkgName)
```
Ensure returns the `pkgImportName` for the given `pkgImportPath` as stored in
`this` (or if missing, devises one in the form of eg. `pkg__encoding_json` for
`encoding/json` and stores it, assuming that `PkgImportNamePrefix` is set to
"pkg__", its default value).

#### type PkgName

```go
type PkgName string
```

PkgName offers some handy methods on package import names.

#### func (PkgName) C

```go
func (this PkgName) C(funcName string, args ...IAny) *ExprCall
```
C constructs an `ExprCall` of the `funcName` exported by `this`
imported-package.

#### func (PkgName) N

```go
func (this PkgName) N(exportedName string, maybeFurtherDotOperands ...IAny) IExprDottish
```

#### func (PkgName) T

```go
func (this PkgName) T(typeName string) *TypeRef
```
T constructs a `TypeRef` with `Named` referring to `this PkgName` and
`typeName`.

#### func (PkgName) Tª

```go
func (this PkgName) Tª(typeName string) *TypeRef
```
Tª constructs a `TypeRef` with its `Pointer`'s `Named` referring to `this
PkgName` and `typeName`.

#### type SingleLineDocCommentParagraphs

```go
type SingleLineDocCommentParagraphs []string
```

SingleLineDocCommentParagraphs prepends doc-comments to a top-level `SynFunc`
being emitted. Each represents a "single-line-paragraph" that in the generated
output will be separated from the next via an empty `// ` line.

#### func (*SingleLineDocCommentParagraphs) Add

```go
func (this *SingleLineDocCommentParagraphs) Add(docCommentLines ...string)
```
Add is a convenience short-hand for `append`.

#### type SourceFile

```go
type SourceFile struct {
	// package name
	PkgName string

	// top-level definition declarations
	SynBlock
}
```

SourceFile is a simple collection of `ISyn`s representing top-level definition
declarations destined to be emitted into a single source file, plus the
`package` name for that file. Imports are handled outside, eg. by utilizing
`PkgImports`.

#### func  File

```go
func File(pkgName string, allocBodyCap int, topLevelDecls ...ISyn) *SourceFile
```
File constructs a `SourceFile`.

#### func (*SourceFile) CodeGen

```go
func (this *SourceFile) CodeGen(codeGenCommentNotice string, pkgImportPathsToNames PkgImports, emitNoOpFuncBodies bool, goFmt bool) (src []byte, goFmtTimeTaken time.Duration, goFmtErr error)
```
CodeGen generates the code via `this.CodeGenPlain()`, and then optionally
`go/format`s it. Any `error` returned is from `go/format`, and if so, `src` will
instead contain the original (non-formatted) generated code that was given to
`go/format` to aid investigating the issue.

#### func (*SourceFile) CodeGenPlain

```go
func (this *SourceFile) CodeGenPlain(codeGenCommentNotice string, pkgImportPathsToNames PkgImports, emitNoOpFuncBodies bool) []byte
```
CodeGenPlain generates the code represented by `this` into `src`, without
`go/format`ting it.

#### type StmtBreak

```go
type StmtBreak Named
```

StmtBreak represents Go's `break` keyword.

#### type StmtConst

```go
type StmtConst struct {
	// Name is required, Type optional
	NamedTyped
	// required literal constant, should be ExprLit or OpDot usually
	Expr ISyn
}
```

StmtConst represents Go's `const` keyword.

#### func  Const

```go
func Const(name string, maybeType *TypeRef, expr ISyn) (this *StmtConst)
```
Const constructs a `StmtConst`.

#### type StmtContinue

```go
type StmtContinue Named
```

StmtContinue represents Go's `continue` keyword.

#### type StmtDefer

```go
type StmtDefer struct {
	StmtUnary
}
```

StmtDefer represents Go's `defer` keyword.

#### func  Defer

```go
func Defer(call *ExprCall) (this StmtDefer)
```
Defer constructs a `StmtDefer`.

#### type StmtFor

```go
type StmtFor struct {
	// the loop body
	SynBlock

	// `for .. range`: used if at least `Iteree` set
	Range struct {
		// left-hand (key / index) var
		Key Named
		// right-hand (value) var
		Val Named
		// what to `range` over
		Over ISyn
	}
	// classical `for` loop: used if no `range`
	Loop struct {
		// one-off pre-loop initialization statement
		Init ISyn
		// pre-iteration condition-check predicate
		Cond ISyn
		// post-iteration statement
		Step ISyn
	}
}
```

StmtFor represents either a `for .. range` loop or a classical `for` (not
`range`) one.

#### func  For

```go
func For(maybeInit ISyn, maybeCond ISyn, maybeStep ISyn, body ...ISyn) (this *StmtFor)
```
For constructs a `StmtFor` that emits a classical `for` (not `range`) loop.

#### func  ForEach

```go
func ForEach(maybeIdx Named, maybeVal Named, iteree ISyn, body ...ISyn) (this *StmtFor)
```
ForEach constructs a `StmtFor` that emits a `for .. range` loop.

#### func (*StmtFor) Code

```go
func (this *StmtFor) Code(stmts ...ISyn) *StmtFor
```
Code sets `this.Body` and returns `this`.

#### type StmtGo

```go
type StmtGo struct {
	StmtUnary
}
```

StmtGo represents Go's `go` keyword.

#### func  Go

```go
func Go(call *ExprCall) (this StmtGo)
```
Go constructs a `StmtGo`.

#### type StmtGoTo

```go
type StmtGoTo Named
```

StmtGoTo represents Go's `goto` keyword.

#### func  GoTo

```go
func GoTo(name string) StmtGoTo
```
GoTo constructs a `StmtGoTo`.

#### type StmtIf

```go
type StmtIf struct {
	// one or more `if` or `else if` conditions
	// with their associated branches
	IfThens SynCases
	// optional final `else` branch
	Else SynBlock
}
```

StmtIf represents Go's `if .. else` construct.

#### func  If

```go
func If(ifThensAndMaybeAnElse ...ISyn) (this *StmtIf)
```
If constructs a `StmtIf` with `ifThensAndMaybeAnElse` containing 0 or more
alternating-pairs of `if` conditions and corresponding `then` branches, plus
optionally a final `else` branch.

Should any of the `if` conditions be `nil`, then `this` will return as `nil`.

#### type StmtLabel

```go
type StmtLabel struct {
	Named
	SynBlock
}
```

StmtLabel represents a label that one can `goto` or `break` from.

#### func  Label

```go
func Label(name string, stmts ...ISyn) *StmtLabel
```
Label constructs a `StmtLabel` with the given `name` and associated code
`SynBlock`.

#### type StmtRet

```go
type StmtRet struct {
	StmtUnary
}
```

StmtRet represents Go's `return` keyword.

#### func  Ret

```go
func Ret(retExpr ISyn) (this StmtRet)
```
Ret constructs a `StmtRet`. To have it generate `return nil`, your `retExpr`
should equal `B.Nil` (ie. an `ExprLit` with no `Val` set). If `nil` is passed
for `retExpr`, this generates an empty `return;` statement.

#### type StmtSwitch

```go
type StmtSwitch struct {
	// optional scrutinee
	Scrutinee ISyn
	// 0 or more `case` branches
	Cases SynCases
	// optional `default` branch
	Default SynBlock
}
```

StmtSwitch represents Go's `switch .. case` construct.

#### func  Switch

```go
func Switch(maybeScrutinee ISyn, caseCondsAndBlocksPlusMaybeDefaultBlock ...ISyn) (this *StmtSwitch)
```
Switch constructs a `StmtSwitch`.

#### func (*StmtSwitch) Case

```go
func (this *StmtSwitch) Case(cond ISyn, thens ...ISyn) *StmtSwitch
```
Case adds the given `case` branch to the `StmtSwitch.Cases` of `this`.

#### func (*StmtSwitch) CasesFrom

```go
func (this *StmtSwitch) CasesFrom(areAllSynCases bool, synCasesOrCondsAndThens ...ISyn) *StmtSwitch
```
CasesFrom adds to `this.Cases` and returns `this`. If `areAllSynCases`, each
`ISyn` is expected to be a `*SynCase` and added. Otherwise,
`synCasesOrCondsAndThens` are alternating pairs of `Cond`s-and-thens that are
used to construct the individual `SynCase`s to add.

#### func (*StmtSwitch) CasesOf

```go
func (this *StmtSwitch) CasesOf(cases ...SynCase) *StmtSwitch
```
CasesOf adds the given `case` branches to the `StmtSwitch.Cases` of `this`.

#### func (*StmtSwitch) DefaultCase

```go
func (this *StmtSwitch) DefaultCase(stmts ...ISyn) *StmtSwitch
```
DefaultCase sets the `default` branch of this `StmtSwitch`.

#### type StmtUnary

```go
type StmtUnary struct {
	// the keyword's argument: must be non-`nil`
	// `*ExprCall` for `StmtGo` / `StmtDefer`,
	// can be anything or nothing for `StmtRet`.
	Expr ISyn
}
```

StmtUnary is embedded by `StmtRet`, `StmtDefer`, `StmtGo`.

#### type StmtVar

```go
type StmtVar struct {
	// Name is required, Type optional
	NamedTyped
	// optional initialization-value expression
	Expr ISyn
}
```

StmtVar represents Go's `var` keyword.

#### func  Var

```go
func Var(name string, maybeType *TypeRef, maybeExpr ISyn) (this *StmtVar)
```
Var constructs a `StmtVar`.

#### type SynBlock

```go
type SynBlock struct {
	Body Syns
}
```

SynBlock represents a list of statements typically wrapped in curly-braces and
separated by `;` (pre-`gofmt`).

#### func  Block

```go
func Block(body ...ISyn) (this SynBlock)
```
Block constructs a `SynBlock`.

#### func (*SynBlock) Add

```go
func (this *SynBlock) Add(stmts ...ISyn)
```
Add is a convenience short-hand for `this.Body = append(this.Body,..)`.

#### type SynCase

```go
type SynCase struct {
	// some condition expression
	Cond ISyn
	// associated-branch statements
	SynBlock
}
```

SynCase represents a condition expression together with a block of statements,
used for both `StmtIf`s and `StmtSwitch`es.

#### func  Case

```go
func Case(cond ISyn, thens ...ISyn) *SynCase
```
Case constructs a `SynCase` as used in `StmtIf`s and `StmtSwitch`es.

#### type SynCases

```go
type SynCases []SynCase
```

SynCases is a slice of `SynCase`s.

#### func (*SynCases) Add

```go
func (this *SynCases) Add(cond ISyn, thens ...ISyn)
```
Add is a convenience short-hand for `append`.

#### type SynFunc

```go
type SynFunc struct {
	// the func's body of statements --- if it is lacking a
	// final `StmtRet` and all return values are named, one
	// will automatically appear at the end during code-gen
	SynBlock
	// optionally the func's `Name` (if top-level decl),
	// the `Type` must point to the func's signature
	// via its `TypeFunc`-typed `Func` field
	NamedTyped
	// optional (used if `Type` is
	// non-`nil`) method receiver
	Recv NamedTyped
	// doc comments for this func declaration
	Docs SingleLineDocCommentParagraphs
	// if true, emitted inside /* comment */
	EmitCommented bool
}
```

SynFunc represents either a top-level (named) func / method declaration, or an
anonymous func expression.

#### func  Fn

```go
func Fn(maybeRecv NamedTyped, name string, sig *TypeFunc, body ...ISyn) (this *SynFunc)
```
Fn constructs a `SynFunc`. If `maybeRecv` has a `Type` set, `this` represents a
method of that type.

#### func  Func

```go
func Func(name string, args ...NamedTyped) *SynFunc
```
Func constructs a `SynFunc` with the given `name` and `args`.

#### func (*SynFunc) Arg

```go
func (this *SynFunc) Arg(name string, typeRef *TypeRef) *SynFunc
```
Arg adds to `this.Type.Func.Args` and returns `this`.

#### func (*SynFunc) ArgIf

```go
func (this *SynFunc) ArgIf(onlyIf bool, arg NamedTyped) *SynFunc
```

#### func (*SynFunc) Args

```go
func (this *SynFunc) Args(args ...NamedTyped) *SynFunc
```
Args sets `this.Type.Func.Args` and returns `this`.

#### func (*SynFunc) Code

```go
func (this *SynFunc) Code(stmts ...ISyn) *SynFunc
```
Code adds to `this.SynBlock.Body` and returns `this`.

#### func (*SynFunc) Doc

```go
func (this *SynFunc) Doc(docCommentLines ...string) *SynFunc
```
Doc adds to `this.Docs` and returns `this`.

#### func (*SynFunc) EmitsCommented

```go
func (this *SynFunc) EmitsCommented(emitCommented bool) *SynFunc
```

#### func (*SynFunc) Ret

```go
func (this *SynFunc) Ret(name string, typeRef *TypeRef) *SynFunc
```
Ret adds to `this.Type.Func.Rets` and returns `this`.

#### func (*SynFunc) Rets

```go
func (this *SynFunc) Rets(rets ...NamedTyped) *SynFunc
```
Rets sets `this.Type.Func.Rets` and returns `this`.

#### func (*SynFunc) Sig

```go
func (this *SynFunc) Sig(sig *TypeFunc) *SynFunc
```
Sig sets `this.Type.Func` to `sig` and returns `this`.

#### func (*SynFunc) Spreads

```go
func (this *SynFunc) Spreads() *SynFunc
```

#### type SynRaw

```go
type SynRaw struct {
	Src         []byte
	ImportsUsed map[PkgName]string

	// if true, emitted inside /* comment */
	EmitCommented bool
}
```

SynRaw is an `ISyn` that at codegen time simply emits its self-contained raw Go
source-code (perhaps hardcoded or via `text/template`s or other means) directly.

#### type SynStructField

```go
type SynStructField struct {
	// field Name (optional) and Type
	NamedTyped
	// optional field tags
	Tags map[string]string
}
```

SynStructField represents one of a `TypeStruct`'s `Fields`.

#### func  TdStructFld

```go
func TdStructFld(name string, typeRef *TypeRef, tags map[string]string) (fld SynStructField)
```
TdStructFld constructs a `SynStructField` for `TypeStruct`s.

#### func (*SynStructField) JsonName

```go
func (this *SynStructField) JsonName() (name string)
```
JsonName returns `this.Tags["json"][:semicolon]` or `this.Name`.

#### type SynStructFields

```go
type SynStructFields []SynStructField
```


#### func (SynStructFields) Exists

```go
func (this SynStructFields) Exists(ok func(*SynStructField) bool) bool
```

#### func (SynStructFields) IndicesWhere

```go
func (this SynStructFields) IndicesWhere(ok func(*SynStructField) bool) (indices []int)
```

#### func (SynStructFields) NTs

```go
func (this SynStructFields) NTs() (nts NamedsTypeds)
```

#### func (SynStructFields) NamedOnly

```go
func (this SynStructFields) NamedOnly() (named SynStructFields)
```

#### type Syns

```go
type Syns []ISyn
```

Syns is a slice of `ISyn`s.

#### func  Else

```go
func Else(body ...ISyn) Syns
```
Else simply returns `body`, just like `Then` does: it's only readability sugar
for use in `If` (or `GEN_IF`) calls.

#### func  GEN_FOR

```go
func GEN_FOR(start int, whileLessThan int, incrementBy int, do func(int) ISyn) (yield Syns)
```
GEN_FOR_IN is a codegen-time iterating `Syns`-builder. It calls `do` once per
iteration with the current index, which is equal to `start` in the very first
iteration, never less-than zero and always less-than `whileLessThan`.

#### func  GEN_FOR_IN

```go
func GEN_FOR_IN(sl []IAny, step int, each func(int, []IAny) ISyn) (yield Syns)
```
GEN_FOR_IN is a codegen-time iterating `Syns`-builder. Traversing `sl` with the
given `step` skip-length, it calls `each` once per iteration with the current
index into `sl` and the next sub-slice of `sl` (at that index) that has a `len`
of usually `step` and always greater than zero (but it might be less than `step`
in the very last iteration depending on the `len` of `sl`).

#### func  GEN_IF

```go
func GEN_IF(check bool, stmts ...ISyn) Syns
```
GEN_IF returns either none, all, or one of `stmts` depending on `check` and as
follows:

- if there are 2 `stmts` and _each one_ is a `Syns`, they're **then/else**-like
and one returns

- otherwise: if `check` is `true`, all `stmts` are returned, else `nil` is
returned

#### func  SynsFrom

```go
func SynsFrom(eitherSyn ISyn, orThings ...IAny) Syns
```

#### func  Then

```go
func Then(body ...ISyn) Syns
```
Then simply returns `body`, just like `Else` does: it's only readability sugar
for use in `If` (or `GEN_IF`) calls.

#### func (*Syns) Add

```go
func (this *Syns) Add(syns ...ISyn)
```
Add is a convenience short-hand for `append`.

#### func (Syns) Transform

```go
func (this Syns) Transform(transform func(ISyn) ISyn) Syns
```

#### type TypeDecl

```go
type TypeDecl struct {
	// denotes type name and underlying type
	NamedTyped
	// denotes whether alias (`=`) or not
	IsAlias bool
}
```

TypeDecl represents a type-definition declaration or type-alias declaration.

#### func  TDecl

```go
func TDecl(name string, typeRef *TypeRef, isAlias bool) (this *TypeDecl)
```
TDecl constructs a named `TypeDecl` of the specified underlying type.

#### type TypeFunc

```go
type TypeFunc struct {
	// func arguments
	Args NamedsTypeds
	// func return values
	Rets NamedsTypeds

	LastArgSpreads bool
}
```

TypeFunc represents a func signature.

#### func  TdFn

```go
func TdFn(args NamedsTypeds, rets ...NamedTyped) *TypeFunc
```
TdFn constructs a `TypeFunc`,

#### func  TdFunc

```go
func TdFunc(args ...NamedTyped) *TypeFunc
```
TdFunc constructs an initially-empty (arg-less and return-less) `TypeFunc`,

#### func (*TypeFunc) Arg

```go
func (this *TypeFunc) Arg(name string, typeRef *TypeRef) *TypeFunc
```
Arg adds to `this.Args` and returns `this`.

#### func (*TypeFunc) Ret

```go
func (this *TypeFunc) Ret(name string, typeRef *TypeRef) *TypeFunc
```
Ret adds to `this.Rets` and returns `this`.

#### func (*TypeFunc) Spreads

```go
func (this *TypeFunc) Spreads() *TypeFunc
```

#### func (*TypeFunc) T

```go
func (this *TypeFunc) T() *TypeRef
```
T constructs a `TypeRef` whose `Func` points to `this`.

#### type TypeInterface

```go
type TypeInterface struct {
	// 0-or-more embedded interfaces,
	// each denoted via `TypeRef.Named`
	Embeds []*TypeRef
	// named methods, with `Type` detailing each method's
	// signature via its `TypeFunc`-typed `Func` field
	Methods NamedsTypeds
}
```

TypeInterface represents Go's `interface{..}` construct.

#### func  TdInterface

```go
func TdInterface(embeds []*TypeRef, methods ...NamedTyped) *TypeInterface
```
TdInterface constructs a `TypeInterface`.

#### type TypeRef

```go
type TypeRef struct {
	Pointer struct {
		Of *TypeRef
	}
	ArrOrSlice struct {
		Of         *TypeRef
		IsFixedLen ISyn
		IsEllipsis bool
	}
	Map struct {
		OfKey *TypeRef
		ToVal *TypeRef
	}
	Chan struct {
		Of      *TypeRef
		DirRecv bool
		DirSend bool
	}
	Func      *TypeFunc
	Interface *TypeInterface
	Struct    *TypeStruct
	Named     struct {
		PkgName  string // empty ("") if Go-native (built-in) or package-local (non-import) type
		TypeName string
	}
}
```

TypeRef represents a reference to a type, such as used for func arguments' or
struct fields' explicit type annotations.

#### func  TArray

```go
func TArray(numElems ISyn, typeRef *TypeRef) *TypeRef
```
TArray constructs a `TypeRef` referring to an array of the specified type.

#### func  TChan

```go
func TChan(of *TypeRef, dirRecv bool, dirSend bool) *TypeRef
```
TChan constructs a `TypeRef` referring to the specified channel. TODO:
TypeRef.emitTo implementation!

#### func  TFrom

```go
func TFrom(pkgName PkgName, typeName string) (this *TypeRef)
```
TFrom constructs a `TypeRef` referring to the specified named type exported from
the given package.

#### func  TFunc

```go
func TFunc(typeFunc *TypeFunc) *TypeRef
```
TFunc constructs a `TypeRef` referring to the specified unnamed `func(..)(..)`
signature.

#### func  TInterface

```go
func TInterface(typeIface *TypeInterface) *TypeRef
```
TInterface constructs a `TypeRef` referring to the specified unnamed
`interface{..}`.

#### func  TLocal

```go
func TLocal(typeName string) (this *TypeRef)
```
TFrom constructs a `TypeRef` referring to the specified named type in the local
package.

#### func  TMap

```go
func TMap(ofKey *TypeRef, toVal *TypeRef) (this *TypeRef)
```
TMap constructs a `TypeRef` referring to a map with the specified key and value
types.

#### func  TPointer

```go
func TPointer(typeRef *TypeRef) *TypeRef
```
TPointer constructs a `TypeRef` referring to a pointer to the specified type.

#### func  TSlice

```go
func TSlice(typeRef *TypeRef) *TypeRef
```
TSlice constructs a `TypeRef` referring to a slice of the specified type.

#### func  TStruct

```go
func TStruct(typeStruct *TypeStruct) *TypeRef
```
TStruct constructs a `TypeRef` referring to the specified unnamed `struct{..}`.

#### func (*TypeRef) BitSizeIfBuiltInNumberType

```go
func (this *TypeRef) BitSizeIfBuiltInNumberType() int
```
BitSizeIfBuiltInNumberType returns 8 for `int8`, `byte`, `uint8`, or 16, 32, 64,
128 as applicable, recognizing only direct `Named` refs to Go' native `builtin`
number types (no type-alias dereferencing yet).

#### func (*TypeRef) From

```go
func (this *TypeRef) From(expr IAny) *ExprCall
```
From constructs an `ExprCall` that represents a conversion of `expr` into `this`
type. (Returns `ExprCall` because Go's conversion syntax, eg. `int(myexpr)`, is
covered by it due to the same emitting logic.)

#### func (*TypeRef) IsBuiltinPrimType

```go
func (this *TypeRef) IsBuiltinPrimType(orIsUnderlyingBuiltinPrimType bool) bool
```
IsBuiltinPrimType returns whether `this` refers to one of Go's built-in
primitive-types such as `bool`, `string` etc. (If
`orIsUnderlyingBuiltinPrimType`, it walks the `ArrOrSlice` / `Pointer` / `Map` /
`Chan` as applicable.)

#### func (*TypeRef) IsNamedAndPublic

```go
func (this *TypeRef) IsNamedAndPublic() bool
```

#### func (*TypeRef) IsZeroish

```go
func (this *TypeRef) IsZeroish(exprOfThisType ISyn, canLen bool, canNum bool) ISyn
```

#### func (*TypeRef) IsntZeroish

```go
func (this *TypeRef) IsntZeroish(exprOfThisType ISyn, canLen bool, canNum bool) (expr ISyn)
```

#### func (*TypeRef) Method

```go
func (this *TypeRef) Method(name string, args ...NamedTyped) *SynFunc
```
Method constructs a `SynFunc` with the given `name` and `args` plus a
`this`-typed method `Recv` also named `"this"`.

#### func (*TypeRef) N

```go
func (this *TypeRef) N(name string) NamedTyped
```
N constructs a `NamedTyped` based on `name` and `this` type.

#### func (*TypeRef) String

```go
func (this *TypeRef) String() string
```

#### func (*TypeRef) UltimateElemType

```go
func (this *TypeRef) UltimateElemType() (tEl *TypeRef)
```

#### type TypeStruct

```go
type TypeStruct struct {
	// named fields and un-named ones ("embeds")
	Fields SynStructFields
}
```

TypeStruct represents Go's `struct{..}` construct.

#### func  TdStruct

```go
func TdStruct(fields ...SynStructField) *TypeStruct
```
TdStruct constructs a `TypeStruct`.

#### func (*TypeStruct) Field

```go
func (this *TypeStruct) Field(name string, tryJsonNamesToo bool) (fld *SynStructField)
```
Field returns the `SynStructField` in `this.Fields` matching `name`.

#### type UNLESS

```go
type UNLESS map[bool]ISyn
```

UNLESS serves as a codegen-time readability wrapper for `GEN_BYCASE` callers.

#### type USUALLY

```go
type USUALLY ISyn
```

USUALLY serves as a codegen-time readability wrapper for `GEN_BYCASE` callers.
