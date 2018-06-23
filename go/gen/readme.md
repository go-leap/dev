# udevgogen
--
    import "github.com/go-leap/dev/go/gen"

Package github.com/go-leap/dev/go/gen provides AST constructs for generating Go
code. These are by design much simpler and leaner than `go/ast`, given that the
latter is designed to represent existing & parsed code, while the former is for
on-the-fly construction of newly-to-be-emitted code.

Some language primitives I haven't needed to emit yet aren't covered yet, to be
added when they're first needed (such as fixed-size-array types or `chan`nels).

As a noteworthy goodie, all `func`s that have named return values automatically
get a final `return` statement appended to their `Body` at code-gen time, if
they don't already have one.

## Usage

```go
var (

	// see `PkgImports.Ensure(string) string` for details
	PkgImportNamePrefix = "pkg__"

	// intended to remain zero-valued (Name="" and Type=nil)
	NoMethodRecv NamedTyped

	// singletons for simple (operand-less / arg-less) keywords
	K struct {
		Break    StmtBreak
		Continue StmtContinue
		Ret      StmtRet
	}

	// singletons for stdlib-builtins
	B struct {
		Nil ExprNil

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
	V struct {
		// `err`
		Err NamedTyped
		// `this`
		This Named
		// `ret`
		Ret Named
	}

	// singletons for common type-refs
	T struct {
		// empty interface{}
		Interface *TypeRef

		Bool       *TypeRef
		Byte       *TypeRef
		Complex64  *TypeRef
		Complex128 *TypeRef
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

#### type ExprCall

```go
type ExprCall struct {
	Callee ISyn
	Args   Syns
}
```

ExprCall represents a call to any callable `Callee`, or a type conversion (if
`Callee` effectively names a type).

#### func  Call

```go
func Call(callee ISyn, args ...ISyn) *ExprCall
```
Call constructs an `ExprCall`.

#### type ExprLit

```go
type ExprLit struct {
	Val interface{}
}
```

ExprLit represents any literal constant value, such as a string, rune, number or
boolean.

#### func  L

```go
func L(lit interface{}) ExprLit
```
L constructs an `ExprLit`.

#### type ExprNil

```go
type ExprNil struct {
}
```

ExprNil represents Go's `nil` built-in value.

#### type ISyn

```go
type ISyn interface {
	// contains filtered or unexported methods
}
```

ISyn implementations represent some element in the Abstract Syntax Tree:
literals, vars, consts, type-defs, type-refs, funcs, keywords, operators etc..

#### type Named

```go
type Named struct{ Name string }
```

Named `Emit`s its `Name` during code-generation as-is, hence useful for
referring to named vars, consts, types, funcs etc.

#### func  N

```go
func N(name string) Named
```
N constructs a `Named`.

#### func (Named) Typed

```go
func (this Named) Typed(typeRef *TypeRef) (nt NamedTyped)
```
Typed returns a `NamedTyped` with `this.Name` and `typeRef`.

#### type NamedTyped

```go
type NamedTyped struct {
	Named
	Type *TypeRef
}
```

NamedTyped details a `Name` and a `TypeRef`, such as needed for func args,
return values, struct fields etc.

#### func  NT

```go
func NT(name string, t *TypeRef) NamedTyped
```
NT constructs a `NamedTyped`.

#### type NamedsTypeds

```go
type NamedsTypeds []NamedTyped
```

NamedsTypeds is a slice of 0-or-more `NamedTyped`s.

#### func  NTs

```go
func NTs(namesAndTypeRefs ...interface{}) (nts NamedsTypeds)
```
NTs is merely a handy convenience short-hand to create a slice of `NamedTyped`s.
`namesAndTypeRefs` must be alternating: `string`, `*TypeRef`, `string`,
`*TypeRef`, etc.

#### func (*NamedsTypeds) Add

```go
func (this *NamedsTypeds) Add(name string, typeRef *TypeRef)
```
Add is a convenience short-hand for `append`.

#### func (NamedsTypeds) AllNamed

```go
func (this NamedsTypeds) AllNamed() bool
```
AllNamed returns whether all `NamedTyped`s in `this` have a `Name` set.

#### func (NamedsTypeds) AllTyped

```go
func (this NamedsTypeds) AllTyped() bool
```
AllTyped returns whether all `NamedTyped`s in `this` have a `Type` set.

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

#### type OpComma

```go
type OpComma struct{ Op }
```

OpComma emits all its operands separated by `,` commas.

#### func  C

```go
func C(operands ...ISyn) OpComma
```
C constructs an `OpComma`.

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

#### type OpIdx

```go
type OpIdx struct{ Op }
```

OpIdx represents one or more `operand0[operand1][operand2]` indexers.

#### func  I

```go
func I(operands ...ISyn) OpIdx
```
I constructs an `OpIdx`.

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
type PkgImports map[string]string
```

PkgImports maps (via its `Ensure` method) package import paths to package import
names.

#### func (*PkgImports) Ensure

```go
func (this *PkgImports) Ensure(pkgImportPath string) (pkgImportName string)
```
Ensure returns the `pkgImportName` for the given `pkgImportPath` as stored in
`this` (or if missing, devises one in the form of eg. `pkg__encoding_json` for
`encoding/json` and stores it, assuming that `PkgImportNamePrefix` is set to
"pkg__", its default value).

#### type SingleLineDocCommentParagraphs

```go
type SingleLineDocCommentParagraphs []string
```

SingleLineDocCommentParagraphs prepends doc-comments to a top-level `SynFunc`
being `Emit`ted. Each represents a "single-line-paragraph" that in the generated
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
type StmtBreak struct{}
```

StmtBreak represents Go's `break` keyword.

#### type StmtConst

```go
type StmtConst struct {
	// Name is required, Type optional
	NamedTyped
	// required literal constant
	Expr ExprLit
}
```

StmtConst represents Go's `const` keyword.

#### func  Const

```go
func Const(name string, maybeType *TypeRef, exprLit ExprLit) (this *StmtConst)
```
Const constructs a `StmtConst`.

#### type StmtContinue

```go
type StmtContinue struct{}
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
		Idx Named
		// right-hand (value) var
		Val Named
		// what to `range` over
		Iteree ISyn
	}
	// classical `for` loop: used if no `range`
	Loop struct {
		// one-off pre-loop initialization statement
		Init ISyn
		// pre-iteration condition-check predicate
		Cond ISyn
		// post-iteration statement
		Each ISyn
	}
}
```

StmtFor represents either a `for .. range` loop or a classical `for` (not
`range`) one.

#### func  ForLoop

```go
func ForLoop(maybeInit ISyn, maybeCond ISyn, maybeEach ISyn, body ...ISyn) (this *StmtFor)
```
ForLoop constructs a `StmtFor` that emits a classical `for` (not `range`) loop.

#### func  ForRange

```go
func ForRange(maybeIdx Named, maybeVal Named, iteree ISyn, body ...ISyn) (this *StmtFor)
```
ForRange constructs a `StmtFor` that emits a `for .. range` loop.

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

#### type StmtIf

```go
type StmtIf struct {
	// one or more `if` or `else if` conditions
	// with their associated branches
	IfThens SynConds
	// optional final `else` branch
	Else SynBlock
}
```

StmtIf represents Go's `if .. else` construct.

#### func  If

```go
func If(cond ISyn, thens ...ISyn) *StmtIf
```
If constructs a simple `StmtIf` with a single condition and `then` branch (plus
initially empty `else` branch).

#### func  Ifs

```go
func Ifs(ifThensAndMaybeAnElse ...ISyn) (this *StmtIf)
```
Ifs constructs a more complex `StmtIf` than `If` does, with
`ifThensAndMaybeAnElse` containing 0 or more alternating pairs of `if` (or `else
if`) conditions and corresponding `then` branches (each a `SynBlock`), plus
optionally a final `else` branch (also a `SynBlock`).

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
Ret constructs a `StmtRet`.

#### type StmtSwitch

```go
type StmtSwitch struct {
	// optional scrutinee
	Scrutinee ISyn
	// 0 or more `case` branches
	Cases SynConds
	// optional `default` branch
	Default SynBlock
}
```

StmtSwitch represents Go's `switch .. case` construct.

#### func  Switch

```go
func Switch(maybeCond ISyn, casesCap int, caseCondsAndBlocksPlusMaybeDefaultBlock ...ISyn) (this *StmtSwitch)
```
Switch constructs a `StmtSwitch`.

#### type StmtUnary

```go
type StmtUnary struct {
	// the keyword's argument: must be non-`nil`
	// `*ExprCall` for `StmtGo` / `StmtDefer`,
	// can be anything incl. `nil` for `StmtRet`.
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

#### type SynCond

```go
type SynCond struct {
	// some condition expression
	Cond ISyn
	// associated-branch statements
	SynBlock
}
```

SynCond represents a condition expression together with a block of statements,
used for both `StmtIf`s and `StmtSwitch`es.

#### func  Cond

```go
func Cond(cond ISyn, thens ...ISyn) (this SynCond)
```
Cond constructs a `SynCond` as used in `StmtIf`s and `StmtSwitch`es.

#### type SynConds

```go
type SynConds []SynCond
```

SynConds is a slice of `SynCond`s.

#### func (*SynConds) Add

```go
func (this *SynConds) Add(cond ISyn, thens ...ISyn)
```
Add is a convenience short-hand for `append`.

#### type SynFunc

```go
type SynFunc struct {
	// the func's body of statements --- if it is missing
	// a final `StmtRet` and all return values are named,
	// one will be automatically appended at code-gen time
	SynBlock
	// optionally the func's `Name` (if top-level decl),
	// the `Type` must point to the func's signature
	// via its `TypeFunc`-typed `Func` field
	NamedTyped
	// optional (used if `Type` is
	// non-`nil`) method receiver
	Recv NamedTyped
	// doc comments for this func declaration
	Doc SingleLineDocCommentParagraphs
}
```

SynFunc represents either a top-level (named) func / method declaration, or an
anonymous func expression.

#### func  Fn

```go
func Fn(maybeRecv NamedTyped, name string, sig *TypeFunc, body ...ISyn) (this *SynFunc)
```
Fn constructs a `SynFunc`. If `maybeRecv` is given, it will represent a method
of that type.

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

#### type Syns

```go
type Syns []ISyn
```

Syns is a slice of `ISyn`s.

#### func  A

```go
func A(argsOrOperandsOrStmts ...ISyn) Syns
```
A is merely a handy convenience short-hand to create a slice of `ISyn`s, as
sometimes needed for listing arguments, operands, or statements.

#### func (*Syns) Add

```go
func (this *Syns) Add(syns ...ISyn)
```
Add is a convenience short-hand for `append`.

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
func TDecl(name string, typeRef *TypeRef, isAlias bool) (this TypeDecl)
```
TDecl constructs a named `TypeDecl` of the specified underlying type.

#### type TypeFunc

```go
type TypeFunc struct {
	// func arguments
	Args NamedsTypeds
	// func return values
	Rets NamedsTypeds
}
```

TypeFunc represents a func signature.

#### func  TdFunc

```go
func TdFunc(args NamedsTypeds, rets ...NamedTyped) *TypeFunc
```
TdFunc constructs a `TypeFunc`,

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
	Slice *TypeRef // slice-of-foo
	Ptr   *TypeRef // pointer-to-foo
	Map   struct {
		Key *TypeRef
		Val *TypeRef
	}
	Func      *TypeFunc
	Interface *TypeInterface
	Struct    *TypeStruct
	Named     struct {
		PkgName  string // empty if package-local (not imported) type
		TypeName string
	}
}
```

TypeRef represents a reference to a type, such as used for func arguments' or
struct fields' explicit type annotations.

#### func  TrFunc

```go
func TrFunc(typeFunc *TypeFunc) *TypeRef
```
TrFunc constructs a `TypeRef` referring to the specified unnamed `func(..)(..)`
signature.

#### func  TrInterface

```go
func TrInterface(typeIface *TypeInterface) *TypeRef
```
TrInterface constructs a `TypeRef` referring to the specified unnamed
`interface{..}`.

#### func  TrMap

```go
func TrMap(keyType *TypeRef, valType *TypeRef) (this *TypeRef)
```
TrMap constructs a `TypeRef` referring to a map with the specified key and value
types.

#### func  TrNamed

```go
func TrNamed(pkgName string, typeName string) (this *TypeRef)
```
TrNamed constructs a `TypeRef` referring to the specified named type.

#### func  TrPtr

```go
func TrPtr(typeRef *TypeRef) *TypeRef
```
TrPtr constructs a `TypeRef` referring to a pointer to the specified type.

#### func  TrSlice

```go
func TrSlice(typeRef *TypeRef) *TypeRef
```
TrSlice constructs a `TypeRef` referring to a slice of the specified type.

#### func  TrStruct

```go
func TrStruct(typeStruct *TypeStruct) *TypeRef
```
TrStruct constructs a `TypeRef` referring to the specified unnamed `struct{..}`.

#### func (*TypeRef) IsBuiltinPrimType

```go
func (this *TypeRef) IsBuiltinPrimType(orIsUnderlyingBuiltinPrimType bool) bool
```
IsBuiltinPrimType returns whether `this` refers to one of Go's built-in
primitive-types such as `bool`, `byte`, `uint`, `string` etc. (If
`orIsUnderlyingBuiltinPrimType`, it walks the `Slice` / `Ptr` / `Map` as
applicable.)

#### func (*TypeRef) SafeBitSizeIfBuiltInNumberType

```go
func (me *TypeRef) SafeBitSizeIfBuiltInNumberType() int
```

#### type TypeStruct

```go
type TypeStruct struct {
	// named fields and un-named ones ("embeds")
	Fields []SynStructField
}
```

TypeStruct represents Go's `struct{..}` construct.

#### func  TdStruct

```go
func TdStruct(fields ...SynStructField) *TypeStruct
```
TdStruct constructs a `TypeStruct`.
