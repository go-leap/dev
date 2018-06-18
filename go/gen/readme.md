# udevgogen
--
    import "github.com/go-leap/dev/go/gen"


## Usage

```go
var (

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
		Err Named
		// `this`
		This Named
		// `ret`
		Ret Named
	}

	// singletons for common type-refs
	T struct {
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

#### type DocCommentSingleLineParagraphs

```go
type DocCommentSingleLineParagraphs []string
```

DocCommentSingleLineParagraphs prepends doc-comments to a top-level `SynFunc`
being `Emit`ted. Each represents a "single-line-paragraph" that in the generated
output will be separated from the next via an empty `// ` line.

#### func (*DocCommentSingleLineParagraphs) Add

```go
func (this *DocCommentSingleLineParagraphs) Add(docCommentLines ...string)
```
Add is a convenience short-hand for `append`.

#### type ExprCall

```go
type ExprCall struct {
	Callee ISyn
	Args   []ISyn
}
```

ExprCall represents a call to any callable `Callee`, or a type conversion (if
`Callee` effectively names a type).

#### func  Call

```go
func Call(callee ISyn, args ...ISyn) *ExprCall
```
Call constructs an `ExprCall`.

#### func (*ExprCall) Emit

```go
func (this *ExprCall) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (ExprLit) Emit

```go
func (this ExprLit) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

#### type ExprNil

```go
type ExprNil struct {
}
```

ExprNil represents Go's `nil` built-in value.

#### func (ExprNil) Emit

```go
func (ExprNil) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

#### type ISyn

```go
type ISyn interface {
	// generates the code represented by this `ISyn`
	Emit(IWriter)
}
```

ISyn implementations represent some element in the Abstract Syntax Tree:
literals, vars, consts, type-defs, type-refs, funcs, keywords, operators etc..

#### func  A

```go
func A(argsOrOperandsOrStmts ...ISyn) []ISyn
```
A is merely a handy convenience short-hand to create a slice of `ISyn`s, as
sometimes needed for listing arguments, operands, or statements.

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

IWriter represents the buffer or other output stream that any `ISyn` can `Emit`
code to.

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

#### func (Named) Emit

```go
func (this Named) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func  Nt

```go
func Nt(name string, t *TypeRef) NamedTyped
```
Nt constructs a `NamedTyped`.

#### type NamedsTypeds

```go
type NamedsTypeds []NamedTyped
```

NamedsTypeds is a slice of 0-or-more `NamedTyped`s.

#### type Op

```go
type Op struct {
	// 1 or more operands: if 1 then
	// unary syntax output, else n-ary
	Operands []ISyn
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

#### func (OpAdd) Emit

```go
func (this OpAdd) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpAddr) Emit

```go
func (this OpAddr) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpAnd) Emit

```go
func (this OpAnd) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpComma) Emit

```go
func (this OpComma) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpDecl) Emit

```go
func (this OpDecl) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpDeref) Emit

```go
func (this OpDeref) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpDiv) Emit

```go
func (this OpDiv) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpDot) Emit

```go
func (this OpDot) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpEq) Emit

```go
func (this OpEq) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpGeq) Emit

```go
func (this OpGeq) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpGt) Emit

```go
func (this OpGt) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpIdx) Emit

```go
func (this OpIdx) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpLeq) Emit

```go
func (this OpLeq) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpLt) Emit

```go
func (this OpLt) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpMul) Emit

```go
func (this OpMul) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpNeq) Emit

```go
func (this OpNeq) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpNot) Emit

```go
func (this OpNot) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpOr) Emit

```go
func (this OpOr) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpSet) Emit

```go
func (this OpSet) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (OpSub) Emit

```go
func (this OpSub) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

#### type PkgImports

```go
type PkgImports map[string]string
```

PkgImports maps (via its `I` method) package import paths to package import
names.

#### func (*PkgImports) I

```go
func (this *PkgImports) I(pkgImportPath string) (pkgImportName string)
```
Returns the `pkgImportName` for the given `pkgImportPath` as stored in `this`
(or if missing, devises one in the form of eg. `encodingjson` for
`encoding/json` and stores it).

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
func File(pkgName string, topLevelDecls ...ISyn) *SourceFile
```
File constructs a `SourceFile`.

#### func (*SourceFile) Emit

```go
func (this *SourceFile) Emit(w IWriter, codeGenCommentNotice string, pkgImportPathsToNames PkgImports)
```
Emit generates the code represented by `this`.

#### func (*SourceFile) Src

```go
func (this *SourceFile) Src(codeGenCommentNotice string, emitNoOpFuncBodies bool, pkgImportPathsToNames PkgImports) (src []byte, err error)
```
Src calls `this.Emit` to generate the code into `src`, and then `go/format`s it.
Any `err` returned is from `go/format`, and if so, `src` will instead contain
the original non-formatted generated code to aid troubleshooting the issue.

#### type StmtBreak

```go
type StmtBreak struct{}
```

StmtBreak represents Go's `break` keyword.

#### func (StmtBreak) Emit

```go
func (StmtBreak) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (*StmtConst) Emit

```go
func (this *StmtConst) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

#### type StmtContinue

```go
type StmtContinue struct{}
```

StmtContinue represents Go's `continue` keyword.

#### func (StmtContinue) Emit

```go
func (StmtContinue) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (StmtDefer) Emit

```go
func (this StmtDefer) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (*StmtFor) Emit

```go
func (this *StmtFor) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (StmtGo) Emit

```go
func (this StmtGo) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

#### type StmtIf

```go
type StmtIf struct {
	// one or more `if` or `else if` conditions
	// with their associated branches
	IfThens []SynCond
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

#### func (*StmtIf) Emit

```go
func (this *StmtIf) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (StmtRet) Emit

```go
func (this StmtRet) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

#### type StmtSwitch

```go
type StmtSwitch struct {
	// optional scrutinee
	Scrutinee ISyn
	// 0 or more `case` branches
	Cases []SynCond
	// optional `default` branch
	Default SynBlock
}
```

StmtSwitch represents Go's `switch .. case` construct.

#### func  Switch

```go
func Switch(maybeCond ISyn, caseCondsAndBlocksPlusMaybeDefaultBlock ...ISyn) (this *StmtSwitch)
```
Switch constructs a `StmtSwitch`.

#### func (*StmtSwitch) Emit

```go
func (this *StmtSwitch) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (*StmtVar) Emit

```go
func (this *StmtVar) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

#### type SynBlock

```go
type SynBlock struct {
	Body []ISyn
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
Add is a convenience short-hand for `append`.

#### func (SynBlock) Emit

```go
func (this SynBlock) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### type SynFunc

```go
type SynFunc struct {
	// the func's body of statements (a tailing
	// `StmtRet` will always be implicitly present
	// and should not be explicitly included in here)
	SynBlock
	// optionally the func's `Name` (if top-level decl),
	// the `Type` must point to the func's signature
	// via its `TypeFunc`-typed `Func` field
	NamedTyped
	// optional (used if `Type` is
	// non-`nil`) method receiver
	Recv NamedTyped
	// doc comments for this func declaration
	Doc DocCommentSingleLineParagraphs
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

#### func (*SynFunc) Emit

```go
func (this *SynFunc) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (*SynStructField) Emit

```go
func (this *SynStructField) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (TypeDecl) Emit

```go
func (this TypeDecl) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (*TypeFunc) Emit

```go
func (this *TypeFunc) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (*TypeInterface) Emit

```go
func (this *TypeInterface) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

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

#### func (*TypeRef) Emit

```go
func (this *TypeRef) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.

#### func (*TypeRef) IsBuiltinPrimType

```go
func (this *TypeRef) IsBuiltinPrimType(orIsUnderlyingBuiltinPrimType bool) bool
```
IsBuiltinPrimType returns whether `this` refers to one of Go's built-in
primitive-types such as `bool`, `byte`, `uint`, `string` etc. (If
`orIsUnderlyingBuiltinPrimType`, it walks the `Slice` / `Ptr` / `Map` as
applicable.)

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

#### func (*TypeStruct) Emit

```go
func (this *TypeStruct) Emit(w IWriter)
```
Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.