package udevgogen

// Named `Emit`s its `Name` during code-generation as-is, hence
// useful for referring to named vars, consts, types, funcs etc.
type Named struct{ Name string }

// T returns a `NamedTyped` with `this.Name` and `typeRef`.
func (this Named) T(typeRef *TypeRef) (nt NamedTyped) {
	nt.Named, nt.Type = this, typeRef
	return
}

// NamedTyped details a `Name` and a `TypeRef`, such as
// needed for func args, return values, struct fields etc.
type NamedTyped struct {
	Named
	Type *TypeRef
}

// NamedsTypeds is a slice of 0-or-more `NamedTyped`s.
type NamedsTypeds []NamedTyped

// Add is a convenience short-hand for `append`.
func (this *NamedsTypeds) Add(name string, typeRef *TypeRef) {
	*this = append(*this, NamedTyped{Type: typeRef, Named: Named{Name: name}})
}

// AllNamed returns whether all `NamedTyped`s in `this` have a `Name` set.
func (this NamedsTypeds) AllNamed() bool {
	for i := range this {
		if this[i].Name == "" {
			return false
		}
	}
	return len(this) > 0
}

// AllTyped returns whether all `NamedTyped`s in `this` have a `Type` set.
func (this NamedsTypeds) AllTyped() bool {
	for i := range this {
		if this[i].Type == nil {
			return false
		}
	}
	return len(this) > 0
}

// TypeFunc represents a func signature.
type TypeFunc struct {
	// func arguments
	Args NamedsTypeds
	// func return values
	Rets NamedsTypeds
}

// TypeInterface represents Go's `interface{..}` construct.
type TypeInterface struct {
	// 0-or-more embedded interfaces,
	// each denoted via `TypeRef.Named`
	Embeds []*TypeRef
	// named methods, with `Type` detailing each method's
	// signature via its `TypeFunc`-typed `Func` field
	Methods NamedsTypeds
}

// TypeStruct represents Go's `struct{..}` construct.
type TypeStruct struct {
	// named fields and un-named ones ("embeds")
	Fields []SynStructField
}

// SynStructField represents one of a `TypeStruct`'s `Fields`.
type SynStructField struct {
	// field Name (optional) and Type
	NamedTyped
	// optional field tags
	Tags map[string]string
}

// TypeDecl represents a type-definition
// declaration or type-alias declaration.
type TypeDecl struct {
	// denotes type name and underlying type
	NamedTyped
	// denotes whether alias (`=`) or not
	IsAlias bool
}

// TypeRef represents a reference to a  type,
// such as used for func arguments' or struct
// fields' explicit type annotations.
type TypeRef struct {
	PtrTo        *TypeRef // pointer-to-foo
	ArrOrSliceOf struct {
		Val        *TypeRef
		IsFixedLen *uint64
		IsEllipsis bool
	}
	MapOf struct { // etc...
		Key *TypeRef
		Val *TypeRef
	}
	ChanOf struct {
		Val     *TypeRef
		DirRecv bool
		DirSend bool
	}
	Func      *TypeFunc
	Interface *TypeInterface
	Struct    *TypeStruct
	Named     struct {
		PkgName  string // empty if package-local (not imported) type
		TypeName string
	}
}

func (me *TypeRef) SafeBitSizeIfBuiltInNumberType() int {
	if me.Named.PkgName == "" {
		switch me.Named.TypeName {
		case "int8", "uint8", "byte":
			return 8
		case "int16", "uint16":
			return 16
		case "int64", "uint64", "float64", "complex64":
			return 64
		case "complex128":
			return 128
		case "int32", "uint32", "float32", "rune", "int", "uint":
			return 32
		}
	}
	return 0
}

// IsBuiltinPrimType returns whether `this` refers to one of Go's
// built-in primitive-types such as `bool`, `byte`, `uint`, `string` etc.
// (If `orIsUnderlyingBuiltinPrimType`, it walks the `Slice` / `Ptr` / `Map` as applicable.)
func (this *TypeRef) IsBuiltinPrimType(orIsUnderlyingBuiltinPrimType bool) bool {
	switch this.Named {
	case T.Bool.Named, T.Byte.Named, T.Complex128.Named, T.Complex64.Named, T.Float32.Named, T.Float64.Named, T.Int.Named, T.Int16.Named, T.Int32.Named, T.Int64.Named, T.Int8.Named, T.Rune.Named, T.String.Named, T.Uint.Named, T.Uint16.Named, T.Uint32.Named, T.Uint64.Named, T.Uint8.Named:
		return true
	}
	if orIsUnderlyingBuiltinPrimType {
		switch {
		case this.ArrOrSliceOf.Val != nil:
			return this.ArrOrSliceOf.Val.IsBuiltinPrimType(orIsUnderlyingBuiltinPrimType)
		case this.PtrTo != nil:
			return this.PtrTo.IsBuiltinPrimType(orIsUnderlyingBuiltinPrimType)
		case this.MapOf.Key != nil && this.MapOf.Val != nil:
			return this.MapOf.Key.IsBuiltinPrimType(orIsUnderlyingBuiltinPrimType) && this.MapOf.Val.IsBuiltinPrimType(orIsUnderlyingBuiltinPrimType)
		}
	}
	return false
}

// SynBlock represents a list of statements typically
// wrapped in curly-braces and separated by `;` (pre-`gofmt`).
type SynBlock struct {
	Body Syns
}

// Add is a convenience short-hand for `this.Body = append(this.Body,..)`.
func (this *SynBlock) Add(stmts ...ISyn) { this.Body = append(this.Body, stmts...) }

// SynFunc represents either a top-level (named) func /
// method declaration, or an anonymous func expression.
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

// SingleLineDocCommentParagraphs prepends doc-comments to a top-level `SynFunc` being `Emit`ted. Each represents
// a "single-line-paragraph" that in the generated output will be separated from the next via an empty `// ` line.
type SingleLineDocCommentParagraphs []string

// Add is a convenience short-hand for `append`.
func (this *SingleLineDocCommentParagraphs) Add(docCommentLines ...string) {
	*this = append(*this, docCommentLines...)
}

// StmtUnary is embedded by `StmtRet`, `StmtDefer`, `StmtGo`.
type StmtUnary struct {
	// the keyword's argument: must be non-`nil`
	// `*ExprCall` for `StmtGo` / `StmtDefer`,
	// can be anything incl. `nil` for `StmtRet`.
	Expr ISyn
}

// StmtBreak represents Go's `break` keyword.
type StmtBreak struct{}

// StmtContinue represents Go's `continue` keyword.
type StmtContinue struct{}

// StmtRet represents Go's `return` keyword.
type StmtRet struct {
	StmtUnary
}

// StmtDefer represents Go's `defer` keyword.
type StmtDefer struct {
	StmtUnary
}

// StmtGo represents Go's `go` keyword.
type StmtGo struct {
	StmtUnary
}

// StmtConst represents Go's `const` keyword.
type StmtConst struct {
	// Name is required, Type optional
	NamedTyped
	// required literal constant
	Expr ExprLit
}

// StmtVar represents Go's `var` keyword.
type StmtVar struct {
	// Name is required, Type optional
	NamedTyped
	// optional initialization-value expression
	Expr ISyn
}

// StmtIf represents Go's `if .. else` construct.
type StmtIf struct {
	// one or more `if` or `else if` conditions
	// with their associated branches
	IfThens SynConds
	// optional final `else` branch
	Else SynBlock
}

// SynConds is a slice of `SynCond`s.
type SynConds []SynCond

// Add is a convenience short-hand for `append`.
func (this *SynConds) Add(cond ISyn, thens ...ISyn) {
	*this = append(*this, Cond(cond, thens...))
}

// SynCond represents a condition expression together with a block
// of statements, used for both `StmtIf`s and `StmtSwitch`es.
type SynCond struct {
	// some condition expression
	Cond ISyn
	// associated-branch statements
	SynBlock
}

// StmtSwitch represents Go's `switch .. case` construct.
type StmtSwitch struct {
	// optional scrutinee
	Scrutinee ISyn
	// 0 or more `case` branches
	Cases SynConds
	// optional `default` branch
	Default SynBlock
}

// StmtFor represents either a `for .. range`
// loop or a classical `for` (not `range`) one.
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

// Op is embedded by all spsecific operators such as `OpAdd`, `OpEq`, etc.
type Op struct {
	// 1 or more operands: if 1 then
	// unary syntax output, else n-ary
	Operands Syns
}

// OpSet represents Go's `=` assignment operator.
type OpSet struct{ Op }

// OpDecl represents Go's `:=` declare-and-initialize operator.
type OpDecl struct{ Op }

// OpComma emits all its operands separated by `,` commas.
type OpComma struct{ Op }

// OpDot represents Go's `.` selector operator.
type OpDot struct{ Op }

// OpAnd represents Go's `&&` boolean-or operator.
type OpAnd struct{ Op }

// OpOr represents Go's `||` boolean-or operator.
type OpOr struct{ Op }

// OpEq represents Go's `==` equality comparison operator.
type OpEq struct{ Op }

// OpNeq represents Go's `!=` inequality comparison operator.
type OpNeq struct{ Op }

// OpGeq represents Go's `>=` greater-or-equal comparison operator.
type OpGeq struct{ Op }

// OpLeq represents Go's `<=` less-or-equal comparison operator.
type OpLeq struct{ Op }

// OpGt represents Go's `>` greater-than comparison operator.
type OpGt struct{ Op }

// OpLt represents Go's `<` less-than comparison operator.
type OpLt struct{ Op }

// OpAdd represents one or more `+` subtractions.
type OpAdd struct{ Op }

// OpSub represents one or more `-` subtractions (or negation if unary).
type OpSub struct{ Op }

// OpMul represents one or more `*` multiplications.
type OpMul struct{ Op }

// OpDiv represents one or more `/` divisions.
type OpDiv struct{ Op }

// OpIdx represents one or more `operand0[operand1][operand2]` indexers.
type OpIdx struct{ Op }

// OpNot represents Go's unary address-taking `&` operator.
type OpAddr struct{ Op }

// OpNot represents Go's unary pointer-dereferencing `*` operator.
type OpDeref struct{ Op }

// OpNot represents Go's unary `!` operator.
type OpNot struct{ Op }

// ExprLit represents any literal constant value,
// such as a string, rune, number or boolean.
type ExprLit struct {
	Val interface{}
}

// ExprCall represents a call to any callable `Callee`, or a
// type conversion (if `Callee` effectively names a type).
type ExprCall struct {
	Callee ISyn
	Args   Syns
}

// SynRaw is an `ISyn` that at codegen time simply emits its self-contained raw Go
// source-code (perhaps hardcoded or generated via templates or other means) directly.
type SynRaw []byte
