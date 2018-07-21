package udevgogen

// OfType returns a `NamedTyped` with `this.Name` and `typeRef`.
func (this Named) OfType(typeRef *TypeRef) (nt NamedTyped) {
	nt.Named, nt.Type = this, typeRef
	return
}

// AllNamed returns whether all `NamedTyped`s in `this` have a `Name` set.
// If `this` is empty, `false` is returned.
func (this NamedsTypeds) AllNamed() bool {
	for i := range this {
		if this[i].Name == "" {
			return false
		}
	}
	return len(this) > 0
}

// AllTyped returns whether all `NamedTyped`s in `this` have a `Type` set.
// If `this` is empty, `false` is returned.
func (this NamedsTypeds) AllTyped() bool {
	for i := range this {
		if this[i].Type == nil {
			return false
		}
	}
	return len(this) > 0
}

// BitSizeIfBuiltInNumberType returns 8 for `int8`, `byte`, `uint8`,
// or 16, 32, 64, 128 as applicable, recognizing only direct `Named` refs
// to Go' native `builtin` number types (no type-alias dereferencing yet).
func (this *TypeRef) BitSizeIfBuiltInNumberType() int {
	if this.Named.PkgName == "" {
		switch this.Named.TypeName {
		case "int8", "uint8", "byte":
			return 8
		case "int16", "uint16":
			return 16
		case "int32", "uint32", "float32", "rune", "int", "uint":
			return 32
		case "int64", "uint64", "float64", "complex64":
			return 64
		case "complex128":
			return 128
		}
	}
	return 0
}

// IsBuiltinPrimType returns whether `this` refers to one of Go's built-in primitive-types such as `bool`, `string` etc.
// (If `orIsUnderlyingBuiltinPrimType`, it walks the `ArrOrSlice` / `Pointer` / `Map` / `Chan` as applicable.)
func (this *TypeRef) IsBuiltinPrimType(orIsUnderlyingBuiltinPrimType bool) bool {
	switch this.Named {
	case T.Bool.Named, T.Byte.Named, T.Complex128.Named, T.Complex64.Named, T.Float32.Named, T.Float64.Named, T.Int.Named, T.Int16.Named, T.Int32.Named, T.Int64.Named, T.Int8.Named, T.Rune.Named, T.String.Named, T.Uint.Named, T.Uint16.Named, T.Uint32.Named, T.Uint64.Named, T.Uint8.Named:
		return true
	}
	if orIsUnderlyingBuiltinPrimType {
		switch {
		case this.ArrOrSlice.Of != nil:
			return this.ArrOrSlice.Of.IsBuiltinPrimType(true)
		case this.Chan.Of != nil:
			return this.Chan.Of.IsBuiltinPrimType(true)
		case this.Pointer.Of != nil:
			return this.Pointer.Of.IsBuiltinPrimType(true)
		case this.Map.OfKey != nil && this.Map.ToVal != nil:
			return this.Map.OfKey.IsBuiltinPrimType(true) && this.Map.ToVal.IsBuiltinPrimType(true)
		}
	}
	return false
}

// Defer constructs a `StmtDefer` of `this` call.
func (this *ExprCall) Defer() StmtDefer {
	return StmtDefer{StmtUnary: StmtUnary{Expr: this}}
}

// Go constructs a `StmtGo` on `this` call.
func (this *ExprCall) Go() StmtGo {
	return StmtGo{StmtUnary: StmtUnary{Expr: this}}
}

func (this *ExprCall) Spreads() *ExprCall {
	this.LastArgSpreads = true
	return this
}

// Call constructs an `ExprCall` of the `funcName` exported by `this` imported-package.
func (this PkgName) Call(funcName string, args ...IAny) *ExprCall {
	return &ExprCall{
		Callee: OpDot{Op: Op{Operands: Syns{Named{Name: string(this)}, Named{Name: funcName}}}},
		Args:   synsFrom(nil, args...),
	}
}

// T constructs a `TypeRef` with `Named` referring to `this PkgName` and `typeName`.
func (this PkgName) T(typeName string) *TypeRef {
	return TrNamed(string(this), typeName)
}

// Tª constructs a `TypeRef` with its `Pointer`'s `Named` referring to `this PkgName` and `typeName`.
func (this PkgName) Tª(typeName string) *TypeRef {
	return TrPtr(this.T(typeName))
}

// Method constructs a `SynFunc` with the given `name` and `args` plus `this` as its method `Recv`.
func (this NamedTyped) Method(name string, args ...NamedTyped) *SynFunc {
	fn := &SynFunc{Recv: this}
	fn.Name, fn.Type = name, &TypeRef{Func: &TypeFunc{Args: args}}
	return fn
}

// Method constructs a `SynFunc` with the given `name` and `args` plus a `this`-typed method `Recv` also named `"this"`.
func (this *TypeRef) Method(name string, args ...NamedTyped) *SynFunc {
	fn := &SynFunc{Recv: NamedTyped{Named: Vars.This, Type: this}}
	fn.Name, fn.Type = name, &TypeRef{Func: &TypeFunc{Args: args}}
	return fn
}

// Conv constructs an `ExprCall` that represents a conversion of `expr` into `this` type.
// (Go's conversion syntax, eg. `int(myexpr)`, is covered by `ExprCall` due to identical emitting logic.)
func (this *TypeRef) Conv(expr ISyn) *ExprCall { return &ExprCall{Callee: this, Args: Syns{expr}} }

// N constructs a `NamedTyped` based on `name` and `this` type.
func (this *TypeRef) N(name string) NamedTyped {
	return NamedTyped{Named: Named{Name: name}, Type: this}
}

// Ref constructs a `TypeRef` whose `Func` points to `this`.
func (this *TypeFunc) Ref() *TypeRef {
	return &TypeRef{Func: this}
}

// Arg adds to `this.Args` and returns `this`.
func (this *TypeFunc) Arg(name string, typeRef *TypeRef) *TypeFunc {
	this.Args = append(this.Args, NamedTyped{Named: Named{Name: name}, Type: typeRef})
	return this
}

// Ret adds to `this.Rets` and returns `this`.
func (this *TypeFunc) Ret(name string, typeRef *TypeRef) *TypeFunc {
	this.Rets = append(this.Rets, NamedTyped{Named: Named{Name: name}, Type: typeRef})
	return this
}

func (this *TypeFunc) Spreads() *TypeFunc {
	this.LastArgSpreads = true
	return this
}

// Args sets `this.Type.Func.Args` and returns `this`.
func (this *SynFunc) Args(args ...NamedTyped) *SynFunc {
	this.Type.Func.Args = args
	return this
}

// Arg adds to `this.Type.Func.Args` and returns `this`.
func (this *SynFunc) Arg(name string, typeRef *TypeRef) *SynFunc {
	this.Type.Func.Args = append(this.Type.Func.Args, NamedTyped{Named: Named{Name: name}, Type: typeRef})
	return this
}

func (this *SynFunc) Spreads() *SynFunc {
	this.Type.Func.LastArgSpreads = true
	return this
}

// Code adds to `this.SynBlock.Body` and returns `this`.
func (this *SynFunc) Code(stmts ...ISyn) *SynFunc {
	this.Body = append(this.Body, stmts...)
	return this
}

// Doc adds to `this.Docs` and returns `this`.
func (this *SynFunc) Doc(docCommentLines ...string) *SynFunc {
	this.Docs = append(this.Docs, docCommentLines...)
	return this
}

// Rets sets `this.Type.Func.Rets` and returns `this`.
func (this *SynFunc) Rets(rets ...NamedTyped) *SynFunc {
	this.Type.Func.Rets = rets
	return this
}

// Ret adds to `this.Type.Func.Rets` and returns `this`.
func (this *SynFunc) Ret(name string, typeRef *TypeRef) *SynFunc {
	this.Type.Func.Rets = append(this.Type.Func.Rets, NamedTyped{Named: Named{Name: name}, Type: typeRef})
	return this
}

// Sig sets `this.Type.Func` to `sig` and returns `this`.
func (this *SynFunc) Sig(sig *TypeFunc) *SynFunc {
	this.Type.Func = sig
	return this
}

// Code sets `this.Body` and returns `this`.
func (this *StmtFor) Code(stmts ...ISyn) *StmtFor {
	this.Body = stmts
	return this
}

// Case adds the given `case` branch to the `StmtSwitch.Cases` of `this`.
func (this *StmtSwitch) Case(cond ISyn, thens ...ISyn) *StmtSwitch {
	this.Cases = append(this.Cases, SynCase{Cond: cond, SynBlock: SynBlock{Body: thens}})
	return this
}

// CasesFrom adds to `this.Cases` and returns `this`.
// If `areAllSynCases`, each `ISyn` is expected to be a `*SynCase` and added.
// Otherwise, `synCasesOrCondsAndThens` are alternating pairs of `Cond`s-and-thens
// that are used to construct the individual `SynCase`s to add.
func (this *StmtSwitch) CasesFrom(areAllSynCases bool, synCasesOrCondsAndThens ...ISyn) *StmtSwitch {
	if div := 2; len(this.Cases) == 0 {
		if areAllSynCases {
			div = 1
		}
		this.Cases = make(SynCases, 0, len(synCasesOrCondsAndThens)/div)
	}

	if areAllSynCases {
		for i := range synCasesOrCondsAndThens {
			this.Cases = append(this.Cases, *synCasesOrCondsAndThens[i].(*SynCase))
		}
	} else {
		for i := 1; i < len(synCasesOrCondsAndThens); i += 2 {
			this.Cases = append(this.Cases, SynCase{Cond: synCasesOrCondsAndThens[i-1], SynBlock: SynBlock{Body: synsFrom(synCasesOrCondsAndThens[i])}})
		}
	}
	return this
}

// CasesOf adds the given `case` branches to the `StmtSwitch.Cases` of `this`.
func (this *StmtSwitch) CasesOf(cases ...SynCase) *StmtSwitch {
	this.Cases = append(this.Cases, cases...)
	return this
}

// DefaultCase sets the `default` branch of this `StmtSwitch`.
func (this *StmtSwitch) DefaultCase(stmts ...ISyn) *StmtSwitch {
	this.Default.Body = stmts
	return this
}

// Field returns the `SynStructField` in `this.Fields` matching `name`.
func (this *TypeStruct) Field(name string) (fld *SynStructField) {
	for i := range this.Fields {
		if this.Fields[i].Name == name {
			return &this.Fields[i]
		}
	}
	return nil
}
