package udevgogen

// T returns a `NamedTyped` with `this.Name` and `typeRef`.
func (this Named) T(typeRef *TypeRef) (nt NamedTyped) {
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

// SafeBitSizeIfBuiltInNumberType returns 8 for `int8`, `byte`, `uint8`,
// or 16, 32, 64, 128 as applicable, recognizing only direct `Named` refs
// to Go' native `builtin` number types (no type-alias dereferencing yet).
func (this *TypeRef) SafeBitSizeIfBuiltInNumberType() int {
	if this.Named.PkgName == "" {
		switch this.Named.TypeName {
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
			return this.ArrOrSliceOf.Val.IsBuiltinPrimType(true)
		case this.ChanOf.Val != nil:
			return this.ChanOf.Val.IsBuiltinPrimType(true)
		case this.PtrTo != nil:
			return this.PtrTo.IsBuiltinPrimType(true)
		case this.MapOf.Key != nil && this.MapOf.Val != nil:
			return this.MapOf.Key.IsBuiltinPrimType(true) && this.MapOf.Val.IsBuiltinPrimType(true)
		}
	}
	return false
}

// Defer constructs a `StmtDefer` of `this` call.
func (this *ExprCall) Defer() StmtDefer { return Defer(this) }

// Go constructs a `StmtGo` on `this` call.
func (this *ExprCall) Go() StmtGo { return Go(this) }

// C constructs an `ExprCall` of `n` exposed by `this` imported-package.
func (this PkgName) C(n string, args ...ISyn) *ExprCall { return C.Dot(string(this), n, args...) }

// Method constructs a `SynFunc` with the given `name` and `args` plus `this` as its method `Recv`.
func (this NamedTyped) Method(name string, args ...NamedTyped) *SynFunc {
	return Fn(this, name, TdFunc(args))
}

// Method constructs a `SynFunc` with the given `name` and `args` plus a `this`-typed method `Recv` also named `"this"`.
func (this *TypeRef) Method(name string, args ...NamedTyped) *SynFunc {
	return V.This.T(this).Method(name, args...)
}

// Conv constructs an `ExprCall` that represents a conversion of `expr` into `this` type.
// (Go's conversion syntax, eg. `int(myexpr)`, is covered by `ExprCall` due to identical emitting logic.)
func (this *TypeRef) Conv(expr ISyn) *ExprCall { return Call(this, expr) }

// Args sets `this.Type.Func.Args` and returns `this`.
func (this *SynFunc) Args(args ...NamedTyped) *SynFunc {
	this.Type.Func.Args = args
	return this
}

// Arg adds to `this.Type.Func.Args` and returns `this`.
func (this *SynFunc) Arg(name string, typeRef *TypeRef) *SynFunc {
	this.Type.Func.Args.Add(name, typeRef)
	return this
}

// Code adds to `this.SynBlock.Body` and returns `this`.
func (this *SynFunc) Code(stmts ...ISyn) *SynFunc {
	this.Add(stmts...)
	return this
}

// Doc adds to `this.Docs` and returns `this`.
func (this *SynFunc) Doc(docCommentLines ...string) *SynFunc {
	this.Docs.Add(docCommentLines...)
	return this
}

// N sets `this.Named.Name` and returns `this`.
func (this *SynFunc) N(name string) *SynFunc {
	this.Named.Name = name
	return this
}

// Rets sets `this.Type.Func.Rets` and returns `this`.
func (this *SynFunc) Rets(rets ...NamedTyped) *SynFunc {
	this.Type.Func.Rets = rets
	return this
}

// Ret adds to `this.Type.Func.Rets` and returns `this`.
func (this *SynFunc) Ret(name string, typeRef *TypeRef) *SynFunc {
	this.Type.Func.Rets.Add(name, typeRef)
	return this
}

// Sig sets `this.Type.Func` to `sig` and returns `this`.
func (this *SynFunc) Sig(sig *TypeFunc) *SynFunc {
	this.Type.Func = sig
	return this
}

// Case adds the given `case` branch to the `StmtSwitch.Cases` of `this`.
func (this *StmtSwitch) Case(cond ISyn, thens ...ISyn) *StmtSwitch {
	this.Cases.Add(cond, thens...)
	return this
}

// Case adds the given `case` branches to the `StmtSwitch.Cases` of `this`.
func (this *StmtSwitch) CasesOf(conds ...SynCond) *StmtSwitch {
	this.Cases = append(this.Cases, conds...)
	return this
}

// DefaultCase sets the `default` branch of this `StmtSwitch`.
func (this *StmtSwitch) DefaultCase(stmts ...ISyn) *StmtSwitch {
	this.Default.Body = stmts
	return this
}
