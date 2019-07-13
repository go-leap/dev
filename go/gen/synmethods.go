package udevgogen

import (
	"github.com/go-leap/str"
)

func (me Syns) Transform(transform func(ISyn) ISyn) Syns {
	for i := range me {
		me[i] = transform(me[i])
	}
	return me
}

// OfType returns a `NamedTyped` with `me.Name` and `typeRef`.
func (me Named) OfType(typeRef *TypeRef) (nt NamedTyped) {
	nt.Named, nt.Type = me, typeRef
	return
}

func (me NamedsTypeds) Names(strLits bool) []interface{} {
	slice := make([]interface{}, len(me))
	for i := range me {
		if strLits {
			slice[i] = me[i].Name
		} else {
			slice[i] = me[i].Named
		}
	}
	return slice
}

func (me NamedsTypeds) ToAnys(transform func(*NamedTyped) IAny) (transformed []IAny) {
	transformed = make([]IAny, len(me))
	for i := range transformed {
		if transform != nil {
			transformed[i] = transform(&me[i])
		} else {
			transformed[i] = me[i]
		}
	}
	return
}

func (me NamedsTypeds) ToSyns(transform func(*NamedTyped) ISyn) (transformed Syns) {
	transformed = make(Syns, len(me))
	for i := range transformed {
		if transform != nil {
			transformed[i] = transform(&me[i])
		} else {
			transformed[i] = me[i]
		}
	}
	return
}

func (me NamedsTypeds) LastEllipsisIfSlice() (r NamedsTypeds) {
	if r = me; len(r) > 0 {
		if last := &r[len(r)-1]; last.Type != nil && last.Type.ArrOrSlice.Of != nil && last.Type.ArrOrSlice.IsFixedLen == nil && !last.Type.ArrOrSlice.IsEllipsis {
			r = make(NamedsTypeds, len(me))
			copy(r, me)
			typecopy := *r[len(r)-1].Type
			typecopy.ArrOrSlice.IsEllipsis = true
			r[len(r)-1].Type = &typecopy
		}
	}
	return
}

func (me NamedsTypeds) Renamed(rename func(string) string) (renamed NamedsTypeds) {
	renamed = make(NamedsTypeds, len(me))
	copy(renamed, me)
	for i := range renamed {
		renamed[i].Name = rename(renamed[i].Name)
	}
	return
}

// AllNamed returns whether all `NamedTyped`s in `me` have a `Name` set.
// If `me` is empty, `false` is returned.
func (me NamedsTypeds) AllNamed() bool {
	for i := range me {
		if me[i].Name == "" {
			return false
		}
	}
	return len(me) > 0
}

// AllTyped returns whether all `NamedTyped`s in `me` have a `Type` set.
// If `me` is empty, `false` is returned.
func (me NamedsTypeds) AllTyped() bool {
	for i := range me {
		if me[i].Type == nil {
			return false
		}
	}
	return len(me) > 0
}

// BitSizeIfBuiltInNumberType returns 8 for `int8`, `byte`, `uint8`,
// or 16, 32, 64, 128 as applicable, recognizing only direct `Named` refs
// to Go' native `builtin` number types (no type-alias dereferencing yet).
func (me *TypeRef) BitSizeIfBuiltInNumberType() int {
	if me.Named.PkgName == "" {
		switch me.Named.TypeName {
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

func (me *TypeRef) IsZeroish(exprOfThisType ISyn, forceCanLen bool, forceCanNum bool) ISyn {
	return Not(me.IsntZeroish(exprOfThisType, forceCanLen, forceCanNum))
}

func (me *TypeRef) IsntZeroish(exprOfThisType ISyn, forceCanLen bool, forceCanNum bool) (expr ISyn) {
	switch {
	case me.Named == T.Bool.Named:
		expr = exprOfThisType
	case me.Named == T.Error.Named || me.Func != nil || me.Pointer.Of != nil || me.Interface != nil || me.Chan.Of != nil:
		expr = Neq(exprOfThisType, B.Nil)
	case forceCanLen || me.Named == T.String.Named || me.ArrOrSlice.Of != nil || (me.Map.OfKey != nil && me.Map.ToVal != nil):
		expr = Neq(Call(B.Len, exprOfThisType), L(0))
	case forceCanNum || me.Named.PkgName == "" && me.Named.TypeName != "" && (me.Named.TypeName == T.Byte.Named.TypeName || me.Named.TypeName == T.Complex128.Named.TypeName || me.Named.TypeName == T.Complex64.Named.TypeName || me.Named.TypeName == T.Float32.Named.TypeName || me.Named.TypeName == T.Float64.Named.TypeName || me.Named.TypeName == T.Int.Named.TypeName || me.Named.TypeName == T.Int16.Named.TypeName || me.Named.TypeName == T.Int32.Named.TypeName || me.Named.TypeName == T.Int64.Named.TypeName || me.Named.TypeName == T.Int8.Named.TypeName || me.Named.TypeName == T.Rune.Named.TypeName || me.Named.TypeName == T.Uint.Named.TypeName || me.Named.TypeName == T.Uint16.Named.TypeName || me.Named.TypeName == T.Uint32.Named.TypeName || me.Named.TypeName == T.Uint64.Named.TypeName || me.Named.TypeName == T.Uint8.Named.TypeName):
		expr = Neq(exprOfThisType, L(0))
	}
	return
}

func (me *TypeRef) Implements() {
}

// IsBuiltinPrimType returns whether `me` refers to one of Go's built-in primitive-types such as `bool`, `string` etc.
// (If `orIsUnderlyingBuiltinPrimType`, it walks the `ArrOrSlice` / `Pointer` / `Map` / `Chan` as applicable.)
func (me *TypeRef) IsBuiltinPrimType(orIsUnderlyingBuiltinPrimType bool) bool {
	switch me.Named {
	case T.Bool.Named, T.Byte.Named, T.Complex128.Named, T.Complex64.Named, T.Float32.Named, T.Float64.Named, T.Int.Named, T.Int16.Named, T.Int32.Named, T.Int64.Named, T.Int8.Named, T.Rune.Named, T.String.Named, T.Uint.Named, T.Uint16.Named, T.Uint32.Named, T.Uint64.Named, T.Uint8.Named:
		return true
	}
	if orIsUnderlyingBuiltinPrimType {
		switch {
		case me.ArrOrSlice.Of != nil:
			return me.ArrOrSlice.Of.IsBuiltinPrimType(true)
		case me.Chan.Of != nil:
			return me.Chan.Of.IsBuiltinPrimType(true)
		case me.Pointer.Of != nil:
			return me.Pointer.Of.IsBuiltinPrimType(true)
		case me.Map.OfKey != nil && me.Map.ToVal != nil:
			return me.Map.OfKey.IsBuiltinPrimType(true) && me.Map.ToVal.IsBuiltinPrimType(true)
		}
	}
	return false
}

func (me *TypeRef) IsNamedAndPublic() bool {
	return me.Named.PkgName != "" || (me.Named.TypeName != "" && ustr.BeginsUpper(me.Named.TypeName))
}

func (me *TypeRef) UltimateElemType() (tEl *TypeRef) {
	switch {
	case me.ArrOrSlice.Of != nil:
		tEl = me.ArrOrSlice.Of
	case me.Pointer.Of != nil:
		tEl = me.Pointer.Of
	case me.Chan.Of != nil:
		tEl = me.Pointer.Of
	case me.Map.ToVal != nil:
		tEl = me.Map.ToVal
	}
	if tEl == nil {
		tEl = me
	} else {
		tEl = tEl.UltimateElemType()
	}
	return
}

func (me *TypeRef) EffectiveFieldNameWhenEmbedded() string {
	switch {
	case me.Named.TypeName != "":
		return me.Named.TypeName
	case me.Pointer.Of != nil:
		return me.Pointer.Of.EffectiveFieldNameWhenEmbedded()
	}
	return "?"
}

func (me *TypeRef) String() string {
	switch {
	case me.Named.TypeName != "":
		if me.Named.PkgName != "" {
			return me.Named.PkgName + "." + me.Named.TypeName
		}
		return me.Named.TypeName
	case me.ArrOrSlice.Of != nil && me.ArrOrSlice.IsEllipsis:
		return "..." + me.ArrOrSlice.Of.String()
	case me.ArrOrSlice.Of != nil && me.ArrOrSlice.IsFixedLen != nil:
		return "[_]" + me.ArrOrSlice.Of.String()
	case me.ArrOrSlice.Of != nil && me.ArrOrSlice.IsFixedLen == nil:
		return "[]" + me.ArrOrSlice.Of.String()
	case me.Chan.Of != nil:
		return "chan " + me.Chan.Of.String()
	case me.Interface != nil:
		return "interface{..}"
	case me.Map.OfKey != nil && me.Map.ToVal != nil:
		return "map[" + me.Map.OfKey.String() + "]" + me.Map.ToVal.String()
	case me.Pointer.Of != nil:
		return "*" + me.Pointer.Of.String()
	case me.Struct != nil:
		return "struct{..}"
	}
	panic(me)
}

// Defer constructs a `StmtDefer` of `me` call.
func (me *ExprCall) Defer() StmtDefer {
	return StmtDefer{StmtUnary: StmtUnary{Expr: me}}
}

// Go constructs a `StmtGo` on `me` call.
func (me *ExprCall) Go() StmtGo {
	return StmtGo{StmtUnary: StmtUnary{Expr: me}}
}

func (me *ExprCall) Spreads() *ExprCall {
	me.LastArgSpreads = true
	return me
}

// C constructs an `ExprCall` of the `funcName` exported by `me` imported-package.
func (me PkgName) C(funcName string, args ...IAny) *ExprCall {
	return &ExprCall{
		Callee: OpDot{Op: Op{Operands: Syns{Named{Name: string(me)}, Named{Name: funcName}}}},
		Args:   SynsFrom(nil, args...),
	}
}

func (me PkgName) N(exportedName string, maybeFurtherDotOperands ...IAny) IExprDottish {
	if me == "" {
		if len(maybeFurtherDotOperands) > 0 {
			return N(exportedName).D(maybeFurtherDotOperands...)
		}
		return N(exportedName)
	}
	if len(maybeFurtherDotOperands) > 0 {
		return N(string(me)).D(append([]IAny{N(exportedName)}, maybeFurtherDotOperands...)...)
	}
	return N(string(me)).D(exportedName)
}

// T constructs a `TypeRef` with `Named` referring to `PkgName` and `typeName`.
func (me PkgName) T(typeName string) *TypeRef {
	return TFrom(me, typeName)
}

// Tª constructs a `TypeRef` with its `Pointer`'s `Named` referring to `PkgName` and `typeName`.
func (me PkgName) Tª(typeName string) *TypeRef {
	return TPointer(me.T(typeName))
}

// Method constructs a `SynFunc` with the given `name` and `args` plus `me` as its method `Recv`.
func (me NamedTyped) Method(name string, args ...NamedTyped) *SynFunc {
	fn := &SynFunc{Recv: me}
	fn.Name, fn.Type = name, &TypeRef{Func: &TypeFunc{Args: args}}
	return fn
}

// Method constructs a `SynFunc` with the given `name` and `args` plus a `me`-typed method `Recv` named after `Self`.
func (me *TypeRef) Method(name string, args ...NamedTyped) *SynFunc {
	fn := &SynFunc{Recv: NamedTyped{Named: Self, Type: me}}
	fn.Name, fn.Type = name, &TypeRef{Func: &TypeFunc{Args: args}}
	return fn
}

var Self = Named{"this"}

// From constructs an `ExprCall` that represents a conversion of `expr` into `me` type.
// (Returns `ExprCall` because Go's conversion syntax, eg. `int(myexpr)`, is covered by it due to the same emitting logic.)
func (me *TypeRef) From(expr IAny) *ExprCall {
	return &ExprCall{Callee: me, Args: Syns{synFrom(expr)}}
}

// N constructs a `NamedTyped` based on `name` and `me` type.
func (me *TypeRef) N(name string) NamedTyped {
	return NamedTyped{Named: Named{Name: name}, Type: me}
}

// T constructs a `TypeRef` whose `Func` points to `me`.
func (me *TypeFunc) T() *TypeRef {
	return &TypeRef{Func: me}
}

// Arg adds to `me.Args` and returns `me`.
func (me *TypeFunc) Arg(name string, typeRef *TypeRef) *TypeFunc {
	me.Args = append(me.Args, NamedTyped{Named: Named{Name: name}, Type: typeRef})
	return me
}

// Ret adds to `me.Rets` and returns `me`.
func (me *TypeFunc) Ret(name string, typeRef *TypeRef) *TypeFunc {
	me.Rets = append(me.Rets, NamedTyped{Named: Named{Name: name}, Type: typeRef})
	return me
}

func (me *TypeFunc) Spreads() *TypeFunc {
	me.LastArgSpreads = true
	return me
}

// Args sets `me.Type.Func.Args` and returns `me`.
func (me *SynFunc) Args(args ...NamedTyped) *SynFunc {
	me.Type.Func.Args = args
	return me
}

func (me *SynFunc) ArgIf(onlyIf bool, arg NamedTyped) *SynFunc {
	if onlyIf {
		me.Type.Func.Args = append(me.Type.Func.Args, arg)
	}
	return me
}

// Arg adds to `me.Type.Func.Args` and returns `me`.
func (me *SynFunc) Arg(name string, typeRef *TypeRef) *SynFunc {
	me.Type.Func.Args = append(me.Type.Func.Args, NamedTyped{Named: Named{Name: name}, Type: typeRef})
	return me
}

func (me *SynFunc) Spreads() *SynFunc {
	me.Type.Func.LastArgSpreads = true
	return me
}

// Code adds to `me.SynBlock.Body` and returns `me`.
func (me *SynFunc) Code(stmts ...ISyn) *SynFunc {
	me.Body = append(me.Body, stmts...)
	return me
}

func (me *SynFunc) EmitsCommented(emitCommented bool) *SynFunc {
	me.EmitCommented = emitCommented
	return me
}

// Doc adds to `me.Docs` and returns `me`.
func (me *SynFunc) Doc(docCommentLines ...string) *SynFunc {
	me.Docs = append(me.Docs, docCommentLines...)
	return me
}

// Doc adds to `me.Docs` and returns `me`.
func (me *TypeDecl) Doc(docCommentLines ...string) *TypeDecl {
	me.Docs = append(me.Docs, docCommentLines...)
	return me
}

func (me *TypeDecl) DocIf(ok bool, docCommentLines ...string) *TypeDecl {
	if !ok {
		return me
	}
	return me.Doc(docCommentLines...)
}

// Rets sets `me.Type.Func.Rets` and returns `me`.
func (me *SynFunc) Rets(rets ...NamedTyped) *SynFunc {
	me.Type.Func.Rets = rets
	return me
}

// Ret adds to `me.Type.Func.Rets` and returns `me`.
func (me *SynFunc) Ret(name string, typeRef *TypeRef) *SynFunc {
	me.Type.Func.Rets = append(me.Type.Func.Rets, NamedTyped{Named: Named{Name: name}, Type: typeRef})
	return me
}

// Sig sets `me.Type.Func` to `sig` and returns `me`.
func (me *SynFunc) Sig(sig *TypeFunc) *SynFunc {
	me.Type.Func = sig
	return me
}

// Code sets `me.Body` and returns `me`.
func (me *StmtFor) Code(stmts ...ISyn) *StmtFor {
	me.Body = stmts
	return me
}

// Case adds the given `case` branch to the `StmtSwitch.Cases` of `me`.
func (me *StmtSwitch) Case(cond ISyn, thens ...ISyn) *StmtSwitch {
	me.Cases = append(me.Cases, SynCase{Cond: cond, SynBlock: SynBlock{Body: thens}})
	return me
}

// CasesFrom adds to `me.Cases` and returns `me`.
// If `areAllSynCases`, each `ISyn` is expected to be a `*SynCase` and added.
// Otherwise, `synCasesOrCondsAndThens` are alternating pairs of `Cond`s-and-thens
// that are used to construct the individual `SynCase`s to add.
func (me *StmtSwitch) CasesFrom(areAllSynCases bool, synCasesOrCondsAndThens ...ISyn) *StmtSwitch {
	if div := 2; len(me.Cases) == 0 {
		if areAllSynCases {
			div = 1
		}
		me.Cases = make(SynCases, 0, len(synCasesOrCondsAndThens)/div)
	}

	if areAllSynCases {
		for i := range synCasesOrCondsAndThens {
			me.Cases = append(me.Cases, *synCasesOrCondsAndThens[i].(*SynCase))
		}
	} else {
		for i := 1; i < len(synCasesOrCondsAndThens); i += 2 {
			me.Cases = append(me.Cases, SynCase{Cond: synCasesOrCondsAndThens[i-1], SynBlock: SynBlock{Body: SynsFrom(synCasesOrCondsAndThens[i])}})
		}
	}
	return me
}

// CasesOf adds the given `case` branches to the `StmtSwitch.Cases` of `me`.
func (me *StmtSwitch) CasesOf(cases ...SynCase) *StmtSwitch {
	me.Cases = append(me.Cases, cases...)
	return me
}

// DefaultCase sets the `default` branch of this `StmtSwitch`.
func (me *StmtSwitch) DefaultCase(stmts ...ISyn) *StmtSwitch {
	me.Default.Body = stmts
	return me
}

// Field returns the `SynStructField` in `me.Fields` matching `name`.
func (me *TypeStruct) Field(name string, tryJsonNamesToo bool) (fld *SynStructField) {
	for i := range me.Fields {
		if me.Fields[i].Name == name {
			return &me.Fields[i]
		}
	}
	if tryJsonNamesToo {
		for i := range me.Fields {
			if me.Fields[i].JsonName() == name {
				return &me.Fields[i]
			}
		}
	}
	return nil
}

func (me *SynStructField) EffectiveName() string {
	if me.Name == "" {
		return me.Type.EffectiveFieldNameWhenEmbedded()
	}
	return me.Name
}

func (me *SynStructField) EffectiveNameBeginsUpper() bool {
	return ustr.BeginsUpper(me.EffectiveName())
}

// JsonName returns `me.Tags["json"][:comma]` or `me.Name`.
func (me *SynStructField) JsonName() (name string) {
	if name = me.Tags["json"]; name != "" {
		if i := ustr.IdxR(name, ','); i >= 0 {
			name = name[:i]
		}
	}
	if name == "" {
		name = me.Name
	}
	return
}

func (me *SynStructField) JsonNameFinal() (name string) {
	if t := me.Type.UltimateElemType(); t.Chan.Of == nil && t.Func == nil &&
		me.Type.Chan.Of == nil && me.Type.Func == nil {
		if name = me.JsonName(); name == "-" || (name == me.Name && !ustr.BeginsUpper(name)) {
			name = ""
		}
	}
	return
}

func (me *SynStructField) JsonOmitEmpty() bool {
	if tag := me.Tags["json"]; tag != "" {
		return ustr.Suff(tag, ",omitempty") || ustr.Has(tag, ",omitempty,")
	}
	return false
}

func (me SynStructFields) Exists(ok func(*SynStructField) bool) bool {
	for i := range me {
		if ok(&me[i]) {
			return true
		}
	}
	return false
}

func (me SynStructFields) IndicesWhere(ok func(*SynStructField) bool) (indices []int) {
	indices = make([]int, 0, len(me))
	for i := range me {
		if ok(&me[i]) {
			indices = append(indices, i)
		}
	}
	return
}

func (me SynStructFields) NamedOnly() (named SynStructFields) {
	named = me
	var needcopy bool
	for i := range named {
		if needcopy = (named[i].Name == ""); needcopy {
			break
		}
	}
	if needcopy {
		named = make(SynStructFields, len(me))
		copy(named, me)
		for i := 0; i < len(named); i++ {
			if named[i].Name == "" {
				i, named = i-1, append(named[:i], named[i+1:]...)
			}
		}
	}
	return
}

func (me SynStructFields) NTs() (nts NamedsTypeds) {
	nts = make(NamedsTypeds, len(me))
	for i := range me {
		nts[i] = me[i].NamedTyped
	}
	return
}
