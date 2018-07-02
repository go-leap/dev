// Package `github.com/go-leap/dev/go/gen` provides AST nodes for generating Go code.
// These are by design simpler and more lightweight than `go/ast`, given that the latter
// is designed to represent existing-and-parsed code (keeping track of much housekeeping),
// while this package only focuses on ad-hoc assemblage of newly-to-be-emitted Go sources.
//
// As a noteworthy goodie, all `func`s that have named return values automatically get a final
// `return` statement appended to their `Body` at code-gen time, if they don't already have one.
//
// A small handful of `udevgogen` exports have entirely upper-case names such as `GEN_IF`,
// `GEN_BYCASE` and so on. All these offer _codegen-time_ control flow and their usage is
// showcased throughout the numerous `github.com/metaleap/go-gent/gents/...` packages. They
// do incur a slight overhead (vs. using Go-native control-flows) for the neat readability
// sugar they offer. The upper-case names do stand out appropriately in real-world multi-line
// AST constructions and this eases differentiating between say branches or loops at code-gen
// time vs. to-be-emitted branches or loops belonging to the currently generated AST nodes.
//
// Likewise, there are numerous interfaces with `IExpr`-prefixed names such as `IExprBoolish`,
// `IExprNumerish`, etc (implemented by the various `ISyn` AST-node implementations provided),
// and those interfaces offer handy dot-accessor-style methods over standard constructor funcs
// --- by way of an illustrative example, think: `foo.Eq(bar).And(baz.Minus(2).Gt(0))` instead
// of `And(Eq(foo, bar), Gt(Sub(baz, L(2)), L(0)))`. All `IExpr‹Foo›ish` implementations are just
// such translations under the hood, implying some miniscule (or perhaps sub-nanoscule) cost there.
package udevgogen

import (
	"github.com/go-leap/str"
)

// SourceFile is a simple collection of `ISyn`s
// representing top-level definition declarations
// destined to be emitted into a single source file,
// plus the `package` name for that file.
// Imports are handled outside, eg. by utilizing `PkgImports`.
type SourceFile struct {
	// package name
	PkgName string

	// top-level definition declarations
	SynBlock
}

// PkgName offers some handy methods on package import names.
type PkgName string

// PkgImports maps (via its `Ensure` method) package
// import paths to package import names.
type PkgImports map[string]PkgName

// Ensure returns the `pkgImportName` for the given `pkgImportPath`
// as stored in `this` (or if missing, devises one in the form of eg.
// `pkg__encoding_json` for `encoding/json` and stores it, assuming
// that `PkgImportNamePrefix` is set to "pkg__", its default value).
func (this *PkgImports) Ensure(pkgImportPath string) (pkgImportName PkgName) {
	self := *this
	if self == nil {
		self = map[string]PkgName{}
	}
	if pkgImportName = self[pkgImportPath]; pkgImportName == "" {
		pkgImportName = PkgImportNamePrefix + PkgName(ustr.ReplB(pkgImportPath, '/', '_'))
		self[pkgImportPath] = pkgImportName
	}
	*this = self
	return
}

var (
	// see `PkgImports.Ensure(string) string` for details
	PkgImportNamePrefix PkgName = "pkg__"

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

	// singletons for Go's built-in reserved-identifiers
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

	// common Call constructors
	C struct {
		Append func(...ISyn) *ExprCall
		Len    func(...ISyn) *ExprCall
		Make   func(...ISyn) *ExprCall

		Dot   func(string, string, ...ISyn) *ExprCall
		Named func(string, ...ISyn) *ExprCall
	}

	// singletons for common var names
	Vars struct {
		// `"err"`
		Err NamedTyped
		// `"this"`, suits method-receivers (Go style fetishists hate it though)
		This Named
		// `"self"`, Pythonic flavour of `this` (just as pretty and as disliked)
		Self Named
		// `"me"`, VB6-style `this`/`self` alternative that won't trigger golint
		Me Named
		// `"ok"`, common for type-asserts / lookups / predicates
		Ok Named
		// `"r"`, common for a func's primary named `return` value
		R Named
		// `"s"`, common for a func's `string` arg
		S Named
		// `"i"`, common for iterations
		I Named
		// `"j"`, common for sub-iterations
		J Named
		// `"k"`, common for key-value pairs, eg. in for..range iterations
		K Named
		// `"v"`, for key-value pairs or func args (eg. `v interface{}`)
		V Named
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

		// some not-so-uncommon slices
		Sl struct {
			Ints    *TypeRef
			Strings *TypeRef
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

func init() {
	Vars.Err.Name, Vars.Err.Type, Vars.R.Name, Vars.This.Name, Vars.Self.Name, Vars.Me.Name, Vars.I.Name, Vars.J.Name, Vars.K.Name, Vars.V.Name, Vars.Ok.Name, Vars.S.Name = "err", TrNamed("", "error"), "r", "this", "self", "me", "i", "j", "k", "v", "ok", "s"

	B.Append.Name, B.Cap.Name, B.Close.Name, B.Complex.Name, B.Copy.Name, B.Delete.Name, B.Imag.Name, B.Len.Name, B.Make.Name, B.New.Name, B.Panic.Name, B.Print.Name, B.Println.Name, B.Real.Name, B.Recover.Name = "append", "cap", "close", "complex", "copy", "delete", "imag", "len", "make", "new", "panic", "print", "println", "real", "recover"
	B.True, B.False = L(true), L(false)

	c := func(n Named) func(...ISyn) *ExprCall {
		return func(args ...ISyn) *ExprCall { return Call(n, args...) }
	}
	C.Append, C.Len, C.Make = c(B.Append), c(B.Len), c(B.Make)
	C.Named = func(name string, args ...ISyn) *ExprCall {
		return Call(N(name), args...)
	}
	C.Dot = func(dotLeft string, dotRight string, args ...ISyn) *ExprCall {
		return Call(D(N(dotLeft), N(dotRight)), args...)
	}
	T.Bool, T.Byte, T.Complex128, T.Complex64, T.Float32, T.Float64, T.Int, T.Int16, T.Int32, T.Int64, T.Int8, T.Rune, T.String, T.Uint, T.Uint16, T.Uint32, T.Uint64, T.Uint8 = TrNamed("", "bool"), TrNamed("", "byte"), TrNamed("", "complex128"), TrNamed("", "complex64"), TrNamed("", "float32"), TrNamed("", "float64"), TrNamed("", "int"), TrNamed("", "int16"), TrNamed("", "int32"), TrNamed("", "int64"), TrNamed("", "int8"), TrNamed("", "rune"), TrNamed("", "string"), TrNamed("", "uint"), TrNamed("", "uint16"), TrNamed("", "uint32"), TrNamed("", "uint64"), TrNamed("", "uint8")
	T.Interface = TrInterface(TdInterface(nil))
	T.Sl.Ints, T.Sl.Strings = TrSlice(T.Int), TrSlice(T.String)

	Sigs.NoneToBool.Rets, Sigs.NoneToString.Rets = NamedsTypeds{Vars.R.T(T.Bool)}, NamedsTypeds{Vars.R.T(T.String)}
}
