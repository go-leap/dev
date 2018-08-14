// Package `github.com/go-leap/dev/go/gen` provides AST nodes for generating Go code.
// These are by design simpler and more lightweight than `go/ast`, given that the latter
// is designed to represent existing-and-parsed code (keeping track of much housekeeping),
// while this package only focuses on ad-hoc assemblage of newly-to-be-emitted Go sources.
//
// As a noteworthy goodie, all `func`s that have named return values automatically get a final
// `return` statement appended to their `Body` at code-gen time, if they don't already have one.
//
// A small handful of `udevgogen` exports have entirely upper-case names (such as `GEN_IF`, `GEN_BYCASE`,
// `GEN_FOR`, `UNLESS`, etc.) All these offer _codegen-time_ control flow and their usage is showcased
// throughout the numerous `github.com/metaleap/go-gent/gents/...` packages. They do incur a slight overhead (vs.
// using Go-native control-flows) for the neat readability sugar they offer. The upper-case names do stand out
// in real-world multi-line AST constructions, letting the reader differentiate between say branches / loops
// to-be-evaluated at code-gen time vs. to-be-emitted branches/loops belonging to the currently generated AST nodes.
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

// IAny is opinionated brevity-readability-delight.
type IAny = interface{}

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
	if pkgImportPath != "" {
		self := *this
		if self == nil {
			self = map[string]PkgName{}
		}
		if pkgImportName = self[pkgImportPath]; pkgImportName == "" {
			pkgimpname := ustr.TrimPref(pkgImportPath, PkgImportNameTrimPathsPrefixPrior)
			pkgImportName = PkgImportNamePrefix + PkgName(ustr.ReplB(pkgimpname, '/', '_', '.', '_', '-', '_'))
			self[pkgImportPath] = pkgImportName
		}
		*this = self
	}
	return
}

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

func init() {
	B.Append.Name, B.Cap.Name, B.Close.Name, B.Complex.Name, B.Copy.Name, B.Delete.Name, B.Imag.Name, B.Len.Name, B.Make.Name, B.New.Name, B.Panic.Name, B.Print.Name, B.Println.Name, B.Real.Name, B.Recover.Name = "append", "cap", "close", "complex", "copy", "delete", "imag", "len", "make", "new", "panic", "print", "println", "real", "recover"
	B.True, B.False = L(true), L(false)

	T.Error, T.Bool, T.Byte, T.Complex128, T.Complex64, T.Float32, T.Float64, T.Int, T.Int16, T.Int32, T.Int64, T.Int8, T.Rune, T.String, T.Uint, T.Uint16, T.Uint32, T.Uint64, T.Uint8 = TFrom("", "error"), TFrom("", "bool"), TFrom("", "byte"), TFrom("", "complex128"), TFrom("", "complex64"), TFrom("", "float32"), TFrom("", "float64"), TFrom("", "int"), TFrom("", "int16"), TFrom("", "int32"), TFrom("", "int64"), TFrom("", "int8"), TFrom("", "rune"), TFrom("", "string"), TFrom("", "uint"), TFrom("", "uint16"), TFrom("", "uint32"), TFrom("", "uint64"), TFrom("", "uint8")
	T.Empty.Interface, T.Empty.Struct = TInterface(TdInterface(nil)), TStruct(TdStruct())
	T.SliceOf.Bytes, T.SliceOf.Ints, T.SliceOf.Strings = TSlice(T.Byte), TSlice(T.Int), TSlice(T.String)
	Vars.Err.Name, Vars.Err.Type, Vars.R.Name, Vars.I.Name, Vars.J.Name, Vars.K.Name, Vars.V.Name, Vars.Ok.Name, Vars.S.Name, Vars.T.Name, Vars.E.Name, Vars.KV.Name, Vars.KVs.Name, Vars.Sl.Name, Vars.Id.Name, Vars.Ids.Name = "err", T.Error, "r", "i", "j", "k", "v", "ok", "s", "t", "e", "kv", "kvs", "sl", "id", "ids"
	Sigs.NoneToBool.Rets, Sigs.NoneToString.Rets = NamedsTypeds{Vars.R.OfType(T.Bool)}, NamedsTypeds{Vars.R.OfType(T.String)}
}
