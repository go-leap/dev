// Package github.com/go-leap/dev/go/gen provides AST constructs for generating Go code.
// These are by design much simpler and leaner than `go/ast`, given that the latter is
// designed to represent existing & parsed code, while the former is for on-the-fly
// construction of newly-to-be-emitted code.
//
// Some language primitives I haven't needed to emit yet aren't covered yet, to be
// added when they're first needed (such as fixed-size-array types or `chan`nels).
//
// As a noteworthy goodie, all `func`s that have named return values automatically get a final
// `return` statement appended to their `Body` at code-gen time, if they don't already have one.
package udevgogen

import (
	"strings"
)

// ISyn implementations represent some discrete item in the Abstract Syntax Tree:
// literals, vars, consts, type-defs, type-refs, funcs, keywords, operators etc..
type ISyn interface {
	// generates the code represented by this `ISyn`
	emitTo(*writer)
}

// Syns is a slice of `ISyn`s.
type Syns []ISyn

// Add is a convenience short-hand for `append`.
func (this *Syns) Add(syns ...ISyn) { *this = append(*this, syns...) }

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

// PkgImports maps (via its `Ensure` method) package
// import paths to package import names.
type PkgImports map[string]string

// Ensure returns the `pkgImportName` for the given `pkgImportPath`
// as stored in `this` (or if missing, devises one in the form of eg.
// `pkg__encoding_json` for `encoding/json` and stores it, assuming
// that `PkgImportNamePrefix` is set to "pkg__", its default value).
func (this *PkgImports) Ensure(pkgImportPath string) (pkgImportName string) {
	self := *this
	if self == nil {
		self = map[string]string{}
	}
	if pkgImportName = self[pkgImportPath]; pkgImportName == "" {
		pkgImportName = PkgImportNamePrefix + pkgImportsStrReplSlashesToUnderscores.Replace(pkgImportPath)
		self[pkgImportPath] = pkgImportName
	}
	*this = self
	return
}

var (
	pkgImportsStrReplSlashesToUnderscores = strings.NewReplacer("/", "_")

	// see `PkgImports.Ensure(string) string` for details
	PkgImportNamePrefix = "pkg__"

	// intended to remain zero-valued (Name="" and Type=nil)
	NoMethodRecv NamedTyped

	// intended to remain zero-valued (Name="")
	None Named

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
		// `ok`
		Ok Named
		// `r`
		R Named
		// `i`
		I Named
		// `k`
		K Named
		// `v`
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
	V.Err.Name, V.Err.Type, V.R.Name, V.This.Name, V.I.Name, V.K.Name, V.V.Name, V.Ok.Name = "err", TrNamed("", "error"), "r", "this", "i", "k", "v", "ok"
	B.Append.Name, B.Cap.Name, B.Close.Name, B.Complex.Name, B.Copy.Name, B.Delete.Name, B.Imag.Name, B.Len.Name, B.Make.Name, B.New.Name, B.Panic.Name, B.Print.Name, B.Println.Name, B.Real.Name, B.Recover.Name = "append", "cap", "close", "complex", "copy", "delete", "imag", "len", "make", "new", "panic", "print", "println", "real", "recover"
	T.Bool, T.Byte, T.Complex128, T.Complex64, T.Float32, T.Float64, T.Int, T.Int16, T.Int32, T.Int64, T.Int8, T.Rune, T.String, T.Uint, T.Uint16, T.Uint32, T.Uint64, T.Uint8 = TrNamed("", "bool"), TrNamed("", "byte"), TrNamed("", "complex128"), TrNamed("", "complex64"), TrNamed("", "float32"), TrNamed("", "float64"), TrNamed("", "int"), TrNamed("", "int16"), TrNamed("", "int32"), TrNamed("", "int64"), TrNamed("", "int8"), TrNamed("", "rune"), TrNamed("", "string"), TrNamed("", "uint"), TrNamed("", "uint16"), TrNamed("", "uint32"), TrNamed("", "uint64"), TrNamed("", "uint8")
	T.Interface = TrInterface(TdInterface(nil))
	T.Sl.Ints, T.Sl.Strings = TrSlice(T.Int), TrSlice(T.String)

	Sigs.NoneToBool.Rets, Sigs.NoneToString.Rets = NamedsTypeds{V.R.Typed(T.Bool)}, NamedsTypeds{V.R.Typed(T.String)}
}
