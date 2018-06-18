package udevgogen

import (
	"strings"
)

// ISyn implementations represent some element in the Abstract Syntax Tree:
// literals, vars, consts, type-defs, type-refs, funcs, keywords, operators etc..
type ISyn interface {
	// generates the code represented by this `ISyn`
	Emit(IWriter)
}

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

// PkgImports maps (via its `I` method) package
// import paths to package import names.
type PkgImports map[string]string

// Returns the `pkgImportName` for the given `pkgImportPath`
// as stored in `this` (or if missing, devises one in the form
// of eg. `encodingjson` for `encoding/json` and stores it).
func (this *PkgImports) I(pkgImportPath string) (pkgImportName string) {
	self := *this
	if self == nil {
		self = map[string]string{}
	}
	if pkgImportName = self[pkgImportPath]; pkgImportName == "" {
		pkgImportName = pkgImportsStrReplSlashesToUnderscores.Replace(pkgImportPath)
		self[pkgImportPath] = pkgImportName
	}
	*this = self
	return
}

var (
	pkgImportsStrReplSlashesToUnderscores = strings.NewReplacer("/", "_")

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

func init() {
	V.Err.Name, V.Ret.Name, V.This.Name = "err", "ret", "this"
	B.Append.Name, B.Cap.Name, B.Close.Name, B.Complex.Name, B.Copy.Name, B.Delete.Name, B.Imag.Name, B.Len.Name, B.Make.Name, B.New.Name, B.Panic.Name, B.Print.Name, B.Println.Name, B.Real.Name, B.Recover.Name = "append", "cap", "close", "complex", "copy", "delete", "imag", "len", "make", "new", "panic", "print", "println", "real", "recover"
	T.Bool, T.Byte, T.Complex128, T.Complex64, T.Float32, T.Float64, T.Int, T.Int16, T.Int32, T.Int64, T.Int8, T.Rune, T.String, T.Uint, T.Uint16, T.Uint32, T.Uint64, T.Uint8 = TrNamed("", "bool"), TrNamed("", "byte"), TrNamed("", "complex128"), TrNamed("", "complex64"), TrNamed("", "float32"), TrNamed("", "float64"), TrNamed("", "int"), TrNamed("", "int16"), TrNamed("", "int32"), TrNamed("", "int64"), TrNamed("", "int8"), TrNamed("", "rune"), TrNamed("", "string"), TrNamed("", "uint"), TrNamed("", "uint16"), TrNamed("", "uint32"), TrNamed("", "uint64"), TrNamed("", "uint8")

	Sigs.NoneToBool.Rets, Sigs.NoneToString.Rets = NamedsTypeds{V.Ret.Typed(T.Bool)}, NamedsTypeds{V.Ret.Typed(T.String)}
}
