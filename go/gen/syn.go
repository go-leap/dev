package udevgogen

import (
	"strings"
)

type DocCommentSingleLineParagraphs []string

func (this *DocCommentSingleLineParagraphs) Add(docCommentLines ...string) {
	*this = append(*this, docCommentLines...)
}

type Named struct{ Name string }

func (this Named) Typed(typeRef *TypeRef) (nt NamedTyped) {
	nt.Named, nt.Type = this, typeRef
	return
}

type NamedTyped struct {
	Named
	Type *TypeRef
}

type NamedsTypeds []NamedTyped

type TypeFunc struct {
	Args NamedsTypeds
	Rets NamedsTypeds
}

type TypeInterface struct {
	Embeds  []TypeRef
	Methods NamedsTypeds
}

type TypeStruct struct {
	Embeds []TypeRef
	Fields []SynStructField
}

type SynStructField struct {
	NamedTyped
	Tags map[string]string
}

type TypeDecl struct {
	NamedTyped
	IsAlias bool
}

type TypeRef struct {
	Slice *TypeRef
	Ptr   *TypeRef
	Map   struct {
		Key *TypeRef
		Val *TypeRef
	}
	Func      *TypeFunc
	Interface *TypeInterface
	Struct    *TypeStruct
	Named     struct {
		PkgName  string
		TypeName string
	}
	Prim struct {
		Bool       bool
		Byte       bool
		Complex64  bool
		Complex128 bool
		Float32    bool
		Float64    bool
		Int8       bool
		Int16      bool
		Int32      bool
		Int64      bool
		Int        bool
		Uint       bool
		Uint8      bool
		Uint16     bool
		Uint32     bool
		Uint64     bool
		Rune       bool
		String     bool
	}
}

type SynBlock struct {
	Body []ISyn
}

func (this *SynBlock) Add(stmts ...ISyn) { this.Body = append(this.Body, stmts...) }

type SynFunc struct {
	SynBlock
	NamedTyped
	Recv NamedTyped
	Doc  DocCommentSingleLineParagraphs
}

type StmtUnary struct {
	Expr ISyn
}

type StmtBreak struct{}

type StmtContinue struct{}

type StmtRet struct {
	StmtUnary
}

type StmtDefer struct {
	StmtUnary
}

type StmtGo struct {
	StmtUnary
}

type StmtConst struct {
	NamedTyped
	Expr ExprLit
}

type StmtVar struct {
	NamedTyped
	Expr ISyn
}

type StmtIf struct {
	IfThens []SynCond
	Else    SynBlock
}

type SynCond struct {
	Cond ISyn
	SynBlock
}

type StmtSwitch struct {
	Cond    ISyn
	Cases   []SynCond
	Default SynBlock
}

type StmtFor struct {
	SynBlock
	Range struct {
		Idx    Named
		Val    Named
		Iteree ISyn
	}
	Loop struct {
		Init ISyn
		Cond ISyn
		Each ISyn
	}
}

type Op struct {
	Operands []ISyn
}

type OpSet struct{ Op }
type OpDecl struct{ Op }
type OpComma struct{ Op }
type OpDot struct{ Op }
type OpAnd struct{ Op }
type OpOr struct{ Op }
type OpEq struct{ Op }
type OpNeq struct{ Op }
type OpGeq struct{ Op }
type OpLeq struct{ Op }
type OpGt struct{ Op }
type OpLt struct{ Op }
type OpAdd struct{ Op }
type OpSub struct{ Op }
type OpMul struct{ Op }
type OpDiv struct{ Op }
type OpIdx struct{ Op }
type OpAddr struct{ Op }
type OpDeref struct{ Op }
type OpNot struct{ Op }

type ExprLit struct {
	Val interface{}
}

type ExprNil struct {
}

type ExprCall struct {
	Callee ISyn
	Args   []ISyn
}

type PkgImports map[string]string

func (this *PkgImports) I(pkgImportPath string) (pkgImportName string) {
	self := *this
	if self == nil {
		self = map[string]string{}
	}
	if pkgImportName = self[pkgImportPath]; pkgImportName == "" {
		pkgImportName = strSlashesToUnderscores.Replace(pkgImportPath)
		self[pkgImportPath] = pkgImportName
	}
	*this = self
	return
}

var strSlashesToUnderscores = strings.NewReplacer("/", "_")

type SynFile struct {
	PkgName string
	SynBlock
}
