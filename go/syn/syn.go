package udevgosyn

type Named struct {
	Name string
}

type NamedTyped struct {
	Named
	Type TypeRef
}

type NamedsTypeds []*NamedTyped

type TypeFunc struct {
	Args NamedsTypeds
	Rets NamedsTypeds
}

type TypeInterface struct {
	Embeds  []*TypeRef
	Methods NamedsTypeds
}

type TypeStruct struct {
	Embeds []*TypeRef
	Fields NamedsTypeds
}

type TypeDef struct {
	NamedTyped
	IsAlias bool
}

type TypeRef struct {
	ToPrim struct {
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
	ToOther struct {
		PkgName  string
		TypeName string
	}
	ToSliceOf *TypeRef
	ToPtrOf   *TypeRef
	ToMapOf   struct {
		Key *TypeRef
		Val *TypeRef
	}
	ToFunc      *TypeFunc
	ToInterface *TypeInterface
	ToStruct    *TypeStruct
}

type Func struct {
	NamedTyped
	Recv *NamedTyped
	Body []IEmit
}

type stmtSimple struct {
	Expr IEmit
}

type StmtRet struct {
	stmtSimple
}

type StmtDefer struct {
	stmtSimple
}

type StmtGo struct {
	stmtSimple
}

type StmtConst struct {
	NamedTyped
	Expr ExprLit
}

type StmtVar struct {
	NamedTyped
	Expr IEmit
}

type Op struct {
	Operands []IEmit
}

type OpSet struct{ Op }
type OpDecl struct{ Op }
type OpTup struct{ Op }
type OpDot struct{ Op }
type OpAnd struct{ Op }
type OpOr struct{ Op }
type OpEq struct{ Op }
type OpNeq struct{ Op }
type OpGeq struct{ Op }
type OpLeq struct{ Op }
type OpGt struct{ Op }
type OpLt struct{ Op }
type OpPlus struct{ Op }
type OpMinus struct{ Op }
type OpMult struct{ Op }
type OpDiv struct{ Op }
type OpIdx struct{ Op }
type OpAddr struct{ Op }
type OpDeref struct{ Op }
type OpNot struct{ Op }

type ExprLit struct {
	Val interface{}
}

type ExprName struct {
	Named
}

type ExprNil struct {
}

type ExprCall struct {
	Callee IEmit
	Args   []IEmit
}
