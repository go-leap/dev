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

type Op2 struct {
	Left  IEmit
	Right IEmit
}

type Op2Set struct{ Op2 }
type Op2Decl struct{ Op2 }
type Op2Tup struct{ Op2 }
type Op2Dot struct{ Op2 }
type Op2And struct{ Op2 }
type Op2Or struct{ Op2 }
type Op2Eq struct{ Op2 }
type Op2Neq struct{ Op2 }
type Op2Geq struct{ Op2 }
type Op2Leq struct{ Op2 }
type Op2Gt struct{ Op2 }
type Op2Lt struct{ Op2 }
type Op2Plus struct{ Op2 }
type Op2Minus struct{ Op2 }
type Op2Mult struct{ Op2 }
type Op2Div struct{ Op2 }
type Op2Idx struct{ Op2 }

type Op1 struct {
	Right IEmit
}

type Op1Addr struct{ Op1 }
type Op1Deref struct{ Op1 }
type Op1Not struct{ Op1 }
type Op1Minus struct{ Op1 }

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
