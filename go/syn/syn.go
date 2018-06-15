package udevgosyn

type NamedTyped struct {
	Name string
	Type TypeRef
}

type TypeFunc struct {
	Args []*NamedTyped
	Rets []*NamedTyped
}

type TypeInterface struct {
	Embeds  []*TypeRef
	Methods map[string]*TypeFunc
}

type TypeStruct struct {
	Embeds []*TypeRef
	Fields []*NamedTyped
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
