package udevgosyn

import (
	"fmt"
	"io"
)

type IEmit interface {
	Emit(IEmitter)
}

type IEmitter interface {
	io.ByteWriter
	io.Writer
	WriteRune(rune) (int, error)
	WriteString(string) (int, error)
}

func (this *Named) Emit(w IEmitter) {
	w.WriteString(this.Name)
}

func (this *NamedTyped) emit(w IEmitter, noFuncKeywordBecauseInterfaceMethod bool) {
	if this.Name != "" {
		w.WriteString(this.Name)
		w.WriteByte(' ')
	}
	this.Type.emit(w, noFuncKeywordBecauseInterfaceMethod)
}

func (this NamedsTypeds) emit(w IEmitter, sep rune, noFuncKeywordBecauseInterfaceMethods bool) {
	for i := range this {
		if i > 0 {
			w.WriteRune(sep)
		}
		this[i].emit(w, noFuncKeywordBecauseInterfaceMethods)
	}
}

func (this *TypeFunc) Emit(w IEmitter) {
	this.emit(w, false)
}

func (this *TypeFunc) emit(w IEmitter, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod bool) {
	if !noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod {
		w.WriteString("func")
	}
	w.WriteByte('(')
	this.Args.emit(w, ',', false)
	w.WriteByte(')')
	if len(this.Rets) == 1 && this.Rets[0].Name == "" {
		w.WriteByte(' ')
		this.Rets[0].Type.Emit(w)
	} else if len(this.Rets) > 0 {
		w.WriteString(" (")
		this.Rets.emit(w, ',', false)
		w.WriteByte(')')
	}
}

func (this *TypeInterface) Emit(w IEmitter) {
	w.WriteString("interface{")
	for i := range this.Embeds {
		if i > 0 {
			w.WriteByte(';')
		}
		this.Embeds[i].Emit(w)
	}
	if len(this.Embeds) > 0 && len(this.Methods) > 0 {
		w.WriteByte(';')
	}
	this.Methods.emit(w, ';', true)
	w.WriteByte('}')
}

func (this *TypeStruct) Emit(w IEmitter) {
	w.WriteString("struct{")
	for i := range this.Embeds {
		if i > 0 {
			w.WriteByte(';')
		}
		this.Embeds[i].Emit(w)
	}
	if len(this.Embeds) > 0 && len(this.Fields) > 0 {
		w.WriteByte(';')
	}
	this.Fields.emit(w, ';', false)
	w.WriteByte('}')
}

func (this *TypeRef) Emit(w IEmitter) {
	this.emit(w, false)
}

func (this *TypeRef) emit(w IEmitter, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod bool) {
	switch {
	case this.ToPtrOf != nil:
		w.WriteByte('*')
		this.ToPtrOf.Emit(w)
	case this.ToSliceOf != nil:
		w.WriteString("[]")
		this.ToSliceOf.Emit(w)
	case this.ToMapOf.Key != nil:
		w.WriteString("map[")
		this.ToMapOf.Key.Emit(w)
		w.WriteByte(']')
		this.ToMapOf.Val.Emit(w)
	case this.ToOther.TypeName != "":
		if this.ToOther.PkgName != "" {
			w.WriteString(this.ToOther.PkgName)
			w.WriteByte('.')
		}
		w.WriteString(this.ToOther.TypeName)
	case this.ToFunc != nil:
		this.ToFunc.emit(w, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod)
	case this.ToInterface != nil:
		this.ToInterface.Emit(w)
	case this.ToStruct != nil:
		this.ToStruct.Emit(w)
	case this.ToPrim.Bool:
		w.WriteString("bool")
	case this.ToPrim.Byte:
		w.WriteString("byte")
	case this.ToPrim.Complex128:
		w.WriteString("complex128")
	case this.ToPrim.Complex64:
		w.WriteString("complex64")
	case this.ToPrim.Float32:
		w.WriteString("float32")
	case this.ToPrim.Float64:
		w.WriteString("float64")
	case this.ToPrim.Int:
		w.WriteString("int")
	case this.ToPrim.Int16:
		w.WriteString("int16")
	case this.ToPrim.Int32:
		w.WriteString("int32")
	case this.ToPrim.Int64:
		w.WriteString("int64")
	case this.ToPrim.Int8:
		w.WriteString("int8")
	case this.ToPrim.Rune:
		w.WriteString("rune")
	case this.ToPrim.String:
		w.WriteString("string")
	case this.ToPrim.Uint:
		w.WriteString("uint")
	case this.ToPrim.Uint16:
		w.WriteString("uint16")
	case this.ToPrim.Uint32:
		w.WriteString("uint32")
	case this.ToPrim.Uint64:
		w.WriteString("uint64")
	case this.ToPrim.Uint8:
		w.WriteString("uint8")
	}
}

func (this *TypeDef) Emit(w IEmitter) {
	w.WriteString("type ")
	if w.WriteString(this.Name); this.IsAlias {
		w.WriteByte('=')
	}
	w.WriteByte(' ')
	this.Type.Emit(w)
}

func (this *Func) Emit(w IEmitter) {
	if w.WriteString("func "); this.Recv != nil {
		w.WriteByte('(')
		w.WriteString(this.Recv.Name)
		w.WriteByte(' ')
		this.Recv.Type.Emit(w)
		w.WriteByte(')')
	}
	w.WriteString(this.Name)
	this.Type.emit(w, true)
	w.WriteByte('{')
	for i := range this.Body {
		if i > 0 {
			w.WriteByte(';')
		}
		this.Body[i].Emit(w)
	}
	w.WriteByte('}')
}

func (this *StmtUnary) emit(w IEmitter, keywordPlusSpace string) {
	if w.WriteString(keywordPlusSpace); this.Expr != nil {
		this.Expr.Emit(w)
	}
}

func (this *StmtRet) Emit(w IEmitter) {
	this.StmtUnary.emit(w, "return ")
}

func (this *StmtDefer) Emit(w IEmitter) {
	this.StmtUnary.emit(w, "defer ")
}

func (this *StmtGo) Emit(w IEmitter) {
	this.StmtUnary.emit(w, "go ")
}

func (this *StmtConst) Emit(w IEmitter) {
	w.WriteString("const ")
	w.WriteString(this.Name)
	this.Type.Emit(w)
	w.WriteByte('=')
	this.Expr.Emit(w)
}

func (this *StmtVar) Emit(w IEmitter) {
	w.WriteString("var ")
	w.WriteString(this.Name)
	if this.Type.Emit(w); this.Expr != nil {
		w.WriteByte('=')
		this.Expr.Emit(w)
	}
}

func (this *Op) emit(w IEmitter, op string) {
	if len(this.Operands) == 1 {
		w.WriteString(op)
		this.Operands[0].Emit(w)
	} else {
		for i := range this.Operands {
			if i > 0 {
				w.WriteString(op)
			}
			this.Operands[i].Emit(w)
		}
	}
}

func (this *OpSet) Emit(w IEmitter)   { this.Op.emit(w, " = ") }
func (this *OpDecl) Emit(w IEmitter)  { this.Op.emit(w, " := ") }
func (this *OpComma) Emit(w IEmitter) { this.Op.emit(w, ",") }
func (this *OpDot) Emit(w IEmitter)   { this.Op.emit(w, ".") }
func (this *OpAnd) Emit(w IEmitter)   { this.Op.emit(w, " && ") }
func (this *OpOr) Emit(w IEmitter)    { this.Op.emit(w, " || ") }
func (this *OpEq) Emit(w IEmitter)    { this.Op.emit(w, " == ") }
func (this *OpNeq) Emit(w IEmitter)   { this.Op.emit(w, " != ") }
func (this *OpGeq) Emit(w IEmitter)   { this.Op.emit(w, " >= ") }
func (this *OpLeq) Emit(w IEmitter)   { this.Op.emit(w, " <= ") }
func (this *OpGt) Emit(w IEmitter)    { this.Op.emit(w, " > ") }
func (this *OpLt) Emit(w IEmitter)    { this.Op.emit(w, " < ") }
func (this *OpPlus) Emit(w IEmitter)  { this.Op.emit(w, "+") }
func (this *OpMinus) Emit(w IEmitter) { this.Op.emit(w, "-") }
func (this *OpMult) Emit(w IEmitter)  { this.Op.emit(w, "*") }
func (this *OpDiv) Emit(w IEmitter)   { this.Op.emit(w, "/") }
func (this *OpAddr) Emit(w IEmitter)  { this.Op.emit(w, "&") }
func (this *OpDeref) Emit(w IEmitter) { this.Op.emit(w, "*") }
func (this *OpNot) Emit(w IEmitter)   { this.Op.emit(w, "!") }
func (this *OpIdx) Emit(w IEmitter) {
	for i := range this.Operands {
		if i > 0 {
			w.WriteByte('[')
		}
		this.Operands[i].Emit(w)
		if i > 0 {
			w.WriteByte(']')
		}
	}
}

func (this *ExprLit) Emit(w IEmitter) {
	w.WriteString(fmt.Sprintf("%#v", this.Val))
}

func (this *ExprNil) Emit(w IEmitter) {
	w.WriteString("nil")
}

func (this *ExprCall) Emit(w IEmitter) {
	this.Callee.Emit(w)
	w.WriteByte('(')
	for i := range this.Args {
		if i > 0 {
			w.WriteByte(',')
		}
		this.Args[i].Emit(w)
	}
	w.WriteByte(')')
}
