package udevgogen

import (
	"bytes"
	"fmt"
	"go/format"
	"io"
	"sort"
	"strconv"
)

// ISyn can be any legal element in the Abstract Syntax Tree:
// literals, vars, consts, type-defs, type-refs, funcs, keywords, operators etc..
type ISyn interface {
	Emit(IWriter)
}

type writer struct {
	bytes.Buffer
	emitNoOpFuncBodies bool
}

func (this *writer) ShouldEmitNoOpFuncBodies() bool { return this.emitNoOpFuncBodies }

type IWriter interface {
	ShouldEmitNoOpFuncBodies() bool
	io.ByteWriter
	io.Writer
	// WriteRune(rune) (int, error)
	WriteString(string) (int, error)
}

func (this Named) Emit(w IWriter) {
	w.WriteString(this.Name)
}

func (this NamedTyped) emit(w IWriter, noFuncKeywordBecauseInterfaceMethod bool) {
	if this.Name != "" {
		w.WriteString(this.Name)
		w.WriteByte(' ')
	}
	this.Type.emit(w, noFuncKeywordBecauseInterfaceMethod)
}

func (this NamedsTypeds) emit(w IWriter, sep byte, noFuncKeywordBecauseInterfaceMethods bool) {
	for i := range this {
		if i > 0 {
			w.WriteByte(sep)
		}
		this[i].emit(w, noFuncKeywordBecauseInterfaceMethods)
	}
}

func (this *TypeFunc) Emit(w IWriter) {
	this.emit(w, false)
}

func (this *TypeFunc) emit(w IWriter, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod bool) {
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

func (this *TypeInterface) Emit(w IWriter) {
	w.WriteString("interface{")
	for i := range this.Embeds {
		this.Embeds[i].Emit(w)
		w.WriteByte(';')
	}
	this.Methods.emit(w, ';', true)
	w.WriteByte('}')
}

func (this *TypeStruct) Emit(w IWriter) {
	w.WriteString("struct{")
	for i := range this.Embeds {
		this.Embeds[i].Emit(w)
		w.WriteByte(';')
	}
	for i := range this.Fields {
		this.Fields[i].Emit(w)
		w.WriteByte(';')
	}
	w.WriteByte('}')
}

func (this *SynStructField) Emit(w IWriter) {
	this.NamedTyped.emit(w, false)
	if len(this.Tags) > 0 {
		w.WriteByte('`')
		idx, sortednames := 0, make(sort.StringSlice, len(this.Tags))
		for tagname := range this.Tags {
			sortednames[idx], idx = tagname, idx+1
		}
		sortednames.Sort()
		for _, tagname := range sortednames {
			w.WriteString(tagname)
			w.WriteByte(':')
			w.WriteString(strconv.Quote(this.Tags[tagname]))
			w.WriteByte(' ')
		}
		w.WriteByte('`')
	}
}

func (this *TypeRef) Emit(w IWriter) {
	this.emit(w, false)
}

func (this *TypeRef) emit(w IWriter, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod bool) {
	switch {
	case this.Ptr != nil:
		w.WriteByte('*')
		this.Ptr.Emit(w)
	case this.Slice != nil:
		w.WriteString("[]")
		this.Slice.Emit(w)
	case this.Map.Key != nil:
		w.WriteString("map[")
		this.Map.Key.Emit(w)
		w.WriteByte(']')
		this.Map.Val.Emit(w)
	case this.Named.TypeName != "":
		if this.Named.PkgName != "" {
			w.WriteString(this.Named.PkgName)
			w.WriteByte('.')
		}
		w.WriteString(this.Named.TypeName)
	case this.Func != nil:
		this.Func.emit(w, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod)
	case this.Interface != nil:
		this.Interface.Emit(w)
	case this.Struct != nil:
		this.Struct.Emit(w)
	case this.Prim.Bool:
		w.WriteString("bool")
	case this.Prim.Byte:
		w.WriteString("byte")
	case this.Prim.Complex128:
		w.WriteString("complex128")
	case this.Prim.Complex64:
		w.WriteString("complex64")
	case this.Prim.Float32:
		w.WriteString("float32")
	case this.Prim.Float64:
		w.WriteString("float64")
	case this.Prim.Int:
		w.WriteString("int")
	case this.Prim.Int16:
		w.WriteString("int16")
	case this.Prim.Int32:
		w.WriteString("int32")
	case this.Prim.Int64:
		w.WriteString("int64")
	case this.Prim.Int8:
		w.WriteString("int8")
	case this.Prim.Rune:
		w.WriteString("rune")
	case this.Prim.String:
		w.WriteString("string")
	case this.Prim.Uint:
		w.WriteString("uint")
	case this.Prim.Uint16:
		w.WriteString("uint16")
	case this.Prim.Uint32:
		w.WriteString("uint32")
	case this.Prim.Uint64:
		w.WriteString("uint64")
	case this.Prim.Uint8:
		w.WriteString("uint8")
	}
}

func (this TypeDecl) Emit(w IWriter) {
	w.WriteString("type ")
	if w.WriteString(this.Name); this.IsAlias {
		w.WriteByte('=')
	}
	w.WriteByte(' ')
	this.Type.Emit(w)
}

func (this SynBlock) Emit(w IWriter) {
	this.emit(w, true, "")
	w.WriteByte(';')
}

func (this SynBlock) emit(w IWriter, wrapInCurlyBraces bool, sep string, appendToBody ...ISyn) {
	if sep == "" {
		sep = "   ;   "
	}
	if wrapInCurlyBraces {
		w.WriteByte('{')
	}
	for i := range this.Body {
		this.Body[i].Emit(w)
		w.WriteString(sep)
	}
	for i := range appendToBody {
		appendToBody[i].Emit(w)
		w.WriteString(sep)
	}
	if wrapInCurlyBraces {
		w.WriteByte('}')
	}
}

func (this *SynFunc) Emit(w IWriter) {
	doc, noop := this.Doc, w.ShouldEmitNoOpFuncBodies()
	if noop {
		doc = append(doc, "As per your current (and presumably temporary) go-gent code-gen settings, this method is effectively a no-op (so each of its return values will always equal its type's zero-value).")
	}
	if len(doc) > 0 {
		w.WriteString("\n\n")
		for i, doccommentpara := range doc {
			if i > 0 {
				w.WriteString("// \n")
			}
			w.WriteString("// ")
			w.WriteString(doccommentpara)
			w.WriteByte('\n')
		}
	}

	if w.WriteString("func "); this.Recv.Type != nil {
		w.WriteByte('(')
		w.WriteString(this.Recv.Name)
		w.WriteByte(' ')
		this.Recv.Type.Emit(w)
		w.WriteByte(')')
	}
	w.WriteString(this.Name)
	if this.Type.emit(w, true); noop {
		SynBlock{}.emit(w, true, "", K.Ret)
	} else {
		this.SynBlock.emit(w, true, "", K.Ret)
	}
}

func (StmtBreak) Emit(w IWriter) {
	w.WriteString("break;")
}

func (StmtContinue) Emit(w IWriter) {
	w.WriteString("continue;")
}

func (this StmtUnary) emit(w IWriter, keywordPlusSpace string) {
	if w.WriteString(keywordPlusSpace); this.Expr != nil {
		this.Expr.Emit(w)
	}
}

func (this StmtRet) Emit(w IWriter) {
	this.StmtUnary.emit(w, "return ")
}

func (this StmtDefer) Emit(w IWriter) {
	this.StmtUnary.emit(w, "defer ")
}

func (this StmtGo) Emit(w IWriter) {
	this.StmtUnary.emit(w, "go ")
}

func (this *StmtConst) Emit(w IWriter) {
	w.WriteString("const ")
	w.WriteString(this.Name)
	this.Type.Emit(w)
	w.WriteByte('=')
	this.Expr.Emit(w)
}

func (this *StmtVar) Emit(w IWriter) {
	w.WriteString("var ")
	w.WriteString(this.Name)
	if this.Type.Emit(w); this.Expr != nil {
		w.WriteByte('=')
		this.Expr.Emit(w)
	}
}

func (this *StmtIf) Emit(w IWriter) {
	for i := range this.IfThens {
		w.WriteString("if ")
		this.IfThens[i].Cond.Emit(w)
		this.IfThens[i].emit(w, true, "")
		w.WriteString(" else ")
	}
	this.Else.emit(w, true, "")
}

func (this *StmtSwitch) Emit(w IWriter) {
	w.WriteString("switch ")
	if this.Cond != nil {
		this.Cond.Emit(w)
	}
	w.WriteByte('{')
	for i := range this.Cases {
		w.WriteString("case ")
		this.Cases[i].Cond.Emit(w)
		w.WriteByte(':')
		this.Cases[i].emit(w, false, "")
	}
	if len(this.Default.Body) > 0 {
		w.WriteString("default: ")
		this.Default.emit(w, false, "")
	}
	w.WriteByte('}')
}

func (this *StmtFor) Emit(w IWriter) {
	if this.Range.Iteree != nil {
		this.emitRange(w)
	} else {
		this.emitLoop(w)
	}
}

func (this *StmtFor) emitRange(w IWriter) {
	w.WriteString("for ")
	if this.Range.Idx.Name != "" || this.Range.Val.Name != "" {
		if this.Range.Idx.Name == "" {
			w.WriteByte('_')
		} else {
			this.Range.Idx.Emit(w)
		}
		if this.Range.Val.Name != "" {
			w.WriteByte(',')
			this.Range.Val.Emit(w)
		}
		w.WriteString(" := ")
	}
	w.WriteString("range")
	this.Range.Iteree.Emit(w)
	this.emit(w, true, "")
}

func (this *StmtFor) emitLoop(w IWriter) {
	w.WriteString("for ")
	if this.Loop.Init != nil {
		this.Loop.Init.Emit(w)
	}
	w.WriteByte(';')
	if this.Loop.Cond != nil {
		this.Loop.Cond.Emit(w)
	}
	w.WriteByte(';')
	if this.Loop.Each != nil {
		this.Loop.Each.Emit(w)
	}
	this.emit(w, true, "")
}

func (this Op) emit(w IWriter, operator string) {
	unary := len(this.Operands) == 1
	for i := range this.Operands {
		if i > 0 || unary {
			w.WriteString(operator)
		}
		_, isanotheroperator := this.Operands[i].(interface{ isOp() })
		if isanotheroperator {
			w.WriteByte('(')
		}
		this.Operands[i].Emit(w)
		if isanotheroperator {
			w.WriteByte(')')
		}
	}
}

func (Op) isOp() {}

func (this OpSet) Emit(w IWriter)   { this.Op.emit(w, " = ") }
func (this OpDecl) Emit(w IWriter)  { this.Op.emit(w, " := ") }
func (this OpComma) Emit(w IWriter) { this.Op.emit(w, ",") }
func (this OpDot) Emit(w IWriter)   { this.Op.emit(w, ".") }
func (this OpAnd) Emit(w IWriter)   { this.Op.emit(w, " && ") }
func (this OpOr) Emit(w IWriter)    { this.Op.emit(w, " || ") }
func (this OpEq) Emit(w IWriter)    { this.Op.emit(w, " == ") }
func (this OpNeq) Emit(w IWriter)   { this.Op.emit(w, " != ") }
func (this OpGeq) Emit(w IWriter)   { this.Op.emit(w, " >= ") }
func (this OpLeq) Emit(w IWriter)   { this.Op.emit(w, " <= ") }
func (this OpGt) Emit(w IWriter)    { this.Op.emit(w, " > ") }
func (this OpLt) Emit(w IWriter)    { this.Op.emit(w, " < ") }
func (this OpAdd) Emit(w IWriter)   { this.Op.emit(w, "+") }
func (this OpSub) Emit(w IWriter)   { this.Op.emit(w, "-") }
func (this OpMul) Emit(w IWriter)   { this.Op.emit(w, "*") }
func (this OpDiv) Emit(w IWriter)   { this.Op.emit(w, "/") }
func (this OpAddr) Emit(w IWriter)  { this.Op.emit(w, "&") }
func (this OpDeref) Emit(w IWriter) { this.Op.emit(w, "*") }
func (this OpNot) Emit(w IWriter)   { this.Op.emit(w, "!") }
func (this OpIdx) Emit(w IWriter) {
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

func (this ExprLit) Emit(w IWriter) {
	w.WriteString(fmt.Sprintf("%#v", this.Val))
}

func (ExprNil) Emit(w IWriter) {
	w.WriteString("nil")
}

func (this *ExprCall) Emit(w IWriter) {
	this.Callee.Emit(w)
	w.WriteByte('(')
	for i := range this.Args {
		this.Args[i].Emit(w)
		w.WriteByte(',')
	}
	w.WriteByte(')')
}

func (this *SynFile) Emit(w IWriter, codeGenCommentNotice string, pkgImportPathsToNames PkgImports) {
	w.WriteString("package ")
	w.WriteString(this.PkgName)
	if len(codeGenCommentNotice) > 0 {
		w.WriteString("\n\n// ")
		w.WriteString(codeGenCommentNotice)
	}
	w.WriteString("\n\n")

	if len(pkgImportPathsToNames) > 0 {
		w.WriteString("import (")
		for pkgpath, pkgname := range pkgImportPathsToNames {
			w.WriteString(pkgname)
			w.WriteString(" \"")
			w.WriteString(pkgpath)
			w.WriteString("\";")
		}
		w.WriteString(")\n\n")
	}

	this.emit(w, false, "\n\n")
}

func (this *SynFile) Src(codeGenCommentNotice string, emitNoOpFuncBodies bool, pkgImportPathsToNames PkgImports) (src []byte, err error) {
	buf := writer{emitNoOpFuncBodies: emitNoOpFuncBodies}
	this.Emit(&buf, codeGenCommentNotice, pkgImportPathsToNames)

	const gofmt = true
	if orig := buf.Bytes(); !gofmt {
		src = orig
	} else if src, err = format.Source(orig); err != nil {
		src = orig
	}
	return
}
