package udevgogen

import (
	"bytes"
	"fmt"
	"go/format"
	"io"
	"sort"
	"strconv"
)

type writer struct {
	bytes.Buffer
	emitNoOpFuncBodies bool
}

func (this *writer) ShouldEmitNoOpFuncBodies() bool { return this.emitNoOpFuncBodies }

// IWriter represents the buffer or other output
// stream that any `ISyn` can `Emit` code to.
type IWriter interface {
	ShouldEmitNoOpFuncBodies() bool
	io.ByteWriter
	io.Writer
	// WriteRune(rune) (int, error)
	WriteString(string) (int, error)
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
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

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
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

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this *TypeInterface) Emit(w IWriter) {
	w.WriteString("interface{")
	for i := range this.Embeds {
		this.Embeds[i].Emit(w)
		w.WriteByte(';')
	}
	this.Methods.emit(w, ';', true)
	w.WriteByte('}')
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this *TypeStruct) Emit(w IWriter) {
	w.WriteString("struct{")
	for i := range this.Fields {
		this.Fields[i].Emit(w)
		w.WriteByte(';')
	}
	w.WriteByte('}')
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
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

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
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
	}
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this TypeDecl) Emit(w IWriter) {
	w.WriteString("type ")
	if w.WriteString(this.Name); this.IsAlias {
		w.WriteByte('=')
	}
	w.WriteByte(' ')
	this.Type.Emit(w)
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
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

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
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

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (StmtBreak) Emit(w IWriter) {
	w.WriteString("break;")
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (StmtContinue) Emit(w IWriter) {
	w.WriteString("continue;")
}

func (this StmtUnary) emit(w IWriter, keywordPlusSpace string) {
	if w.WriteString(keywordPlusSpace); this.Expr != nil {
		this.Expr.Emit(w)
	}
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this StmtRet) Emit(w IWriter) {
	this.StmtUnary.emit(w, "return ")
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this StmtDefer) Emit(w IWriter) {
	this.StmtUnary.emit(w, "defer ")
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this StmtGo) Emit(w IWriter) {
	this.StmtUnary.emit(w, "go ")
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this *StmtConst) Emit(w IWriter) {
	w.WriteString("const ")
	w.WriteString(this.Name)
	this.Type.Emit(w)
	w.WriteByte('=')
	this.Expr.Emit(w)
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this *StmtVar) Emit(w IWriter) {
	w.WriteString("var ")
	w.WriteString(this.Name)
	if this.Type.Emit(w); this.Expr != nil {
		w.WriteByte('=')
		this.Expr.Emit(w)
	}
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this *StmtIf) Emit(w IWriter) {
	for i := range this.IfThens {
		w.WriteString("if ")
		this.IfThens[i].Cond.Emit(w)
		this.IfThens[i].emit(w, true, "")
		w.WriteString(" else ")
	}
	this.Else.emit(w, true, "")
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this *StmtSwitch) Emit(w IWriter) {
	w.WriteString("switch ")
	if this.Scrutinee != nil {
		this.Scrutinee.Emit(w)
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

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
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

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpSet) Emit(w IWriter) { this.Op.emit(w, " = ") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpDecl) Emit(w IWriter) { this.Op.emit(w, " := ") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpComma) Emit(w IWriter) { this.Op.emit(w, ",") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpDot) Emit(w IWriter) { this.Op.emit(w, ".") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpAnd) Emit(w IWriter) { this.Op.emit(w, " && ") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpOr) Emit(w IWriter) { this.Op.emit(w, " || ") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpEq) Emit(w IWriter) { this.Op.emit(w, " == ") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpNeq) Emit(w IWriter) { this.Op.emit(w, " != ") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpGeq) Emit(w IWriter) { this.Op.emit(w, " >= ") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpLeq) Emit(w IWriter) { this.Op.emit(w, " <= ") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpGt) Emit(w IWriter) { this.Op.emit(w, " > ") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpLt) Emit(w IWriter) { this.Op.emit(w, " < ") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpAdd) Emit(w IWriter) { this.Op.emit(w, "+") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpSub) Emit(w IWriter) { this.Op.emit(w, "-") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpMul) Emit(w IWriter) { this.Op.emit(w, "*") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpDiv) Emit(w IWriter) { this.Op.emit(w, "/") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpAddr) Emit(w IWriter) { this.Op.emit(w, "&") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpDeref) Emit(w IWriter) { this.Op.emit(w, "*") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this OpNot) Emit(w IWriter) { this.Op.emit(w, "!") }

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
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

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this ExprLit) Emit(w IWriter) {
	w.WriteString(fmt.Sprintf("%#v", this.Val))
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (ExprNil) Emit(w IWriter) {
	w.WriteString("nil")
}

// Emit implements `ISyn.Emit(IWriter)` to generate the code represented by `this`.
func (this *ExprCall) Emit(w IWriter) {
	this.Callee.Emit(w)
	w.WriteByte('(')
	for i := range this.Args {
		this.Args[i].Emit(w)
		w.WriteByte(',')
	}
	w.WriteByte(')')
}

// Emit generates the code represented by `this`.
func (this *SourceFile) Emit(w IWriter, codeGenCommentNotice string, pkgImportPathsToNames PkgImports) {
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

// Src calls `this.Emit` to generate the code into `src`, and then `go/format`s it.
// Any `err` returned is from `go/format`, and if so, `src` will instead contain
// the original non-formatted generated code to aid troubleshooting the issue.
func (this *SourceFile) Src(codeGenCommentNotice string, emitNoOpFuncBodies bool, pkgImportPathsToNames PkgImports) (src []byte, err error) {
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