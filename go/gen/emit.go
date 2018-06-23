package udevgogen

import (
	"bytes"
	"fmt"
	"go/format"
	"sort"
	"strconv"
	"time"
)

type writer struct {
	bytes.Buffer
	emitNoOpFuncBodies        bool
	pkgImportsRegistered      PkgImports
	pkgImportsActuallyEmitted map[string]bool
}

func (this *writer) shouldEmitNoOpFuncBodies() bool { return this.emitNoOpFuncBodies }

func (this Named) emitTo(w *writer) {
	w.WriteString(this.Name)
}

func (this NamedTyped) emit(w *writer, noFuncKeywordBecauseInterfaceMethod bool) {
	if this.Name != "" {
		w.WriteString(this.Name)
		w.WriteByte(' ')
	}
	this.Type.emit(w, noFuncKeywordBecauseInterfaceMethod)
}

func (this NamedsTypeds) emit(w *writer, sep byte, noFuncKeywordBecauseInterfaceMethods bool) {
	for i := range this {
		if i > 0 {
			w.WriteByte(sep)
		}
		this[i].emit(w, noFuncKeywordBecauseInterfaceMethods)
	}
}

func (this *TypeFunc) emitTo(w *writer) {
	this.emit(w, false)
}

func (this *TypeFunc) emit(w *writer, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod bool) {
	if !noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod {
		w.WriteString("func")
	}
	w.WriteByte('(')
	this.Args.emit(w, ',', false)
	w.WriteByte(')')
	if len(this.Rets) == 1 && this.Rets[0].Name == "" {
		w.WriteByte(' ')
		this.Rets[0].Type.emitTo(w)
	} else if len(this.Rets) > 0 {
		w.WriteString(" (")
		this.Rets.emit(w, ',', false)
		w.WriteByte(')')
	}
}

func (this *TypeInterface) emitTo(w *writer) {
	w.WriteString("interface{")
	for i := range this.Embeds {
		this.Embeds[i].emitTo(w)
		w.WriteByte(';')
	}
	this.Methods.emit(w, ';', true)
	w.WriteByte('}')
}

func (this *TypeStruct) emitTo(w *writer) {
	w.WriteString("struct{")
	for i := range this.Fields {
		this.Fields[i].emitTo(w)
		w.WriteByte(';')
	}
	w.WriteByte('}')
}

func (this *SynStructField) emitTo(w *writer) {
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

func (this *TypeRef) emitTo(w *writer) {
	this.emit(w, false)
}

func (this *TypeRef) emit(w *writer, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod bool) {
	switch {
	case this.Ptr != nil:
		w.WriteByte('*')
		this.Ptr.emitTo(w)
	case this.Slice != nil:
		w.WriteString("[]")
		this.Slice.emitTo(w)
	case this.Map.Key != nil:
		w.WriteString("map[")
		this.Map.Key.emitTo(w)
		w.WriteByte(']')
		this.Map.Val.emitTo(w)
	case this.Named.TypeName != "":
		if this.Named.PkgName != "" {
			w.pkgImportsActuallyEmitted[this.Named.PkgName] = true
			w.WriteString(this.Named.PkgName)
			w.WriteByte('.')
		}
		w.WriteString(this.Named.TypeName)
	case this.Func != nil:
		this.Func.emit(w, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod)
	case this.Interface != nil:
		this.Interface.emitTo(w)
	case this.Struct != nil:
		this.Struct.emitTo(w)
	}
}

func (this TypeDecl) emitTo(w *writer) {
	w.WriteString("type ")
	if w.WriteString(this.Name); this.IsAlias {
		w.WriteByte('=')
	}
	w.WriteByte(' ')
	this.Type.emitTo(w)
}

func (this SynBlock) emitTo(w *writer) {
	this.emit(w, true, "", false)
	w.WriteByte(';')
}

func (this SynBlock) emit(w *writer, wrapInCurlyBraces bool, sep string, addFinalRet bool) {
	if sep == "" {
		sep = "; "
	}
	if wrapInCurlyBraces {
		w.WriteByte('{')
	}
	for i := range this.Body {
		this.Body[i].emitTo(w)
		w.WriteString(sep)
	}
	if addFinalRet {
		K.Ret.emitTo(w)
	}
	if wrapInCurlyBraces {
		w.WriteByte('}')
	}
}

func (this *SynFunc) emitTo(w *writer) {
	doc, noop, hasfinalret, hasnamedrets := this.Doc, w.shouldEmitNoOpFuncBodies(), false, this.Type.Func.Rets.AllNamed()
	if len(this.Type.Func.Rets) > 0 && len(this.Body) > 0 {
		if _, hasfinalret = this.Body[len(this.Body)-1].(*StmtRet); !hasfinalret {
			_, hasfinalret = this.Body[len(this.Body)-1].(StmtRet)
		}
	}
	if noop = noop && (len(this.Type.Func.Rets) == 0 || hasnamedrets); noop {
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
		this.Recv.Type.emitTo(w)
		w.WriteByte(')')
	}
	w.WriteString(this.Name)
	if this.Type.emit(w, true); noop {
		K.Ret.emitTo(w)
	} else {
		this.SynBlock.emit(w, true, "", hasnamedrets && !hasfinalret)
	}
}

func (StmtBreak) emitTo(w *writer) {
	w.WriteString("break;")
}

func (StmtContinue) emitTo(w *writer) {
	w.WriteString("continue;")
}

func (this StmtUnary) emit(w *writer, keywordPlusSpace string) {
	if w.WriteString(keywordPlusSpace); this.Expr != nil {
		this.Expr.emitTo(w)
	}
}

func (this StmtRet) emitTo(w *writer) {
	this.StmtUnary.emit(w, "return ")
}

func (this StmtDefer) emitTo(w *writer) {
	this.StmtUnary.emit(w, "defer ")
}

func (this StmtGo) emitTo(w *writer) {
	this.StmtUnary.emit(w, "go ")
}

func (this *StmtConst) emitTo(w *writer) {
	w.WriteString("const ")
	w.WriteString(this.Name)
	this.Type.emitTo(w)
	w.WriteByte('=')
	this.Expr.emitTo(w)
}

func (this *StmtVar) emitTo(w *writer) {
	w.WriteString("var ")
	w.WriteString(this.Name)
	w.WriteByte(' ')
	if this.Type.emitTo(w); this.Expr != nil {
		w.WriteByte('=')
		this.Expr.emitTo(w)
	}
}

func (this *StmtIf) emitTo(w *writer) {
	finali, finalelse := len(this.IfThens)-1, len(this.Else.Body) > 0
	for i := range this.IfThens {
		w.WriteString("if ")
		this.IfThens[i].Cond.emitTo(w)
		this.IfThens[i].emit(w, true, "", false)
		if i != finali || finalelse {
			w.WriteString(" else ")
		}
	}
	if finalelse {
		this.Else.emit(w, true, "", false)
	}
}

func (this *StmtSwitch) emitTo(w *writer) {
	w.WriteString("switch ")
	if this.Scrutinee != nil {
		this.Scrutinee.emitTo(w)
	}
	w.WriteByte('{')
	for i := range this.Cases {
		w.WriteString("case ")
		this.Cases[i].Cond.emitTo(w)
		w.WriteByte(':')
		this.Cases[i].emit(w, false, "", false)
	}
	if len(this.Default.Body) > 0 {
		w.WriteString("default: ")
		this.Default.emit(w, false, "", false)
	}
	w.WriteByte('}')
}

func (this *StmtFor) emitTo(w *writer) {
	if this.Range.Iteree != nil {
		this.emitRange(w)
	} else {
		this.emitLoop(w)
	}
}

func (this *StmtFor) emitRange(w *writer) {
	w.WriteString("for ")
	if this.Range.Idx.Name != "" || this.Range.Val.Name != "" {
		if this.Range.Idx.Name == "" {
			w.WriteByte('_')
		} else {
			this.Range.Idx.emitTo(w)
		}
		if this.Range.Val.Name != "" {
			w.WriteByte(',')
			this.Range.Val.emitTo(w)
		}
		w.WriteString(" := ")
	}
	w.WriteString("range")
	this.Range.Iteree.emitTo(w)
	this.emit(w, true, "", false)
}

func (this *StmtFor) emitLoop(w *writer) {
	w.WriteString("for ")
	if this.Loop.Init != nil {
		this.Loop.Init.emitTo(w)
	}
	w.WriteByte(';')
	if this.Loop.Cond != nil {
		this.Loop.Cond.emitTo(w)
	}
	w.WriteByte(';')
	if this.Loop.Each != nil {
		this.Loop.Each.emitTo(w)
	}
	this.emit(w, true, "", false)
}

func (this Op) emit(w *writer, operator string) {
	last, unary := len(this.Operands), len(this.Operands) == 1
	parens, canparens := false, operator != "=" && operator != ":="
	for i := range this.Operands {
		if i > 0 || unary {
			w.WriteString(operator)
		}
		if canparens {
			_, parens = this.Operands[i].(interface{ isOp() })
			if (!parens) && i == last && operator == "." {
				if _, parens = this.Operands[i].(*TypeRef); !parens {
					name, _ := this.Operands[i].(Named)
					parens = name.Name == "type"
				}
			}
		}
		if parens {
			w.WriteByte('(')
		}
		this.Operands[i].emitTo(w)
		if parens {
			w.WriteByte(')')
		}
	}
}

func (Op) isOp() {}

func (this OpSet) emitTo(w *writer) { this.Op.emit(w, "=") }

func (this OpDecl) emitTo(w *writer) { this.Op.emit(w, ":=") }

func (this OpComma) emitTo(w *writer) { this.Op.emit(w, ",") }

func (this OpDot) emitTo(w *writer) {
	if pref := PkgImportNamePrefix; len(this.Operands) > 1 {
		if n, ok := this.Operands[0].(Named); ok && len(n.Name) > len(pref) && (len(pref) == 0 || n.Name[:len(pref)] == pref) {
			w.pkgImportsActuallyEmitted[n.Name] = true
		}
		this.Op.emit(w, ".")
	}
}

func (this OpAnd) emitTo(w *writer) { this.Op.emit(w, " && ") }

func (this OpOr) emitTo(w *writer) { this.Op.emit(w, " || ") }

func (this OpEq) emitTo(w *writer) { this.Op.emit(w, " == ") }

func (this OpNeq) emitTo(w *writer) { this.Op.emit(w, " != ") }

func (this OpGeq) emitTo(w *writer) { this.Op.emit(w, " >= ") }

func (this OpLeq) emitTo(w *writer) { this.Op.emit(w, " <= ") }

func (this OpGt) emitTo(w *writer) { this.Op.emit(w, " > ") }

func (this OpLt) emitTo(w *writer) { this.Op.emit(w, " < ") }

func (this OpAdd) emitTo(w *writer) { this.Op.emit(w, "+") }

func (this OpSub) emitTo(w *writer) { this.Op.emit(w, "-") }

func (this OpMul) emitTo(w *writer) { this.Op.emit(w, "*") }

func (this OpDiv) emitTo(w *writer) { this.Op.emit(w, "/") }

func (this OpAddr) emitTo(w *writer) { this.Op.emit(w, "&") }

func (this OpDeref) emitTo(w *writer) { this.Op.emit(w, "*") }

func (this OpNot) emitTo(w *writer) { this.Op.emit(w, "!") }

func (this OpIdx) emitTo(w *writer) {
	for i := range this.Operands {
		if i > 0 {
			w.WriteByte('[')
		}
		this.Operands[i].emitTo(w)
		if i > 0 {
			w.WriteByte(']')
		}
	}
}

func (this ExprLit) emitTo(w *writer) {
	w.WriteString(fmt.Sprintf("%#v", this.Val))
}

func (ExprNil) emitTo(w *writer) {
	w.WriteString("nil")
}

func (this *ExprCall) emitTo(w *writer) {
	_, calleeistyperef := this.Callee.(*TypeRef)
	if calleeistyperef {
		w.WriteByte('(')
	}
	this.Callee.emitTo(w)
	if calleeistyperef {
		w.WriteByte(')')
	}
	w.WriteByte('(')
	for i := range this.Args {
		this.Args[i].emitTo(w)
		w.WriteByte(',')
	}
	w.WriteByte(')')
}

// CodeGen generates the code via `this.CodeGenPlain()`, and then optionally `go/format`s it.
// Any `error` returned is from `go/format`, and if so, `src` will instead contain the original
// (non-formatted) generated code that was given to `go/format` to aid investigating the issue.
func (this *SourceFile) CodeGen(codeGenCommentNotice string, pkgImportPathsToNames PkgImports, emitNoOpFuncBodies bool, goFmt bool) (src []byte, goFmtTimeTaken time.Duration, goFmtErr error) {
	orig := this.CodeGenPlain(codeGenCommentNotice, pkgImportPathsToNames, emitNoOpFuncBodies)
	if !goFmt {
		src = orig
	} else {
		timestarted := time.Now()
		src, goFmtErr = format.Source(orig)
		if goFmtTimeTaken = time.Since(timestarted); goFmtErr != nil {
			src = orig
		}
	}
	return
}

// CodeGenPlain generates the code represented by `this` into `src`, without `go/format`ting it.
func (this *SourceFile) CodeGenPlain(codeGenCommentNotice string, pkgImportPathsToNames PkgImports, emitNoOpFuncBodies bool) []byte {
	wdecls := writer{emitNoOpFuncBodies: emitNoOpFuncBodies, pkgImportsRegistered: pkgImportPathsToNames, pkgImportsActuallyEmitted: map[string]bool{}}
	this.SynBlock.emit(&wdecls, false, "\n\n", false)

	var wmain writer
	wmain.WriteString("package ")
	wmain.WriteString(this.PkgName)
	if len(codeGenCommentNotice) > 0 {
		wmain.WriteString("\n\n// ")
		wmain.WriteString(codeGenCommentNotice)
	}
	wmain.WriteString("\n\n")

	pkgimports := pkgImportPathsToNames
	if emitNoOpFuncBodies {
		pkgimports = make(PkgImports, len(wdecls.pkgImportsActuallyEmitted))
		for pkgpath, pkgname := range pkgImportPathsToNames {
			if wdecls.pkgImportsActuallyEmitted[pkgname] {
				pkgimports[pkgpath] = pkgname
			}
		}
	}
	if len(pkgimports) > 0 {
		wmain.WriteString("import (")
		for pkgpath, pkgname := range pkgimports {
			wmain.WriteString(pkgname)
			wmain.WriteString(" \"")
			wmain.WriteString(pkgpath)
			wmain.WriteString("\";")
		}
		wmain.WriteString(")\n\n")
	}
	wmain.Write(wdecls.Bytes())
	return wmain.Bytes()
}
