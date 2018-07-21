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

func (this Named) emit(w *writer, keywordPlusSpace string) {
	w.WriteString(keywordPlusSpace)
	w.WriteString(this.Name)
}

func (this NamedTyped) emit(w *writer, noFuncKeywordBecauseInterfaceMethod bool, spread bool) {
	if this.Name != "" {
		w.WriteString(this.Name)
		w.WriteByte(' ')
	}
	if spread {
		w.WriteString("...")
	}
	this.Type.emit(w, noFuncKeywordBecauseInterfaceMethod)
}

func (this NamedsTypeds) emit(w *writer, sep byte, noFuncKeywordBecauseInterfaceMethods bool, lastSpreads bool) {
	for i := range this {
		if i > 0 {
			w.WriteByte(sep)
		}
		this[i].emit(w, noFuncKeywordBecauseInterfaceMethods, lastSpreads && i == len(this)-1)
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
	this.Args.emit(w, ',', false, this.LastArgSpreads)
	w.WriteByte(')')
	if len(this.Rets) == 1 && this.Rets[0].Name == "" {
		w.WriteByte(' ')
		this.Rets[0].Type.emitTo(w)
	} else if len(this.Rets) > 0 {
		w.WriteString(" (")
		this.Rets.emit(w, ',', false, false)
		w.WriteByte(')')
	}
}

func (this *TypeInterface) emitTo(w *writer) {
	w.WriteString("interface{")
	for i := range this.Embeds {
		this.Embeds[i].emitTo(w)
		w.WriteByte(';')
	}
	this.Methods.emit(w, ';', true, false)
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
	this.NamedTyped.emit(w, false, false)
	if len(this.Tags) > 0 {
		if unconventional := this.Tags[""]; unconventional != "" && len(this.Tags) == 1 {
			w.WriteString(strconv.Quote(unconventional))
		} else {
			w.WriteByte('`')
			idx, sortednames := 0, make(sort.StringSlice, len(this.Tags))
			for tagname := range this.Tags {
				if tagname != "" {
					idx, sortednames[idx] = idx+1, tagname
				}
			}
			sortednames.Sort()
			for i, tagname := range sortednames {
				if i > 0 {
					w.WriteByte(' ')
				}
				if tagname != "" {
					w.WriteString(tagname)
					w.WriteByte(':')
					w.WriteString(strconv.Quote(this.Tags[tagname]))
				} else {
					w.WriteString(unconventional)
				}
			}
			w.WriteByte('`')
		}
	}
}

func (this *TypeRef) emitTo(w *writer) {
	this.emit(w, false)
}

func (this *TypeRef) emit(w *writer, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod bool) {
	switch {
	case this.Pointer.Of != nil:
		w.WriteByte('*')
		this.Pointer.Of.emitTo(w)
	case this.ArrOrSlice.Of != nil:
		if this.ArrOrSlice.IsEllipsis {
			w.WriteString("...")
		} else {
			w.WriteByte('[')
			if this.ArrOrSlice.IsFixedLen != nil {
				w.WriteString(strconv.FormatUint(*this.ArrOrSlice.IsFixedLen, 10))
			}
			w.WriteByte(']')
		}
		this.ArrOrSlice.Of.emitTo(w)
	case this.Map.OfKey != nil:
		w.WriteString("map[")
		this.Map.OfKey.emitTo(w)
		w.WriteByte(']')
		this.Map.ToVal.emitTo(w)
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

func (this Syns) emitTo(w *writer) {
	if len(this) == 1 {
		this[0].emitTo(w)
	} else {
		for i := range this {
			this[i].emitTo(w)
			w.WriteByte(';')
		}
	}
}

func (this SynBlock) emitTo(w *writer) {
	this.emit(w, true, ';', false)
	w.WriteByte(';')
}

func (this SynBlock) emit(w *writer, wrapInCurlyBraces bool, sep byte, addFinalRet bool) {
	if wrapInCurlyBraces {
		w.WriteByte('{')
	}
	for i := range this.Body {
		this.Body[i].emitTo(w)
		w.WriteByte(sep)
	}
	if addFinalRet {
		K.Return.emitTo(w)
	}
	if wrapInCurlyBraces {
		w.WriteByte('}')
	}
}

func (this *SynFunc) emitTo(w *writer) {
	doc, noop, hasfinalret, hasnamedrets := this.Docs, w.shouldEmitNoOpFuncBodies(), false, this.Type.Func.Rets.AllNamed()
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

	oldimps := w.pkgImportsActuallyEmitted
	if this.EmitCommented {
		w.pkgImportsActuallyEmitted = map[string]bool{}
		w.WriteString("/*\n")
	}
	if w.WriteString("func "); this.Recv.Type != nil {
		w.WriteByte('(')
		w.WriteString(this.Recv.Name)
		w.WriteByte(' ')
		this.Recv.Type.emitTo(w)
		w.WriteByte(')')
	}
	w.WriteString(this.Named.Name)
	if this.Type.emit(w, true); !noop {
		this.SynBlock.emit(w, true, ';', hasnamedrets && !hasfinalret)
	} else {
		w.WriteByte('{')
		K.Return.emitTo(w)
		w.WriteByte('}')
	}
	if this.Name != "" {
		w.WriteByte('\n')
	}
	if this.EmitCommented {
		w.WriteString("*/\n")
		w.pkgImportsActuallyEmitted = oldimps
	}
}

func (this StmtBreak) emitTo(w *writer) {
	Named(this).emit(w, "break ")
}

func (this StmtContinue) emitTo(w *writer) {
	Named(this).emit(w, "continue ")
}

func (this StmtGoTo) emitTo(w *writer) {
	Named(this).emit(w, "goto ")
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
	if this == nil || len(this.IfThens) == 0 || this.IfThens[0].Cond == nil {
		return
	}
	finali, finalelse := len(this.IfThens)-1, len(this.Else.Body) > 0
	for i := range this.IfThens {
		w.WriteString("if ")
		this.IfThens[i].Cond.emitTo(w)
		this.IfThens[i].SynBlock.emit(w, true, ';', false)
		if i != finali || finalelse {
			w.WriteString(" else ")
		}
	}
	if finalelse {
		this.Else.emit(w, true, ';', false)
	}
}

func (this *SynCase) emitTo(w *writer) {
	w.WriteString("case ")
	this.Cond.emitTo(w)
	w.WriteByte(':')
	this.SynBlock.emit(w, false, ';', false)
}

func (this *StmtSwitch) emitTo(w *writer) {
	w.WriteString("switch ")
	if this.Scrutinee != nil {
		this.Scrutinee.emitTo(w)
	}
	w.WriteByte('{')
	for i := range this.Cases {
		this.Cases[i].emitTo(w)
	}
	if len(this.Default.Body) > 0 {
		w.WriteString("default: ")
		this.Default.emit(w, false, ';', false)
	}
	w.WriteByte('}')
}

func (this *StmtFor) emitTo(w *writer) {
	if this.Range.Over != nil {
		this.emitRange(w)
	} else {
		this.emitLoop(w)
	}
}

func (this *StmtFor) emitRange(w *writer) {
	w.WriteString("for ")
	if this.Range.Key.Name != "" || this.Range.Val.Name != "" {
		if this.Range.Key.Name == "" {
			w.WriteByte('_')
		} else {
			this.Range.Key.emitTo(w)
		}
		if this.Range.Val.Name != "" {
			w.WriteByte(',')
			this.Range.Val.emitTo(w)
		}
		w.WriteString(" := ")
	}
	w.WriteString("range ")
	this.Range.Over.emitTo(w)
	this.emit(w, true, ';', false)
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
	if this.Loop.Step != nil {
		this.Loop.Step.emitTo(w)
	}
	this.emit(w, true, ';', false)
}

func (this Op) emit(w *writer, operator string) {
	last, unary := len(this.Operands), len(this.Operands) == 1
	parens, canparens := false, operator != "=" && operator != ":="
	for i := range this.Operands {
		if i > 0 || unary {
			w.WriteString(operator)
		}
		if this.Operands[i] != nil {
			if canparens {
				_, parens = this.Operands[i].(interface{ isOp() })
				if parens {
					switch this.Operands[i].(type) {
					case OpIdx:
						parens = false
					}
				}
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
}

func (Op) isOp() {}

func (this OpSet) emitTo(w *writer) {
	if len(this.Operands) == 2 { // goodie: turn `foo=foo-1` into `foo--` ...
		if lname, lnok := this.Operands[0].(Named); lnok { // ... and same for `+`
			try := func(operands Syns, opop string) bool {
				if rname, rnok := operands[0].(Named); rnok && rname.Name == lname.Name {
					if rlit, rlok := operands[1].(ExprLit); rlok && rlit.Val == 1 {
						rname.emitTo(w)
						w.WriteString(opop)
						return true
					}
				}
				return false
			}
			if rsub, rsok := this.Operands[1].(OpSub); rsok && len(rsub.Operands) == 2 && try(rsub.Operands, "--") {
				return
			} else if radd, raok := this.Operands[1].(OpAdd); raok && len(radd.Operands) == 2 && try(radd.Operands, "++") {
				return
			}
		}
	}
	this.Op.emit(w, "=")
}

func (this OpDecl) emitTo(w *writer) { this.Op.emit(w, ":=") }

func (this OpComma) emitTo(w *writer) { this.Op.emit(w, ",") }

func (this OpColon) emitTo(w *writer) { this.Op.emit(w, ":") }

func (this OpDot) emitTo(w *writer) {
	if pref := PkgImportNamePrefix; len(this.Operands) > 1 {
		if n, ok := this.Operands[0].(Named); ok && len(n.Name) > len(pref) && (len(pref) == 0 || n.Name[:len(pref)] == string(pref)) {
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

func (this OpMod) emitTo(w *writer) { this.Op.emit(w, "%") }

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
	this.emit(w, this.Val)
}

func (this ExprLit) emit(w *writer, val IAny) {
	if val == nil {
		w.WriteString("nil")
		return
	}
	switch v := val.(type) {
	case nil:
		w.WriteString("nil")
	case ExprLit:
		v.emitTo(w)
	case Named:
		v.emitTo(w)
	case NamedTyped:
		v.emitTo(w)
	case bool:
		if v {
			w.WriteString("true")
		} else {
			w.WriteString("false")
		}
	case string:
		w.WriteString(strconv.Quote(v))
	case Syns:
		w.WriteString("[]")
		switch first := v[0].(type) {
		case ExprLit:
			fmt.Fprintf(w, "%T", first.Val)
		case NamedTyped:
			first.Type.emitTo(w)
		default:
			fmt.Fprintf(w, "%T", first)
		}
		w.WriteByte('{')
		for i := range v {
			this.emit(w, v[i])
			w.WriteByte(',')
		}
		w.WriteByte('}')
	default:
		fmt.Fprintf(w, "%#v", v)
	}
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
		if this.Args[i].emitTo(w); i == len(this.Args)-1 && this.LastArgSpreads {
			w.WriteString("...")
		} else {
			w.WriteByte(',')
		}
	}
	w.WriteByte(')')
}

func (this *StmtLabel) emitTo(w *writer) {
	this.Named.emitTo(w)
	w.WriteByte(':')
	this.SynBlock.emit(w, false, ';', false)
}

func (this SynRaw) emitTo(w *writer) {
	w.Write(this)
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
	this.SynBlock.emit(&wdecls, false, '\n', false)

	var wmain writer
	wmain.WriteString("package ")
	wmain.WriteString(this.PkgName)
	if len(codeGenCommentNotice) > 0 {
		wmain.WriteString("\n\n// ")
		wmain.WriteString(codeGenCommentNotice)
	}
	wmain.WriteString("\n\n")

	pkgimports := make(PkgImports, len(wdecls.pkgImportsActuallyEmitted))
	for pkgpath, pkgname := range pkgImportPathsToNames {
		if wdecls.pkgImportsActuallyEmitted[string(pkgname)] {
			pkgimports[pkgpath] = pkgname
		}
	}
	if len(pkgimports) > 0 {
		wmain.WriteString("import (")
		for pkgpath, pkgname := range pkgimports {
			wmain.WriteString(string(pkgname))
			wmain.WriteString(" \"")
			wmain.WriteString(pkgpath)
			wmain.WriteString("\";")
		}
		wmain.WriteString(")\n\n")
	}
	wmain.Write(wdecls.Bytes())
	return wmain.Bytes()
}
