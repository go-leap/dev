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
	pkgImportsActuallyEmitted map[PkgName]bool
}

func (me *writer) shouldEmitNoOpFuncBodies() bool { return me.emitNoOpFuncBodies }

func (me Named) emitTo(w *writer) {
	w.WriteString(me.Name)
}

func (me Named) emit(w *writer, keywordPlusSpace string) {
	w.WriteString(keywordPlusSpace)
	w.WriteString(me.Name)
}

func (me NamedTyped) emit(w *writer, noFuncKeywordBecauseInterfaceMethod bool, spread bool) {
	if me.Name != "" {
		w.WriteString(me.Name)
		w.WriteByte(' ')
	}
	if spread {
		w.WriteString("...")
	}
	me.Type.emit(w, noFuncKeywordBecauseInterfaceMethod)
}

func (me NamedsTypeds) emit(w *writer, sep byte, noFuncKeywordBecauseInterfaceMethods bool, lastSpreads bool) {
	for i := range me {
		if i > 0 {
			w.WriteByte(sep)
		}
		me[i].emit(w, noFuncKeywordBecauseInterfaceMethods, lastSpreads && i == len(me)-1)
	}
}

func (me *TypeFunc) emitTo(w *writer) {
	me.emit(w, false)
}

func (me *TypeFunc) emit(w *writer, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod bool) {
	if !noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod {
		w.WriteString("func")
	}
	w.WriteByte('(')
	me.Args.emit(w, ',', false, me.LastArgSpreads)
	w.WriteByte(')')
	if len(me.Rets) == 1 && me.Rets[0].Name == "" {
		w.WriteByte(' ')
		me.Rets[0].Type.emitTo(w)
	} else if len(me.Rets) > 0 {
		w.WriteString(" (")
		me.Rets.emit(w, ',', false, false)
		w.WriteByte(')')
	}
}

func (me *TypeInterface) emitTo(w *writer) {
	w.WriteString("interface{")
	for i := range me.Embeds {
		me.Embeds[i].emitTo(w)
		w.WriteByte(';')
	}
	me.Methods.emit(w, ';', true, false)
	w.WriteByte('}')
}

func (me *TypeStruct) emitTo(w *writer) {
	w.WriteString("struct{")
	for i := range me.Fields {
		me.Fields[i].emitTo(w)
		w.WriteByte(';')
	}
	w.WriteByte('}')
}

func (me *SynStructField) emitTo(w *writer) {
	me.NamedTyped.emit(w, false, false)
	if len(me.Tags) > 0 {
		if unconventional := me.Tags[""]; unconventional != "" && len(me.Tags) == 1 {
			w.WriteString(strconv.Quote(unconventional))
		} else {
			w.WriteByte('`')
			idx, sortednames := 0, make(sort.StringSlice, len(me.Tags))
			for tagname := range me.Tags {
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
					w.WriteString(strconv.Quote(me.Tags[tagname]))
				} else {
					w.WriteString(unconventional)
				}
			}
			w.WriteByte('`')
		}
	}
}

func (me *TypeRef) emitTo(w *writer) {
	me.emit(w, false)
}

func (me *TypeRef) emit(w *writer, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod bool) {
	switch {
	case me.Pointer.Of != nil:
		w.WriteByte('*')
		me.Pointer.Of.emitTo(w)
	case me.ArrOrSlice.Of != nil:
		if me.ArrOrSlice.IsEllipsis {
			w.WriteString("...")
		} else {
			w.WriteByte('[')
			if me.ArrOrSlice.IsFixedLen != nil {
				me.ArrOrSlice.IsFixedLen.emitTo(w)
			}
			w.WriteByte(']')
		}
		me.ArrOrSlice.Of.emitTo(w)
	case me.Map.OfKey != nil:
		w.WriteString("map[")
		me.Map.OfKey.emitTo(w)
		w.WriteByte(']')
		me.Map.ToVal.emitTo(w)
	case me.Named.TypeName != "":
		if me.Named.PkgName != "" {
			w.pkgImportsActuallyEmitted[PkgName(me.Named.PkgName)] = true
			w.WriteString(me.Named.PkgName)
			w.WriteByte('.')
		}
		w.WriteString(me.Named.TypeName)
	case me.Func != nil:
		me.Func.emit(w, noFuncKeywordBecauseSigPartOfFullBodyOrOfInterfaceMethod)
	case me.Interface != nil:
		me.Interface.emitTo(w)
	case me.Struct != nil:
		me.Struct.emitTo(w)
	}
}

func (me *TypeDecl) emitTo(w *writer) {
	me.Docs.emit(w, false)
	w.WriteString("type ")
	if w.WriteString(me.Name); me.IsAlias {
		w.WriteByte('=')
	}
	w.WriteByte(' ')
	me.Type.emitTo(w)
}

func (me Syns) emitTo(w *writer) {
	if len(me) == 1 {
		me[0].emitTo(w)
	} else {
		for i := range me {
			me[i].emitTo(w)
			w.WriteByte(';')
		}
	}
}

func (me SynBlock) emitTo(w *writer) {
	me.emit(w, true, ';', false)
	w.WriteByte(';')
}

func (me SynBlock) emit(w *writer, wrapInCurlyBraces bool, sep byte, addFinalRet bool) {
	if wrapInCurlyBraces {
		w.WriteByte('{')
	}
	for i := range me.Body {
		me.Body[i].emitTo(w)
		w.WriteByte(sep)
	}
	if addFinalRet {
		K.Return.emitTo(w)
	}
	if wrapInCurlyBraces {
		w.WriteByte('}')
	}
}

func (me SingleLineDocCommentParagraphs) emit(w *writer, isPkgDoc bool) {
	if len(me) > 0 {
		if !isPkgDoc {
			w.WriteString("\n\n")
		}
		for i, doccommentpara := range me {
			if i > 0 {
				w.WriteString("// \n")
			}
			w.WriteString("// ")
			w.WriteString(doccommentpara)
			w.WriteByte('\n')
		}
	}
}

func (me *SynFunc) emitTo(w *writer) {
	doc, noop, hasfinalret, hasnamedrets := me.Docs, w.shouldEmitNoOpFuncBodies(), false, me.Type.Func.Rets.AllNamed()
	if len(me.Type.Func.Rets) > 0 && len(me.Body) > 0 {
		if _, hasfinalret = me.Body[len(me.Body)-1].(*StmtRet); !hasfinalret {
			_, hasfinalret = me.Body[len(me.Body)-1].(StmtRet)
		}
	}
	if noop = noop && (len(me.Type.Func.Rets) == 0 || hasnamedrets); noop {
		doc = append(doc, "As per your current (and presumably temporary) go-gent code-gen settings, this method is effectively a no-op (so each of its return values will always equal its type's zero-value).")
	}
	doc.emit(w, false)

	oldimps := w.pkgImportsActuallyEmitted
	if me.EmitCommented {
		w.pkgImportsActuallyEmitted = map[PkgName]bool{}
		w.WriteString("/*\n")
	}
	if w.WriteString("func "); me.Recv.Type != nil {
		w.WriteByte('(')
		w.WriteString(me.Recv.Name)
		w.WriteByte(' ')
		me.Recv.Type.emitTo(w)
		w.WriteByte(')')
	}
	w.WriteString(me.Named.Name)
	if me.Type.emit(w, true); !noop {
		me.SynBlock.emit(w, true, ';', hasnamedrets && !hasfinalret)
	} else {
		w.WriteByte('{')
		K.Return.emitTo(w)
		w.WriteByte('}')
	}
	if me.Name != "" {
		w.WriteByte('\n')
	}
	if me.EmitCommented {
		w.WriteString("*/\n")
		w.pkgImportsActuallyEmitted = oldimps
	}
}

func (me StmtBreak) emitTo(w *writer) {
	Named(me).emit(w, "break ")
}

func (me StmtContinue) emitTo(w *writer) {
	Named(me).emit(w, "continue ")
}

func (me StmtGoTo) emitTo(w *writer) {
	Named(me).emit(w, "goto ")
}

func (me StmtUnary) emit(w *writer, keywordPlusSpace string) {
	if w.WriteString(keywordPlusSpace); me.Expr != nil {
		me.Expr.emitTo(w)
	}
}

func (me StmtRet) emitTo(w *writer) {
	me.StmtUnary.emit(w, "return ")
}

func (me StmtDefer) emitTo(w *writer) {
	me.StmtUnary.emit(w, "defer ")
}

func (me StmtGo) emitTo(w *writer) {
	me.StmtUnary.emit(w, "go ")
}

func (me *StmtConst) emitTo(w *writer) {
	w.WriteString("const ")
	w.WriteString(me.Name)
	if w.WriteByte(' '); me.Type != nil {
		me.Type.emitTo(w)
	}
	w.WriteByte('=')
	me.Expr.emitTo(w)
}

func (me *StmtVar) emitTo(w *writer) {
	w.WriteString("var ")
	w.WriteString(me.Name)
	if w.WriteByte(' '); me.Type != nil {
		me.Type.emitTo(w)
	}
	if me.Expr != nil {
		w.WriteByte('=')
		me.Expr.emitTo(w)
	}
}

func (me *StmtIf) emitTo(w *writer) {
	if me == nil || len(me.IfThens) == 0 || me.IfThens[0].Cond == nil {
		return
	}
	finali, finalelse := len(me.IfThens)-1, len(me.Else.Body) > 0
	for i := range me.IfThens {
		w.WriteString("if ")
		me.IfThens[i].Cond.emitTo(w)
		me.IfThens[i].SynBlock.emit(w, true, ';', false)
		if i != finali || finalelse {
			w.WriteString(" else ")
		}
	}
	if finalelse {
		me.Else.emit(w, true, ';', false)
	}
}

func (me *SynCase) emitTo(w *writer) {
	w.WriteString("case ")
	me.Cond.emitTo(w)
	w.WriteByte(':')
	me.SynBlock.emit(w, false, ';', false)
}

func (me *StmtSwitch) emitTo(w *writer) {
	w.WriteString("switch ")
	if me.Scrutinee != nil {
		me.Scrutinee.emitTo(w)
	}
	w.WriteByte('{')
	for i := range me.Cases {
		me.Cases[i].emitTo(w)
	}
	if len(me.Default.Body) > 0 {
		w.WriteString("default: ")
		me.Default.emit(w, false, ';', false)
	}
	w.WriteByte('}')
}

func (me *StmtFor) emitTo(w *writer) {
	if me.Range.Over != nil {
		me.emitRange(w)
	} else {
		me.emitLoop(w)
	}
}

func (me *StmtFor) emitRange(w *writer) {
	w.WriteString("for ")
	if me.Range.Key.Name != "" || me.Range.Val.Name != "" {
		if me.Range.Key.Name == "" {
			w.WriteByte('_')
		} else {
			me.Range.Key.emitTo(w)
		}
		if me.Range.Val.Name != "" {
			w.WriteByte(',')
			me.Range.Val.emitTo(w)
		}
		w.WriteString(" := ")
	}
	w.WriteString("range ")
	me.Range.Over.emitTo(w)
	me.emit(w, true, ';', false)
}
func (me *StmtFor) emitLoop(w *writer) {
	w.WriteString("for ")
	if me.Loop.Init != nil {
		me.Loop.Init.emitTo(w)
	}
	w.WriteByte(';')
	if me.Loop.Cond != nil {
		me.Loop.Cond.emitTo(w)
	}
	w.WriteByte(';')
	if me.Loop.Step != nil {
		me.Loop.Step.emitTo(w)
	}
	me.emit(w, true, ';', false)
}

func (me Op) emit(w *writer, operator string) {
	last, unary, canparens, andor :=
		len(me.Operands), len(me.Operands) == 1, (operator != "=" && operator != ":="), (operator == " && " || operator == " || ")
	parens := canparens && andor

	if andor {
		w.WriteByte('(')
	}
	for i := range me.Operands {
		if i > 0 || unary {
			w.WriteString(operator)
		}
		if me.Operands[i] != nil {
			if canparens {
				_, parens = me.Operands[i].(interface{ isOp() })
				if parens {
					switch me.Operands[i].(type) {
					case OpIdx, OpDot:
						parens = false
					}
				}
				if (!parens) && operator == "." {
					if _, parens = me.Operands[i].(*TypeRef); (!parens) && i == last {
						name, _ := me.Operands[i].(Named)
						parens = name.Name == "type"
					}
				}
			}
			if parens {
				w.WriteByte('(')
			}
			me.Operands[i].emitTo(w)
			if parens {
				w.WriteByte(')')
			}
		}
	}
	if andor {
		w.WriteByte(')')
	}
}

func (Op) isOp() {}

func (me OpSet) emitTo(w *writer) {
	if len(me.Operands) == 2 { // goodie: turn `foo=foo-1` into `foo--` ...
		if lname, lnok := me.Operands[0].(Named); lnok { // ... and same for `+`
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
			if rsub, rsok := me.Operands[1].(OpSub); rsok && len(rsub.Operands) == 2 && try(rsub.Operands, "--") {
				return
			} else if radd, raok := me.Operands[1].(OpAdd); raok && len(radd.Operands) == 2 && try(radd.Operands, "++") {
				return
			}
		}
	}
	me.Op.emit(w, "=")
}

func (me OpDecl) emitTo(w *writer) { me.Op.emit(w, ":=") }

func (me OpComma) emitTo(w *writer) {
	if len(me.Operands) == 1 {
		me.Operands[0].emitTo(w)
	} else {
		me.Op.emit(w, ",")
	}
}

func (me OpColon) emitTo(w *writer) { me.Op.emit(w, ":") }

func (me OpDot) emitTo(w *writer) {
	if pref := PkgImportNamePrefix; len(me.Operands) > 1 {
		if n, ok := me.Operands[0].(Named); ok && len(n.Name) > len(pref) && (len(pref) == 0 || n.Name[:len(pref)] == string(pref)) {
			w.pkgImportsActuallyEmitted[PkgName(n.Name)] = true
		}
		me.Op.emit(w, ".")
	}
}

func (me OpAnd) emitTo(w *writer) { me.Op.emit(w, " && ") }

func (me OpOr) emitTo(w *writer) { me.Op.emit(w, " || ") }

func (me OpEq) emitTo(w *writer) { me.Op.emit(w, " == ") }

func (me OpNeq) emitTo(w *writer) { me.Op.emit(w, " != ") }

func (me OpGeq) emitTo(w *writer) { me.Op.emit(w, " >= ") }

func (me OpLeq) emitTo(w *writer) { me.Op.emit(w, " <= ") }

func (me OpGt) emitTo(w *writer) { me.Op.emit(w, " > ") }

func (me OpLt) emitTo(w *writer) { me.Op.emit(w, " < ") }

func (me OpAdd) emitTo(w *writer) { me.Op.emit(w, "+") }

func (me OpSub) emitTo(w *writer) { me.Op.emit(w, "-") }

func (me OpMul) emitTo(w *writer) { me.Op.emit(w, "*") }

func (me OpDiv) emitTo(w *writer) { me.Op.emit(w, "/") }

func (me OpMod) emitTo(w *writer) { me.Op.emit(w, "%") }

func (me OpAddr) emitTo(w *writer) { me.Op.emit(w, "&") }

func (me OpDeref) emitTo(w *writer) { me.Op.emit(w, "*") }

func (me OpNot) emitTo(w *writer) { me.Op.emit(w, "!") }

func (me OpIdx) emitTo(w *writer) {
	for i := range me.Operands {
		if i > 0 {
			w.WriteByte('[')
		}
		me.Operands[i].emitTo(w)
		if i > 0 {
			w.WriteByte(']')
		}
	}
}

func (me ExprLit) emitTo(w *writer) {
	me.emit(w, me.Val)
}

func (me ExprLit) emit(w *writer, val IAny) {
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
			me.emit(w, v[i])
			w.WriteByte(',')
		}
		w.WriteByte('}')
	default:
		fmt.Fprintf(w, "%#v", v)
	}
}

func (me *ExprCall) emitTo(w *writer) {
	_, calleeistyperef := me.Callee.(*TypeRef)
	if calleeistyperef {
		w.WriteByte('(')
	}
	me.Callee.emitTo(w)
	if calleeistyperef {
		w.WriteByte(')')
	}
	w.WriteByte('(')
	for i := range me.Args {
		if me.Args[i].emitTo(w); i == len(me.Args)-1 && me.LastArgSpreads {
			w.WriteString("...")
		} else {
			w.WriteByte(',')
		}
	}
	w.WriteByte(')')
}

func (me *StmtLabel) emitTo(w *writer) {
	me.Named.emitTo(w)
	w.WriteByte(':')
	me.SynBlock.emit(w, false, ';', false)
}

func (me *SynRaw) emitTo(w *writer) {
	for pkgname, pkgpath := range me.ImportsUsed {
		if pkgimpname := w.pkgImportsRegistered[pkgpath]; pkgimpname == "" {
			w.pkgImportsRegistered[pkgpath] = pkgname
		} else if pkgimpname != pkgname {
			panic("SynRaw package-imports conflict: uses package import '" + pkgpath + "' named '" + string(pkgname) + "' but current output file has it registered with name '" + string(pkgimpname) + "'")
		}
		if !me.EmitCommented {
			w.pkgImportsActuallyEmitted[pkgname] = true
		} else {
			w.WriteString(" /*")
			w.WriteString(string(pkgname))
			w.WriteString(strconv.Quote(pkgpath))
			w.WriteString("*/ ")
		}
	}
	if me.EmitCommented {
		w.WriteString("/*")
	}
	if w.Write(me.Src); me.EmitCommented {
		w.WriteString("*/")
	}
}

// CodeGen generates the code via `me.CodeGenPlain()`, and then optionally `go/format`s it.
// Any `error` returned is from `go/format`, and if so, `src` will instead contain the original
// (non-formatted) generated code that was given to `go/format` to aid investigating the issue.
func (me *SourceFile) CodeGen(codeGenCommentNotice string, pkgImportPathsToNames PkgImports, emitNoOpFuncBodies bool, goFmt bool) (src []byte, goFmtTimeTaken time.Duration, goFmtErr error) {
	if orig := me.CodeGenPlain(codeGenCommentNotice, pkgImportPathsToNames, emitNoOpFuncBodies); !goFmt {
		src = orig
	} else {
		timegofmtstart := time.Now()
		src, goFmtErr = format.Source(orig)
		if goFmtTimeTaken = time.Since(timegofmtstart); goFmtErr != nil {
			src = orig
		}
	}
	return
}

// CodeGenPlain generates the code represented by `me` into `src`, without `go/format`ting it.
func (me *SourceFile) CodeGenPlain(codeGenCommentNotice string, pkgImportPathsToNames PkgImports, emitNoOpFuncBodies bool) []byte {
	wdecls := writer{emitNoOpFuncBodies: emitNoOpFuncBodies, pkgImportsRegistered: pkgImportPathsToNames, pkgImportsActuallyEmitted: make(map[PkgName]bool, len(pkgImportPathsToNames))}
	me.SynBlock.emit(&wdecls, false, '\n', false)

	var wmain writer
	me.DocComments.emit(&wmain, true)
	wmain.WriteString("package ")
	wmain.WriteString(me.PkgName)
	if len(codeGenCommentNotice) > 0 {
		wmain.WriteString("\n\n// ")
		wmain.WriteString(codeGenCommentNotice)
	}
	wmain.WriteString("\n\n")

	pkgimports := make(PkgImports, len(wdecls.pkgImportsActuallyEmitted))
	for pkgpath, pkgname := range pkgImportPathsToNames {
		if wdecls.pkgImportsActuallyEmitted[pkgname] {
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
