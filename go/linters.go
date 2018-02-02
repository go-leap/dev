package udevgo

import (
	"github.com/go-leap/dev"
	"github.com/go-leap/str"
)

var (
	GolintIgnoreSubstrings = []string{
		" should have comment ",
		// "if block ends with a return statement, so drop this else and outdent its block",
		"ALL_CAPS",
		"underscore",
		"CamelCase",
		"should not use dot imports",
		"it will be inferred from the right-hand side",
		"should be of the form \"",
		"error strings should",
		// "should omit 2nd value from range; this loop is equivalent to ",
		// "don't use generic names",
	}
)

func LintCheck(cmdname string, pkgimppath string) (msgs udev.SrcMsgs) {
	perline := func(ln string) string {
		if ustr.Pref(ln, pkgimppath+": ") {
			return ln[len(pkgimppath)+2:]
		}
		return ""
	}
	for _, srcref := range udev.CmdExecOnSrc(false, perline, cmdname, pkgimppath) {
		if ustr.Pref(srcref.Msg, pkgimppath+".") {
			srcref.Msg = srcref.Msg[len(pkgimppath)+1:]
		}
		if cmdname == "varcheck" || cmdname == "structcheck" {
			srcref.Msg = "unused & unexported: " + srcref.Msg
		}
		msgs = append(msgs, srcref)
	}
	return
}

func LintIneffAssign(dirrelpath string) (msgs udev.SrcMsgs) {
	msgs = udev.CmdExecOnSrc(false, nil, "ineffassign", "-n", dirrelpath)
	return
}

func LintViaPkgImpPath(cmdname string, pkgimppath string, inclstderr bool) (msgs udev.SrcMsgs) {
	msgs = udev.CmdExecOnSrc(inclstderr, nil, cmdname, pkgimppath)
	return
}

func LintMvDan(cmdname string, pkgimppath string) udev.SrcMsgs {
	cmdargs := []string{pkgimppath}
	if cmdname == "unindent" {
		cmdargs = []string{"-exp.r", "3.01", pkgimppath}
	} else if cmdname == "unparam" {
		cmdargs = []string{"-exported", "-tests", "true", pkgimppath}
	}
	return udev.CmdExecOnSrc(false, nil, cmdname, cmdargs...)
}

func LintHonnef(cmdname string, pkgimppath string) (msgs udev.SrcMsgs) {
	msgs = udev.CmdExecOnSrc(false, nil, cmdname, "-go", GoVersionShort, pkgimppath)
	return
}

func LintGoConst(dirpath string) (msgs udev.SrcMsgs) {
	msgs = udev.CmdExecOnSrc(false, nil, "goconst", "-match-constant", dirpath)
	return
}

func LintGoSimple(pkgimppath string) (msgs udev.SrcMsgs) {
	msgs = udev.CmdExecOnSrc(false, nil, "gosimple", "-go", GoVersionShort, pkgimppath)
	return
}

func LintErrcheck(pkgimppath string) (msgs udev.SrcMsgs) {
	for _, m := range udev.CmdExecOnSrc(false, nil, "errcheck", "-abspath", "-asserts", "-blank", "-ignoretests", "false", pkgimppath) {
		m.Msg = "Ignores a returned `error`: " + m.Msg
		msgs = append(msgs, m)
	}
	return
}

func LintGolint(pkgimppathordirpath string) (msgs udev.SrcMsgs) {
	for _, msg := range udev.CmdExecOnSrc(false, nil, "golint", pkgimppathordirpath) {
		if !lintGolintCensored(msg.Msg) {
			msgs = append(msgs, msg)
		}
	}
	return
}

func lintGolintCensored(msg string) bool {
	for _, s := range GolintIgnoreSubstrings {
		if ustr.Has(msg, s) {
			return true
		}
	}
	words := ustr.Split(msg, " ") // the likes of: "... nameFoo should be nameFOO"
	l := len(words)
	return l >= 5 && words[l-3] == "should" && words[l-2] == "be" && ustr.Eq(words[l-1], words[l-4])
}

func LintGoVet(pkgimppath string) udev.SrcMsgs {
	perline := func(ln string) string {
		if ustr.Pref(ln, "vet: ") || ustr.Pref(ln, "exit status ") {
			return ""
		}
		return ln
	}
	return udev.CmdExecOnSrc(true, perline, "go", "vet", "-shadow=true", "-shadowstrict", "-all", pkgimppath)
}
