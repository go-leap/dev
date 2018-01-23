package udevhs

import (
	"encoding/json"
	"strings"

	"github.com/go-leap/dev"
	"github.com/go-leap/run"
)

type hlint struct {
	Module       string   `json:"module,omitempty"`
	Decl         string   `json:"decl,omitempty"`
	Severity     string   `json:"severity,omitempty"`
	Hint         string   `json:"hint,omitempty"`
	File         string   `json:"file,omitempty"`
	StartLine    int      `json:"startLine,omitempty"`
	StartColumn  int      `json:"startColumn,omitempty"`
	EndLine      int      `json:"endLine,omitempty"`
	EndColumn    int      `json:"endColumn,omitempty"`
	From         string   `json:"from,omitempty"`
	To           string   `json:"to,omitempty"`
	Note         []string `json:"note,omitempty"`
	Refactorings string   `json:"refactorings,omitempty"`
}

var (
	// HlintIgnore contains lints to be ignored, passed via `--ignore` args to hlint during `LintHlint`.
	HlintIgnore = []string{
		// "Use module export list",
		// "Redundant bracket",
		"Use infix",
		"Use camelCase",
		"Use String", //	when we do use [Char] instead of String, it's for a reason
	}
)

// LintHlint runs the `hlint` program for the specified `filePaths`.
// The returned `SrcMsgs` will have at least `Msg`, `Ref` and all 4 `Pos` fields set.
// The `SrcMsg.Misc` field might be set (to Module or Decl or Module.Decl) or not.
// If an hlint offers an edit suggestion, it is stored in `SrcMsg.Data` under the `From`, `To` and `Note` keys.
func LintHlint(filePaths []string) (msgs udev.SrcMsgs) {
	cmdargs := []string{"--color=never", "--json", "-j", "--cross", "--no-exit-code", "-XHaskell2010"}
	for _, ign := range HlintIgnore {
		cmdargs = append(cmdargs, "--ignore", ign)
	}
	cmdargs = append(cmdargs, filePaths...)
	jsonoutput, _, _ := urun.CmdExec("hlint", cmdargs...)
	if jsonoutput = strings.TrimSpace(jsonoutput); jsonoutput != "" {
		jsonoutput = strings.Replace(strings.Replace(jsonoutput, "\n", "", -1), "\r", "", -1)
		var hlints []hlint
		if err := json.Unmarshal([]byte(jsonoutput), &hlints); err != nil {
			msgs = append(msgs, &udev.SrcMsg{Msg: "Problematic HlintJSON: " + err.Error() + " ➜ " + jsonoutput, Ref: filePaths[0], Pos1Ln: 1, Pos1Ch: 1})
		} else {
			for _, hl := range hlints {
				if hl.Severity != "Error" {
					msg := &udev.SrcMsg{Msg: hl.Hint, Ref: hl.File, Pos1Ln: hl.StartLine, Pos1Ch: hl.StartColumn, Pos2Ln: hl.EndLine, Pos2Ch: hl.EndColumn}
					if hl.From != "" && hl.To != "" {
						msg.Data = map[string]interface{}{"From": hl.From, "To": hl.To, "Note": hl.Note}
					}
					if md := strings.Trim(hl.Module+"."+hl.Decl, "."); md != "" {
						msg.Misc = md
					}
					msgs = append(msgs, msg)
				}
			}
		}
	}
	return
}
