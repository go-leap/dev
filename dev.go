package udev

import (
	"errors"
	"os/exec"

	"github.com/go-leap/run"
	"github.com/go-leap/str"
)

// SrcMsgs implements `sort.Interface`.
type SrcMsgs []*SrcMsg

func (me SrcMsgs) Len() int           { return len(me) }
func (me SrcMsgs) Swap(i, j int)      { me[i], me[j] = me[j], me[i] }
func (me SrcMsgs) Less(i, j int) bool { return me[i].Msg < me[j].Msg }

// SrcMsg captures tool-provided information about some source code location.
//
// Meaning and usage/omission of individual fields can vary by tool and/or use-case, and are loosely defined between the producer and consumers of a `SrcMsg`.
type SrcMsg struct {
	Flag   int                    `json:",omitempty"`
	Ref    string                 `json:",omitempty"`
	Msg    string                 `json:",omitempty"`
	Misc   string                 `json:",omitempty"`
	Pos1Ln int                    `json:",omitempty"`
	Pos1Ch int                    `json:",omitempty"`
	Pos2Ln int                    `json:",omitempty"`
	Pos2Ch int                    `json:",omitempty"`
	Data   map[string]interface{} `json:",omitempty"`
}

// SrcMsgFromLn returns the pointer to a newly allocated `SrcMsg` if it can sensibly
// extract `Pos1Ln`, `Pos1Ch`, `Msg` and `Ref` from `line`; otherwise `nil` is returned.
func SrcMsgFromLn(line string) (item *SrcMsg) {
	if lnbits := ustr.Split(line, ":"); len(lnbits) >= 3 {
		if msgpos, fpath := 3, lnbits[0]; len(fpath) > 0 {
			pos1ln := int(ustr.ToInt(lnbits[1], 0))
			pos1ch := int(ustr.ToInt(lnbits[2], 0))
			if pos1ln == 0 {
				pos1ln, msgpos = 1, 1
			} else if pos1ch == 0 {
				msgpos = 2
			}
			if pos1ch == 0 {
				pos1ch = 1
			}
			if msg := ustr.Trim(ustr.Join(lnbits[msgpos:], ":")); len(msg) > 0 || true { // not sure yet, for godef we need to allow "no msg"
				item = &SrcMsg{Msg: msg, Ref: fpath}
				item.Pos1Ln, item.Pos1Ch = pos1ln, pos1ch
			}
		}
	}
	return
}

// SrcMsgsFromLns returns all `SrcMsg` results from calling `SrcMsgFromLn` on all `lines`.
func SrcMsgsFromLns(lines []string) (msgs SrcMsgs) {
	msgs = make(SrcMsgs, 0, len(lines))
	for i := range lines {
		if item := SrcMsgFromLn(lines[i]); item != nil {
			msgs = append(msgs, item)
		} else if l := len(msgs); l > 0 {
			msgs[l-1].Msg += "\n" + lines[i]
		}
	}
	return
}

// CmdExecOnSrc executes the specified command and returns the `SrcMsgsFromLns` of its `CombinedOutput` (if `inclStderr`) or `Output`.
//
// `perLine` may be `nil`, otherwise it is called for each line prior to being ultimately passed to `SrcMsgFromLn`.
func CmdExecOnSrc(inclStderr bool, perLine func(string) string, cmdName string, cmdArgs ...string) SrcMsgs {
	return CmdExecOnSrcIn("", inclStderr, perLine, cmdName, cmdArgs...)
}

// CmdExecOnSrcIn executes the specified command and returns the `SrcMsgsFromLns` of its `CombinedOutput` (if `inclStderr`) or `Output`.
//
// `perLine` may be `nil`, otherwise it is called for each line prior to being ultimately passed to `SrcMsgFromLn`.
func CmdExecOnSrcIn(dir string, inclStderr bool, perLine func(string) string, cmdName string, cmdArgs ...string) SrcMsgs {
	var output []byte
	cmd := exec.Command(cmdName, cmdArgs...)
	cmd.Dir = dir
	if inclStderr {
		output, _ = cmd.CombinedOutput()
	} else {
		output, _ = cmd.Output()
	}
	cmdout := ustr.Trim(string(output))
	msgs := SrcMsgsFromLns(ustr.Map(ustr.Split(cmdout, "\n"), perLine))
	if len(msgs) == 0 && cmdout != "" && dir == "" && inclStderr && perLine == nil {
		msgs = append(msgs, &SrcMsg{Msg: cmdout, Pos1Ch: 1, Pos1Ln: 1})
	}
	return msgs
}

// CmdExecOnStdin executes the specified command and returns the `SrcMsgsFromLns` of its `CombinedOutput` (if `inclStderr`) or `Output`.
//
// `perLine` may be `nil`, otherwise it is called for each line prior to being ultimately passed to `SrcMsgFromLn`.
func CmdExecOnStdin(stdin string, dir string, perLine func(string) string, cmdName string, cmdArgs ...string) (SrcMsgs, error) {
	cmdout, cmderr, err := urun.CmdExecStdin(stdin, dir, cmdName, cmdArgs...)
	if err == nil && len(cmderr) > 0 {
		err = errors.New(cmderr)
	}
	return SrcMsgsFromLns(ustr.Map(ustr.Split(ustr.Trim(cmdout), "\n"), perLine)), err
}
