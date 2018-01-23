package udevgo

import (
	"errors"

	"github.com/go-leap/dev"
	"github.com/go-leap/fs"
	"github.com/go-leap/run"
	"github.com/go-leap/str"
)

func Gorename(cmdName string, filePath string, offset int, newName string, eol string) (fileEdits udev.SrcMsgs, err error) {
	cmdargs := []string{"-d", "-to", newName, "-offset", ustr.Fmt("%s:#%d", filePath, offset)}
	var renout, renerr string
	if len(cmdName) == 0 {
		cmdName = "gorename"
	}
	if renout, renerr, err = urun.CmdExec(cmdName, cmdargs...); err != nil {
		return
	} else if renout = ustr.Trim(renout); renerr != "" && renout == "" {
		if join, re, msgs := " — ", "", udev.SrcMsgsFromLns(ustr.Split(renerr, "\n")); len(msgs) > 0 {
			for _, m := range msgs {
				if m.Ref != "" && ufs.IsFile(m.Ref) {
					re += m.Msg + join
				}
			}
			if re != "" {
				renerr = re[:len(re)-len(join)]
			}
		}
		err = errors.New(renerr)
		return
	}
	i := ustr.Pos(renout, "--- ")
	if i < 0 {
		err = errors.New(renout)
		return
	}
	renout = renout[i+4:]
	rendiffs := ustr.Map(ustr.Split(renout, "--- "), ustr.Trim)
	if len(rendiffs) == 0 {
		return nil, errors.New("Renaming aborted: no diffs could be obtained.")
	}

	for _, rendiff := range rendiffs {
		if i = ustr.Pos(rendiff, "\t"); i <= 0 {
			return nil, errors.New("Renaming aborted: could not detect file path in diffs.")
		}
		ffp := rendiff[:i]
		if !ufs.IsFile(ffp) {
			return nil, errors.New("Renaming aborted: bad absolute file path `" + ffp + "` in diffs.")
		} else if i = ustr.Pos(rendiff, "@@ -"); i <= 0 {
			return nil, errors.New("Renaming aborted: `@@ -` expected.")
		}
		for _, hunkchunk := range ustr.Split(rendiff[i+4:], "@@ -") {
			lns := ustr.Split(hunkchunk, "\n")
			if len(lns) == 0 {
				return nil, errors.New("Renaming aborted: expected something between one `@@ -` and the next.")
			}
			i = ustr.Pos(lns[0], ",")
			lb := int(ustr.ToInt(lns[0][:i], 0))
			s := lns[0][i+1:]
			ll := int(ustr.ToInt(s[:ustr.Pos(s, " +")], 0))
			if lb == 0 || ll == 0 {
				return nil, errors.New("Renaming aborted: diffs contained invalid or unparsable line hints.")
			}
			fed := &udev.SrcMsg{Ref: ffp, Pos1Ln: lb - 1, Pos1Ch: 0, Pos2Ln: lb - 1 + ll, Pos2Ch: 0}
			for _, ln := range lns[1:] {
				if ustr.Pref(ln, " ") || ustr.Pref(ln, "+") {
					fed.Msg = fed.Msg + ln[1:] + eol
				}
			}
			fileEdits = append(fileEdits, fed)
		}
		if len(fileEdits) == 0 {
			err = errors.New("Renaming aborted: a diff without effective edits.")
		}
	}
	return
}
