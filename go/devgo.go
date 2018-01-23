package udevgo

import (
	"os"
	"path/filepath"

	"github.com/go-leap/fs"
	"github.com/go-leap/run"
	"github.com/go-leap/str"
)

var (
	// gopaths is set by `AllGoPaths` or `HasGoDevEnv` to contain all locally declared `GOPATH`s.
	gopaths []string

	// All these are initially set via the first call to `HasGoDevEnv`.
	GoVersion      string
	GoVersionShort string

	Has_godoc        bool
	Has_gofmt        bool
	Has_goimports    bool
	Has_goreturns    bool
	Has_guru         bool
	Has_gorename     bool
	Has_godef        bool
	Has_gogetdoc     bool
	Has_gocode       bool
	Has_structlayout bool
	Has_godocdown    bool

	Has_golint      bool
	Has_checkvar    bool
	Has_checkalign  bool
	Has_checkstruct bool
	Has_errcheck    bool
	Has_ineffassign bool
	Has_interfacer  bool
	Has_unparam     bool
	Has_unindent    bool
	Has_unconvert   bool
	Has_maligned    bool
	Has_goconst     bool
	Has_gosimple    bool
	Has_unused      bool
	Has_staticcheck bool
	Has_deadcode    bool
)

// DirPathToImportPath returns `""` unless `dirPath` is inside a `GOPATH`, in which case the corresponding Go package import path is returned instead.
func DirPathToImportPath(dirPath string) string {
	for _, gopath := range Gopaths() {
		if ustr.Pref(dirPath, gopath) {
			return dirPath[len(filepath.Join(gopath, "src"))+1:]
		}
	}
	return ""
}

// Gopaths returns all paths listed in the `GOPATH` environment variable.
func Gopaths() []string {
	if len(gopaths) == 0 {
		gopaths = filepath.SplitList(os.Getenv("GOPATH"))
	}
	return gopaths
}

// GopathSrc returns the `path/filepath.Join`-ed full directory path for a specified `$GOPATH/src` sub-directory.
// Example: `GopathSrc("github.com", "go-leap", "dev")` yields `c:\gd\src\github.com\go-leap\dev` if `$GOPATH` is `c:\gd`.
func GopathSrc(subDirNames ...string) (gps string) {
	gp := []string{"", "src"}
	for _, gopath := range Gopaths() {
		gp[0] = gopath
		if gps = filepath.Join(append(gp, subDirNames...)...); ufs.IsDir(gps) {
			break
		}
	}
	return
}

// HasGoDevEnv detects (in its first call) the local installation of `go` plus numerous well-known Go developer tools.
func HasGoDevEnv() bool {
	var cmdout, cmderr string
	var err error

	if len(gopaths) > 0 && GoVersion != "" {
		return true
	}

	//  GoVersion
	if cmdout, cmderr, err = urun.CmdExec("go", "tool", "dist", "version"); err == nil && cmderr == "" && cmdout != "" {
		GoVersion = ustr.Trim(cmdout)
	}
	if GoVersion == "" {
		if cmdout, cmderr, err = urun.CmdExec("go", "version"); err == nil && cmderr == "" && cmdout != "" {
			if GoVersion = ustr.TrimPref(ustr.Trim(cmdout), "go version "); GoVersion != "" {
				GoVersion, _ = ustr.BreakOnFirst(GoVersion, " ")
			}
		}
	}
	if GoVersion = ustr.TrimPref(GoVersion, "go"); GoVersion == "" {
		return false
	}

	//  GOPATHs
	if cmdout, cmderr, err = urun.CmdExec("go", "env", "GOPATH"); err != nil || cmderr != "" {
		GoVersion = ""
		gopaths = nil
		return false
	}
	gopaths = filepath.SplitList(ustr.Trim(cmdout))
	for i, gopath := range gopaths {
		if !ufs.IsDir(gopath) {
			gopaths[i] = ""
		}
	}
	if gopaths = ustr.Sans(gopaths, ""); len(gopaths) == 0 {
		GoVersion = ""
		gopaths = nil
		return false
	} else if cmdout, cmderr, err = urun.CmdExec("go", "env", "GOROOT"); err == nil && cmderr == "" && cmdout != "" {
		if gorootdirpath := ustr.Trim(cmdout); gorootdirpath != "" && ufs.IsDir(gorootdirpath) && !ustr.In(gorootdirpath, gopaths...) {
			gopaths = append(gopaths, gorootdirpath)
		}
	}

	i, l := ustr.Idx(GoVersion, '.'), ustr.Last(GoVersion, ".")
	for GoVersionShort = GoVersion; l > i; l = ustr.Last(GoVersionShort, ".") {
		GoVersionShort = GoVersionShort[:l]
	}

	//  OKAY! we ran go command and have 1-or-more GOPATHs, the rest is optional
	stdargs := []string{"-help"}
	urun.CmdsTryStart(map[string]*urun.CmdTry{
		"gofmt":     {Ran: &Has_gofmt, Args: stdargs},
		"goimports": {Ran: &Has_goimports, Args: stdargs},
		"goreturns": {Ran: &Has_goreturns, Args: stdargs},

		"golint":      {Ran: &Has_golint, Args: stdargs},
		"ineffassign": {Ran: &Has_ineffassign, Args: stdargs},
		"errcheck":    {Ran: &Has_errcheck, Args: stdargs},
		"aligncheck":  {Ran: &Has_checkalign, Args: stdargs},
		"structcheck": {Ran: &Has_checkstruct, Args: stdargs},
		"varcheck":    {Ran: &Has_checkvar, Args: stdargs},
		"interfacer":  {Ran: &Has_interfacer, Args: stdargs},
		"unparam":     {Ran: &Has_unparam, Args: stdargs},
		"unindent":    {Ran: &Has_unindent, Args: stdargs},
		"unconvert":   {Ran: &Has_unconvert, Args: stdargs},
		"maligned":    {Ran: &Has_maligned, Args: stdargs},
		"gosimple":    {Ran: &Has_gosimple, Args: stdargs},
		"staticcheck": {Ran: &Has_staticcheck, Args: stdargs},
		"unused":      {Ran: &Has_unused, Args: stdargs},
		"deadcode":    {Ran: &Has_deadcode, Args: stdargs},

		"structlayout": {Ran: &Has_structlayout, Args: stdargs},
		"gorename":     {Ran: &Has_gorename, Args: stdargs},
		"godef":        {Ran: &Has_godef, Args: stdargs},
		"gocode":       {Ran: &Has_gocode, Args: stdargs},
		"guru":         {Ran: &Has_guru, Args: stdargs},
		"gogetdoc":     {Ran: &Has_gogetdoc, Args: stdargs},
		"godocdown":    {Ran: &Has_godocdown, Args: stdargs},
		"godoc":        {Ran: &Has_godoc, Args: stdargs},
		"goconst":      {Ran: &Has_goconst, Args: stdargs},
	})
	return true
}
