# udevgo
--
    import "github.com/go-leap/dev/go"


## Usage

```go
var (

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
```

```go
var (
	GuruScopes        string
	GuruScopeExclPkgs = map[string]bool{}
)
```

```go
var (
	GolintIgnoreSubstrings = []string{
		" should have comment ",

		"ALL_CAPS",
		"underscore",
		"CamelCase",
		"it will be inferred from the right-hand side",
		"should be of the form \"",
		"error strings should",
	}
)
```

#### func  DirPathToImportPath

```go
func DirPathToImportPath(dirPath string) string
```
DirPathToImportPath returns `""` unless `dirPath` is inside a `GOPATH`, in which
case the corresponding Go package import path is returned instead.

#### func  GopathSrc

```go
func GopathSrc(subDirNames ...string) (gps string)
```
GopathSrc returns the `path/filepath.Join`-ed full directory path for a
specified `$GOPATH/src` sub-directory. Example: `GopathSrc("github.com",
"go-leap", "dev")` yields `c:\gd\src\github.com\go-leap\dev` if `$GOPATH` is
`c:\gd`.

#### func  Gopaths

```go
func Gopaths() []string
```
Gopaths returns all paths listed in the `GOPATH` environment variable.

#### func  Gorename

```go
func Gorename(cmdName string, filePath string, offset int, newName string, eol string) (fileEdits udev.SrcMsgs, err error)
```

#### func  HasGoDevEnv

```go
func HasGoDevEnv() bool
```
HasGoDevEnv detects (in its first call) the local installation of `go` plus
numerous well-known Go developer tools.

#### func  LintCheck

```go
func LintCheck(cmdname string, pkgimppath string) (msgs udev.SrcMsgs)
```

#### func  LintErrcheck

```go
func LintErrcheck(pkgimppath string) (msgs udev.SrcMsgs)
```

#### func  LintGoConst

```go
func LintGoConst(dirpath string) (msgs udev.SrcMsgs)
```

#### func  LintGoSimple

```go
func LintGoSimple(pkgimppath string) (msgs udev.SrcMsgs)
```

#### func  LintGoVet

```go
func LintGoVet(pkgimppath string) udev.SrcMsgs
```

#### func  LintGolint

```go
func LintGolint(pkgimppathordirpath string) (msgs udev.SrcMsgs)
```

#### func  LintHonnef

```go
func LintHonnef(cmdname string, pkgimppath string) (msgs udev.SrcMsgs)
```

#### func  LintIneffAssign

```go
func LintIneffAssign(dirrelpath string) (msgs udev.SrcMsgs)
```

#### func  LintMvDan

```go
func LintMvDan(cmdname string, pkgimppath string) udev.SrcMsgs
```

#### func  LintViaPkgImpPath

```go
func LintViaPkgImpPath(cmdname string, pkgimppath string, inclstderr bool) (msgs udev.SrcMsgs)
```

#### func  QueryCallees_Guru

```go
func QueryCallees_Guru(fullsrcfilepath string, srcin string, bytepos1 string, bytepos2 string, altScopes string) (gc *gurujson.Callees, err error)
```

#### func  QueryCallers_Guru

```go
func QueryCallers_Guru(fullsrcfilepath string, srcin string, bytepos1 string, bytepos2 string, altScopes string) (gr []gurujson.Caller, err error)
```

#### func  QueryCallstack_Guru

```go
func QueryCallstack_Guru(fullsrcfilepath string, srcin string, bytepos1 string, bytepos2 string, altScopes string) (gcs *gurujson.CallStack, err error)
```

#### func  QueryCmplSugg_Gocode

```go
func QueryCmplSugg_Gocode(fullsrcfilepath string, srcin string, pos string) (cmpls []map[string]string, err error)
```

#### func  QueryDefDecl_GoDef

```go
func QueryDefDecl_GoDef(fullsrcfilepath string, srcin string, bytepos string) (defdecl string)
```

#### func  QueryDefLoc_Godef

```go
func QueryDefLoc_Godef(fullsrcfilepath string, srcin string, bytepos string) *udev.SrcMsg
```

#### func  QueryDef_Guru

```go
func QueryDef_Guru(fullsrcfilepath string, srcin string, bytepos string) *gurujson.Definition
```

#### func  QueryFreevars_Guru

```go
func QueryFreevars_Guru(fullsrcfilepath string, srcin string, bytepos1 string, bytepos2 string) (gfvs []*gurujson.FreeVar, err error)
```

#### func  QueryImpl_Guru

```go
func QueryImpl_Guru(fullsrcfilepath string, srcin string, bytepos string) *gurujson.Implements
```

#### func  QueryPeers_Guru

```go
func QueryPeers_Guru(fullsrcfilepath string, srcin string, bytepos1 string, bytepos2 string, altScopes string) (gp *gurujson.Peers, err error)
```

#### func  QueryPointsto_Guru

```go
func QueryPointsto_Guru(fullsrcfilepath string, srcin string, bytepos1 string, bytepos2 string, altScopes string) (gr []gurujson.PointsTo, err error)
```

#### func  QueryRefs_Guru

```go
func QueryRefs_Guru(fullsrcfilepath string, srcin string, bytepos string) (refs []gurujson.Ref)
```

#### func  QueryWhat_Guru

```go
func QueryWhat_Guru(fullsrcfilepath string, srcin string, bytepos string) (*gurujson.What, error)
```

#### func  QueryWhicherrs_Guru

```go
func QueryWhicherrs_Guru(fullsrcfilepath string, srcin string, bytepos1 string, bytepos2 string, altScopes string) (gwe *gurujson.WhichErrs, err error)
```

#### type Gogetdoc

```go
type Gogetdoc struct {
	Name   string `json:"name,omitempty"`
	ImpP   string `json:"import,omitempty"`
	Decl   string `json:"decl,omitempty"`
	Doc    string `json:"doc,omitempty"`
	DocUrl string `json:",omitempty"`
	Pos    string `json:"pos,omitempty"`
	Pkg    string `json:"pkg,omitempty"`
	// ImpS   string `json:",omitempty"`
	ImpN string `json:",omitempty"`
	Type string `json:",omitempty"`

	Err     string `json:",omitempty"`
	ErrMsgs string `json:",omitempty"`
}
```


#### type Guru

```go
type Guru struct {
	gurujson.Describe

	IsLessThan func(*gurujson.DescribeMember, *gurujson.DescribeMember) bool `json:"-"`
}
```


#### func  QueryDesc_Guru

```go
func QueryDesc_Guru(fullsrcfilepath string, srcin string, bytepos string) (*Guru, error)
```

#### func (*Guru) Len

```go
func (me *Guru) Len() int
```

#### func (*Guru) Less

```go
func (me *Guru) Less(i int, j int) bool
```

#### func (*Guru) Matches

```go
func (me *Guru) Matches(pM *gurujson.DescribeMember, lowerCaseQuery string) bool
```

#### func (*Guru) Swap

```go
func (me *Guru) Swap(i int, j int)
```
