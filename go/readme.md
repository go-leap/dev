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

#### func  GopathSrcGithub

```go
func GopathSrcGithub(gitHubName string, subDirNames ...string) string
```
GopathSrcGithub essentially calls `GopathSrc` with `"github.com"`, then
`gitHubName` and the given `subDirNames`.

#### func  Gopaths

```go
func Gopaths() []string
```
Gopaths returns all paths listed in the `GOPATH` environment variable.

#### func  HasGoDevEnv

```go
func HasGoDevEnv() bool
```
HasGoDevEnv detects (in its first call) the local installation of `go` plus
numerous well-known Go developer tools.
