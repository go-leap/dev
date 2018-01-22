# udevgo
--
    import "github.com/go-leap/dev/go"


## Usage

```go
var (
	// GoPaths is set by `AllGoPaths` to contain all locally declared `GOPATH`s.
	GoPaths []string
)
```

#### func  AllGoPaths

```go
func AllGoPaths() []string
```
AllGoPaths returns `GoPaths`, or if empty sets it to all paths listed in the
`GOPATH` environment variable.
