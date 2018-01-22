package udevgo

import (
	"os"
	"path/filepath"
)

var (
	// GoPaths is set by `AllGoPaths` to contain all locally declared `GOPATH`s.
	GoPaths []string
)

//	AllGoPaths returns `GoPaths`, or if empty sets it to all paths listed in the `GOPATH` environment variable.
func AllGoPaths() []string {
	if len(GoPaths) == 0 {
		GoPaths = filepath.SplitList(os.Getenv("GOPATH"))
	}
	return GoPaths
}
