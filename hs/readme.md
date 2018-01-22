# udevhs
--
    import "github.com/go-leap/dev/hs"


## Usage

```go
var (
	StackVersion string

	Has_hindent         bool
	Has_brittany        bool
	Has_stylish_haskell bool
	Has_hlint           bool
	Has_ghcmod          bool
	Has_pointfree       bool
	Has_pointful        bool
	Has_hsimport        bool
	Has_htrefact        bool
	Has_htdaemon        bool
	Has_hare            bool
	Has_hasktags        bool
	Has_hothasktags     bool
	Has_lushtags        bool
	Has_deadcodedetect  bool
	Has_intero          bool
	Has_doctest         bool
	Has_hoogle          bool
	Has_apply_refact    bool
)
```
All these are initially set via the first call to `HasHsDevEnv`.

#### func  HasHsDevEnv

```go
func HasHsDevEnv() bool
```
HasHsDevEnv detects (in its first call) the local installation of `stack` plus
numerous well-known Haskell developer tools.
