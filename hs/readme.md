# udevhs
--
    import "github.com/go-leap/dev/hs"


## Usage

```go
var (
	// Sensible default args for running `stack`
	StackArgs = []string{"--dump-logs", "--no-time-in-log", "--no-install-ghc", "--skip-ghc-check", "--skip-msys", "--no-terminal", "--color", "never", "--jobs", "8", "--verbosity", "info"}
	// Additional sensible default args for running `stack build`
	StackArgsBuild = []string{"--copy-bins", "--no-haddock", "--no-open", "--no-haddock-internal", "--no-haddock-deps", "--no-keep-going", "--no-test", "--no-rerun-tests", "--no-bench", "--no-run-benchmarks", "--no-cabal-verbose", "--no-split-objs"}

	// All these are initially set via the first call to `HasHsDevEnv`.
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

```go
var (
	// HlintIgnore contains lints to be ignored, passed via `--ignore` args to hlint during `LintHlint`.
	HlintIgnore = []string{

		"Use infix",
		"Use camelCase",
		"Use String",
	}
)
```

#### func  HasHsDevEnv

```go
func HasHsDevEnv() bool
```
HasHsDevEnv detects (in its first call) the local installation of `stack` plus
numerous well-known Haskell developer tools.

#### func  LintHlint

```go
func LintHlint(filePaths []string) (msgs udev.SrcMsgs)
```
LintHlint runs the `hlint` program for the specified `filePaths`. The returned
`SrcMsgs` will have at least `Msg`, `Ref` and all 4 `Pos` fields set. The
`SrcMsg.Misc` field might be set (to Module or Decl or Module.Decl) or not. If
an hlint offers an edit suggestion, it is stored in `SrcMsg.Data` under the
`From`, `To` and `Note` keys.
