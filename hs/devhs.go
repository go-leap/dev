package udevhs

import (
	"strings"

	"github.com/go-leap/run"
)

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
	// Has_hsautofix       bool
	// Has_hscabal         bool
	// Has_hsclearimports  bool
	// Has_hsdev           bool
	// Has_hshayoo         bool
	// Has_hsinspect       bool
)

// HasHsDevEnv detects (in its first call) the local installation of `stack` plus numerous well-known Haskell developer tools.
func HasHsDevEnv() bool {
	var cmdout, cmderr string
	var err error

	if StackVersion != "" {
		return true
	}
	if cmdout, cmderr, err = urun.CmdExec("stack", "--numeric-version", "--no-terminal", "--color", "never"); err == nil && cmderr == "" && cmdout != "" {
		if StackVersion = strings.TrimSpace(cmdout); StackVersion != "" {
			argsver, argshelp := []string{"--version"}, []string{"--help"}
			urun.CmdsTryStart(map[string]*urun.CmdTry{
				"ghc-mod":             {Ran: &Has_ghcmod, Args: argsver},
				"ghc-hare":            {Ran: &Has_hare, Args: argsver},
				"hsimport":            {Ran: &Has_hsimport, Args: argsver},
				"hasktags":            {Ran: &Has_hasktags, Args: argshelp},
				"lushtags":            {Ran: &Has_lushtags, Args: argshelp},
				"hothasktags":         {Ran: &Has_hothasktags, Args: argshelp},
				"dead-code-detection": {Ran: &Has_deadcodedetect, Args: argsver},
				"pointfree":           {Ran: &Has_pointfree},
				"pointful":            {Ran: &Has_pointful},
				"refactor":            {Ran: &Has_apply_refact},
				"hoogle":              {Ran: &Has_hoogle, Args: argsver},
				"hlint":               {Ran: &Has_hlint, Args: argsver},
				"doctest":             {Ran: &Has_doctest, Args: argsver},
				"intero":              {Ran: &Has_intero, Args: argsver},
				"hindent":             {Ran: &Has_hindent, Args: argsver},
				"brittany":            {Ran: &Has_brittany, Args: argsver},
				"stylish-haskell":     {Ran: &Has_stylish_haskell, Args: argsver},
				"ht-refact":           {Ran: &Has_htrefact},
				"ht-daemon":           {Ran: &Has_htdaemon, Args: []string{"hows'it hangin holmes"}},
			})
		}
	}
	return StackVersion != ""
}
