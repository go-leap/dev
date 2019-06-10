# udev
--
    import "github.com/go-leap/dev"


## Usage

#### type SrcMsg

```go
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
```

SrcMsg captures tool-provided information about some source code location.

Meaning and usage/omission of individual fields can vary by tool and/or
use-case, and are loosely defined between the producer and consumers of a
`SrcMsg`.

#### func  SrcMsgFromLn

```go
func SrcMsgFromLn(line string) (item *SrcMsg)
```
SrcMsgFromLn returns the pointer to a newly allocated `SrcMsg` if it can
sensibly extract `Pos1Ln`, `Pos1Ch`, `Msg` and `Ref` from `line`; otherwise
`nil` is returned.

#### type SrcMsgs

```go
type SrcMsgs []*SrcMsg
```

SrcMsgs implements `sort.Interface`.

#### func  CmdExecOnSrc

```go
func CmdExecOnSrc(inclStderr bool, perLine func(string) string, cmdName string, cmdArgs ...string) SrcMsgs
```
CmdExecOnSrc executes the specified command and returns the `SrcMsgsFromLns` of
its `CombinedOutput` (if `inclStderr`) or `Output`.

`perLine` may be `nil`, otherwise it is called for each line prior to being
ultimately passed to `SrcMsgFromLn`.

#### func  CmdExecOnSrcIn

```go
func CmdExecOnSrcIn(dir string, inclStderr bool, perLine func(string) string, cmdName string, cmdArgs ...string) SrcMsgs
```
CmdExecOnSrcIn executes the specified command and returns the `SrcMsgsFromLns`
of its `CombinedOutput` (if `inclStderr`) or `Output`.

`perLine` may be `nil`, otherwise it is called for each line prior to being
ultimately passed to `SrcMsgFromLn`.

#### func  CmdExecOnStdin

```go
func CmdExecOnStdin(stdin string, dir string, perLine func(string) string, cmdName string, cmdArgs ...string) (SrcMsgs, error)
```
CmdExecOnStdin executes the specified command and returns the `SrcMsgsFromLns`
of its `CombinedOutput` (if `inclStderr`) or `Output`.

`perLine` may be `nil`, otherwise it is called for each line prior to being
ultimately passed to `SrcMsgFromLn`.

#### func  SrcMsgsFromLns

```go
func SrcMsgsFromLns(lines []string) (msgs SrcMsgs)
```
SrcMsgsFromLns returns all `SrcMsg` results from calling `SrcMsgFromLn` on all
`lines`.

#### func (SrcMsgs) Len

```go
func (me SrcMsgs) Len() int
```
Len implements `sort.Interface`.

#### func (SrcMsgs) Less

```go
func (me SrcMsgs) Less(i, j int) bool
```
Less implements `sort.Interface`.

#### func (SrcMsgs) Swap

```go
func (me SrcMsgs) Swap(i, j int)
```
Swap implements `sort.Interface`.
