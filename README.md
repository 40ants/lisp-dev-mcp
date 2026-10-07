<a id="x-2840ANTS-LISP-DEV-MCP-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# 40ants-lisp-dev-mcp - MCP which gives LLM tools for working with running Lisp image.

<a id="40-ants-lisp-dev-mcp-asdf-system-details"></a>

## 40ANTS-LISP-DEV-MCP ASDF System Details

* Description: `MCP` which gives `LLM` tools for working with running Lisp image.
* Licence: Unlicense
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Homepage: [https://40ants/lisp-dev-mcp/][7ed8]
* Bug tracker: [https://github.com/40ants/lisp-dev-mcp/issues][58fb]
* Source control: [GIT][6421]
* Depends on: [40ants-logging][422a], [40ants-mcp][6700], [40ants-slynk][2e1d], [alexandria][8236], [bordeaux-threads][3dbf], [cl-toml][3d58], [defmain][3266], [find-port][0d73], [jsonrpc][a9bd], [log4cl][7f8b], [openrpc-server][c8e7], [serapeum][c41d], [str][ef7f], [trivial-backtrace][fc0e], [yason][aba2]

[![](https://github-actions.40ants.com/40ants/lisp-dev-mcp/matrix.svg?only=ci.run-tests)][7c1b]

![](http://quickdocs.org/badge/40ants-lisp-dev-mcp.svg)

<a id="x-2840ANTS-LISP-DEV-MCP-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
```
then:

```
ros install 40ants/lisp-dev-mcp
```
<a id="x-2840ANTS-LISP-DEV-MCP-DOCS-2FINDEX-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

<a id="running-in-stdio-mode"></a>

### Running in stdio mode

Here is an example config to add lisp-dev-mcp to Qwen:

```
{
  "mcpServers": {
    "lisp-dev": {
      "command": "lisp-dev-mcp",
      "args": []
    }
  },
  "$version": 2
}
```
If you want to debug `MCP` server, then you might start it will logging output and a `SLYNK` port opened:

```
{
  "mcpServers": {
    "lisp-dev": {
      "command": "lisp-dev-mcp",
      "args": ["--log", "mcp.log", "--verbose"],
      "env": {
        "SLYNK_PORT": "9991"
      }
    }
  },
  "$version": 2
}
```
<a id="running-in-http-streaming-mode"></a>

### Running in HTTP streaming mode

<a id="with-open-code"></a>

#### With OpenCode

Let the server pick a free port and record it into `opencode.json` automatically.
Start the lisp process:

```
qlot exec roswell/lisp-dev-mcp.ros --port auto --update-config
```
or, from the `REPL`:

```
(ql:quickload :40ants-lisp-dev-mcp)

(40ants-lisp-dev-mcp/core:start-server :port :auto :update-config t)
```
The server reuses the port already recorded in `opencode.json` when it is still
free, otherwise it chooses a new one and writes `http://localhost:<port>/mcp`
into the `mcp.lisp-dev-mcp.url` key. The resulting config is picked up by OpenCode
without any manual editing:

```
{
    "$schema": "https://opencode.ai/config.json",
    "mcp": {
        "lisp-dev-mcp": {
            "type": "remote",
            "url": "http://localhost:<port>/mcp"
        }
    }
}
```
<a id="with-codex-or-multiple-agents"></a>

#### With Codex or multiple agents

Select Codex to create or update `.codex/config.toml` in the current directory:

```
qlot exec roswell/lisp-dev-mcp.ros --port auto --update-config --agents codex
```
or, from the `REPL`:

```
(40ants-lisp-dev-mcp/core:start-server
 :port :auto :update-config t :agents '(:codex))
```
The minimal Codex config contains:

```toml
[mcp_servers.lisp-dev-mcp]
url = "http://localhost:40001/mcp"
enabled = true
startup_timeout_sec = 10
tool_timeout_sec = 60
```
The `URL` uses the chosen port. Existing settings, other sections and `MCP` servers
are preserved; enabled and timeout defaults are added only when absent. A file
containing only `[features]` is supported, as are missing files and directories.
For example, starting the server on port 40001 with `:agents '(:codex)` and
`:update-config t` updates this existing file:

```toml
[features]
hooks = true
```
to a config with both sections:

```toml
[features]
hooks = true

[mcp_servers.lisp-dev-mcp]
url = "http://localhost:40001/mcp"
enabled = true
startup_timeout_sec = 10
tool_timeout_sec = 60
```
An existing `enabled = false` and custom timeout values remain unchanged.
Comments and formatting may change. The cl-toml library supports `TOML` 0.4;
unsupported syntax causes an error before config files are written.

To update both agents, use `--agents opencode,codex` or
`:agents '(:opencode :codex)`. Automatic port selection checks recorded ports
in the selected order and reuses the first free port. Both agents receive the
same `URL`, including when one config already has the chosen port and the other
needs to be created. The default is `:agents '(:opencode)`; `:agents nil` skips
config reads and updates. Stdio mode never writes config files.

Override paths with `:opencode-config` and `:codex-config`, or rebind
`*opencode-config-pathname*` and `*codex-config-pathname*`.
The config helpers also accept `:agent :codex`, for example:

```
(40ants-lisp-dev-mcp/core:update-port-in-config
 40001 :agent :codex :config #P".codex/config.toml")
```
<a id="with-a-fixed-port-other-id-es"></a>

#### With a fixed port (other IDEs)

For clients that use a different config format, or when you prefer a fixed port,
pass an explicit port number. Start the lisp process:

```
qlot exec roswell/lisp-dev-mcp.ros --port 7890
```
or in the `REPL`:

```
(ql:quickload :40ants-lisp-dev-mcp)

(40ants-lisp-dev-mcp/core:start-server :port 7890)
```
then configure your `IDE`:

```
{
  "mcpServers": {
    "lisp-dev": {
      "url": "http://localhost:7890/mcp"
    }
  },
  "$version": 2
}
```
<a id="x-2840ANTS-LISP-DEV-MCP-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-2840ANTS-LISP-DEV-MCP-DOCS-2FINDEX-3A-3A-4040ANTS-LISP-DEV-MCP-2FCORE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-LISP-DEV-MCP/CORE

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-2240ANTS-LISP-DEV-MCP-2FCORE-22-29-20PACKAGE-29"></a>

#### [package](3b6c) `40ants-lisp-dev-mcp/core`

<a id="x-2840ANTS-LISP-DEV-MCP-DOCS-2FINDEX-3A-3A-7C-4040ANTS-LISP-DEV-MCP-2FCORE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3ACHOOSE-PORT-20FUNCTION-29"></a>

##### [function](528a) `40ants-lisp-dev-mcp/core:choose-port` port &key (config \*opencode-config-pathname\*) (codex-config \*codex-config-pathname\*) (agents '(:opencode))

Resolves `PORT` into a concrete `TCP` port number and returns it as the first value.

As the second value returns T when the resolved port differs from at least one
selected agent's recorded port (including a missing config).

   `PORT` can be:
     - an `INTEGER`, used as-is after checking it is free;
     - the `:AUTO` keyword (or the string "auto"), in which case a free port
       is selected automatically, reusing the first free recorded port in
       `AGENTS` order. `AGENTS` defaults to (`:OPENCODE`); `NIL` skips reading configs.
       `CONFIG` points to OpenCode, `CODEX-CONFIG` points to Codex.

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3AGET-PORT-FROM-ASSISTANT-CONFIG-20FUNCTION-29"></a>

##### [function](6dbd) `40ants-lisp-dev-mcp/core:get-port-from-assistant-config` &key (agent :opencode) (config (agent-config-pathname agent))

Returns the `MCP` port recorded in `CONFIG` for `AGENT`, or `NIL`.
`AGENT` is `:OPENCODE` (the default) or `:CODEX`; `CONFIG` defaults to that agent's pathname.

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3AMAKE-DEFAULT-CONFIG-20FUNCTION-29"></a>

##### [function](da46) `40ants-lisp-dev-mcp/core:make-default-config`

Returns the default OpenCode config with a placeholder `MCP` `URL`.

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3AREAD-CONFIG-20FUNCTION-29"></a>

##### [function](1646) `40ants-lisp-dev-mcp/core:read-config` path

Reads the OpenCode `JSON` config at `PATH`, preserving arrays, booleans and nulls.

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3ASTART-SERVER-20FUNCTION-29"></a>

##### [function](9456) `40ants-lisp-dev-mcp/core:start-server` &key port (in-thread t) update-config (agents '(:opencode)) (opencode-config \*opencode-config-pathname\*) (codex-config \*codex-config-pathname\*)

Starts the `MCP` server.

`PORT` controls the transport and the port:
  - `NIL` (the default) uses the stdio transport;
  - an `INTEGER` uses the Streaming `HTTP` transport on that `TCP` port;
  - `:AUTO` selects a free `TCP` port automatically, reusing the port from
    the selected agents' configs in `AGENTS` order when it is still available.

`IN-THREAD` controls whether the server runs in a background thread (the
default) or blocks the caller.

When `UPDATE-CONFIG` is true and a port was selected (or reused), the chosen
port is written into every selected agent's config, creating missing files
and directories. `AGENTS` is a list of `:OPENCODE` and `:CODEX`, defaulting to
(`:OPENCODE`); `NIL` disables config reads and updates. `OPENCODE-CONFIG` and
`CODEX-CONFIG` default to [`*opencode-config-pathname*`][c75b] and [`*codex-config-pathname*`][b956].
Codex settings other than the `URL` are preserved; missing enabled and timeout
settings receive defaults. `TOML` comments and formatting may change.

Returns the server thread when `IN-THREAD` is true, otherwise blocks.

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3AUPDATE-PORT-IN-CONFIG-20FUNCTION-29"></a>

##### [function](4300) `40ants-lisp-dev-mcp/core:update-port-in-config` port &key (agent :opencode) (config (agent-config-pathname agent))

Writes `PORT` into the `MCP` `URL` in `CONFIG` for `AGENT` (`:OPENCODE` or `:CODEX`).
Creates missing directories and config tables. Codex gets enabled=true and
timeouts of 10 and 60 seconds when those settings are absent; existing values
are preserved. `TOML` comments and formatting are not preserved.

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3AWRITE-CONFIG-20FUNCTION-29"></a>

##### [function](8a0f) `40ants-lisp-dev-mcp/core:write-config` file data

Writes `DATA` as an OpenCode `JSON` config to `FILE`.

<a id="x-2840ANTS-LISP-DEV-MCP-DOCS-2FINDEX-3A-3A-7C-4040ANTS-LISP-DEV-MCP-2FCORE-3FVariables-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Variables

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3A-2ACODEX-CONFIG-PATHNAME-2A-20-28VARIABLE-29-29"></a>

##### [variable](77c5) `40ants-lisp-dev-mcp/core:*codex-config-pathname*` #P".codex/config.toml"

Pathname of the Codex config, relative to the current working directory.
Rebind this variable or pass `:CODEX-CONFIG` to [`start-server`][da24] and [`choose-port`][3ec7],
or `:CONFIG` with `:AGENT` `:CODEX` to the config helpers.

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3A-2AOPENCODE-CONFIG-PATHNAME-2A-20-28VARIABLE-29-29"></a>

##### [variable](fcd9) `40ants-lisp-dev-mcp/core:*opencode-config-pathname*` #P"opencode.json"

Pathname of the Opencode config file which is updated when
[`start-server`][da24] is called with `:UPDATE-CONFIG` T.

You can rebind this variable, pass `:OPENCODE-CONFIG` to [`start-server`][da24],
or pass `:CONFIG` to the config helpers and [`choose-port`][3ec7].


[7ed8]: https://40ants/lisp-dev-mcp/
[b956]: https://40ants/lisp-dev-mcp/#x-2840ANTS-LISP-DEV-MCP-2FCORE-3A-2ACODEX-CONFIG-PATHNAME-2A-20-28VARIABLE-29-29
[c75b]: https://40ants/lisp-dev-mcp/#x-2840ANTS-LISP-DEV-MCP-2FCORE-3A-2AOPENCODE-CONFIG-PATHNAME-2A-20-28VARIABLE-29-29
[3ec7]: https://40ants/lisp-dev-mcp/#x-2840ANTS-LISP-DEV-MCP-2FCORE-3ACHOOSE-PORT-20FUNCTION-29
[da24]: https://40ants/lisp-dev-mcp/#x-2840ANTS-LISP-DEV-MCP-2FCORE-3ASTART-SERVER-20FUNCTION-29
[6421]: https://github.com/40ants/lisp-dev-mcp
[7c1b]: https://github.com/40ants/lisp-dev-mcp/actions
[3b6c]: https://github.com/40ants/lisp-dev-mcp/blob/e6cdd51ae38962652eec3fc90fb32c0aa5c7d6ff/src/core.lisp#L1
[fcd9]: https://github.com/40ants/lisp-dev-mcp/blob/e6cdd51ae38962652eec3fc90fb32c0aa5c7d6ff/src/core.lisp#L124
[77c5]: https://github.com/40ants/lisp-dev-mcp/blob/e6cdd51ae38962652eec3fc90fb32c0aa5c7d6ff/src/core.lisp#L133
[1646]: https://github.com/40ants/lisp-dev-mcp/blob/e6cdd51ae38962652eec3fc90fb32c0aa5c7d6ff/src/core.lisp#L157
[8a0f]: https://github.com/40ants/lisp-dev-mcp/blob/e6cdd51ae38962652eec3fc90fb32c0aa5c7d6ff/src/core.lisp#L168
[da46]: https://github.com/40ants/lisp-dev-mcp/blob/e6cdd51ae38962652eec3fc90fb32c0aa5c7d6ff/src/core.lisp#L181
[6dbd]: https://github.com/40ants/lisp-dev-mcp/blob/e6cdd51ae38962652eec3fc90fb32c0aa5c7d6ff/src/core.lisp#L225
[4300]: https://github.com/40ants/lisp-dev-mcp/blob/e6cdd51ae38962652eec3fc90fb32c0aa5c7d6ff/src/core.lisp#L269
[528a]: https://github.com/40ants/lisp-dev-mcp/blob/e6cdd51ae38962652eec3fc90fb32c0aa5c7d6ff/src/core.lisp#L283
[9456]: https://github.com/40ants/lisp-dev-mcp/blob/e6cdd51ae38962652eec3fc90fb32c0aa5c7d6ff/src/core.lisp#L328
[58fb]: https://github.com/40ants/lisp-dev-mcp/issues
[422a]: https://quickdocs.org/40ants-logging
[6700]: https://quickdocs.org/40ants-mcp
[2e1d]: https://quickdocs.org/40ants-slynk
[8236]: https://quickdocs.org/alexandria
[3dbf]: https://quickdocs.org/bordeaux-threads
[3d58]: https://quickdocs.org/cl-toml
[3266]: https://quickdocs.org/defmain
[0d73]: https://quickdocs.org/find-port
[a9bd]: https://quickdocs.org/jsonrpc
[7f8b]: https://quickdocs.org/log4cl
[c8e7]: https://quickdocs.org/openrpc-server
[c41d]: https://quickdocs.org/serapeum
[ef7f]: https://quickdocs.org/str
[fc0e]: https://quickdocs.org/trivial-backtrace
[aba2]: https://quickdocs.org/yason

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
