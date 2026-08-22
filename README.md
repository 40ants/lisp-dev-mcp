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
* Depends on: [40ants-logging][422a], [40ants-mcp][6700], [40ants-slynk][2e1d], [alexandria][8236], [bordeaux-threads][3dbf], [defmain][3266], [find-port][0d73], [jsonrpc][a9bd], [log4cl][7f8b], [openrpc-server][c8e7], [serapeum][c41d], [str][ef7f], [trivial-backtrace][fc0e], [yason][aba2]

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

#### [package](3754) `40ants-lisp-dev-mcp/core`

<a id="x-2840ANTS-LISP-DEV-MCP-DOCS-2FINDEX-3A-3A-7C-4040ANTS-LISP-DEV-MCP-2FCORE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3ACHOOSE-PORT-20FUNCTION-29"></a>

##### [function](d1c8) `40ants-lisp-dev-mcp/core:choose-port` port &key (config \*opencode-config-pathname\*)

Resolves `PORT` into a concrete `TCP` port number and returns it as the first value.

As the second value returns T when the resolved port differs from the one
recorded in the Opencode config.

   `PORT` can be:
     - an `INTEGER`, used as-is after checking it is free;
     - the `:AUTO` keyword (or the string "auto"), in which case a free port
       is selected automatically, reusing the port from the Opencode config
       when it is still available.

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3AGET-PORT-FROM-ASSISTANT-CONFIG-20FUNCTION-29"></a>

##### [function](64fa) `40ants-lisp-dev-mcp/core:get-port-from-assistant-config` &key (config \*opencode-config-pathname\*)

Returns the port recorded in the Opencode config, or `NIL`.

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3AMAKE-DEFAULT-CONFIG-20FUNCTION-29"></a>

##### [function](0635) `40ants-lisp-dev-mcp/core:make-default-config`

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3AREAD-CONFIG-20FUNCTION-29"></a>

##### [function](fa17) `40ants-lisp-dev-mcp/core:read-config` path

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3ASTART-SERVER-20FUNCTION-29"></a>

##### [function](7ef8) `40ants-lisp-dev-mcp/core:start-server` &key port (in-thread t) update-config (opencode-config \*opencode-config-pathname\*)

Starts the `MCP` server.

`PORT` controls the transport and the port:
  - `NIL` (the default) uses the stdio transport;
  - an `INTEGER` uses the Streaming `HTTP` transport on that `TCP` port;
  - `:AUTO` selects a free `TCP` port automatically, reusing the port from
    the Opencode config when it is still available.

`IN-THREAD` controls whether the server runs in a background thread (the
default) or blocks the caller.

When `UPDATE-CONFIG` is true and a port was selected (or reused), the chosen
port is written into the Opencode config file pointed to by `OPENCODE-CONFIG`
(which defaults to [`*opencode-config-pathname*`][c75b]).

Returns the server thread when `IN-THREAD` is true, otherwise blocks.

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3AUPDATE-PORT-IN-CONFIG-20FUNCTION-29"></a>

##### [function](2dde) `40ants-lisp-dev-mcp/core:update-port-in-config` port &key (config \*opencode-config-pathname\*)

Writes the given `PORT` into the Opencode config file,
creating a default config when the file does not exist yet.

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3AWRITE-CONFIG-20FUNCTION-29"></a>

##### [function](e01a) `40ants-lisp-dev-mcp/core:write-config` file data

<a id="x-2840ANTS-LISP-DEV-MCP-DOCS-2FINDEX-3A-3A-7C-4040ANTS-LISP-DEV-MCP-2FCORE-3FVariables-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Variables

<a id="x-2840ANTS-LISP-DEV-MCP-2FCORE-3A-2AOPENCODE-CONFIG-PATHNAME-2A-20-28VARIABLE-29-29"></a>

##### [variable](86f3) `40ants-lisp-dev-mcp/core:*opencode-config-pathname*` #P"opencode.json"

Pathname of the Opencode config file which is updated when
[`start-server`][da24] is called with `:UPDATE-CONFIG` T.

You can rebind this special variable or pass an explicit
`:OPENCODE-CONFIG` argument to [`start-server`][da24], [`choose-port`][3ec7],
[`get-port-from-assistant-config`][66bc] and [`update-port-in-config`][fd15].


[7ed8]: https://40ants/lisp-dev-mcp/
[c75b]: https://40ants/lisp-dev-mcp/#x-2840ANTS-LISP-DEV-MCP-2FCORE-3A-2AOPENCODE-CONFIG-PATHNAME-2A-20-28VARIABLE-29-29
[3ec7]: https://40ants/lisp-dev-mcp/#x-2840ANTS-LISP-DEV-MCP-2FCORE-3ACHOOSE-PORT-20FUNCTION-29
[66bc]: https://40ants/lisp-dev-mcp/#x-2840ANTS-LISP-DEV-MCP-2FCORE-3AGET-PORT-FROM-ASSISTANT-CONFIG-20FUNCTION-29
[da24]: https://40ants/lisp-dev-mcp/#x-2840ANTS-LISP-DEV-MCP-2FCORE-3ASTART-SERVER-20FUNCTION-29
[fd15]: https://40ants/lisp-dev-mcp/#x-2840ANTS-LISP-DEV-MCP-2FCORE-3AUPDATE-PORT-IN-CONFIG-20FUNCTION-29
[6421]: https://github.com/40ants/lisp-dev-mcp
[7c1b]: https://github.com/40ants/lisp-dev-mcp/actions
[3754]: https://github.com/40ants/lisp-dev-mcp/blob/d91abcf386a7f443f28685fa5d1584112d2149e6/src/core.lisp#L1
[86f3]: https://github.com/40ants/lisp-dev-mcp/blob/d91abcf386a7f443f28685fa5d1584112d2149e6/src/core.lisp#L123
[fa17]: https://github.com/40ants/lisp-dev-mcp/blob/d91abcf386a7f443f28685fa5d1584112d2149e6/src/core.lisp#L136
[e01a]: https://github.com/40ants/lisp-dev-mcp/blob/d91abcf386a7f443f28685fa5d1584112d2149e6/src/core.lisp#L146
[0635]: https://github.com/40ants/lisp-dev-mcp/blob/d91abcf386a7f443f28685fa5d1584112d2149e6/src/core.lisp#L158
[64fa]: https://github.com/40ants/lisp-dev-mcp/blob/d91abcf386a7f443f28685fa5d1584112d2149e6/src/core.lisp#L170
[2dde]: https://github.com/40ants/lisp-dev-mcp/blob/d91abcf386a7f443f28685fa5d1584112d2149e6/src/core.lisp#L186
[d1c8]: https://github.com/40ants/lisp-dev-mcp/blob/d91abcf386a7f443f28685fa5d1584112d2149e6/src/core.lisp#L204
[7ef8]: https://github.com/40ants/lisp-dev-mcp/blob/d91abcf386a7f443f28685fa5d1584112d2149e6/src/core.lisp#L245
[58fb]: https://github.com/40ants/lisp-dev-mcp/issues
[422a]: https://quickdocs.org/40ants-logging
[6700]: https://quickdocs.org/40ants-mcp
[2e1d]: https://quickdocs.org/40ants-slynk
[8236]: https://quickdocs.org/alexandria
[3dbf]: https://quickdocs.org/bordeaux-threads
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
