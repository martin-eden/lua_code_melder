## What

(2024-11, 2026-04)

Command-line tool to aggregate all `*.lua` files in given directory
(and subdirectories) into one.


## Example

```
$ lua meld.lua

Merge all .lua files under given directory into one executable
code block and print it.

Usage

  meld.lua <modules_dir> <root_module_name> [--indent]

Example

  $ lua meld.lua test_case/ test --indent > ingots/test.lua

Parameters

  <modules_dir> -- Directory from which we search for .lua files

  <root_module_name> -- Name of the "main" module which is called
    in generated code block.

  --indent -- Indent code of embedded modules for nice output.

    Indenting is not safe for code! If source code has multi-line strings
    then spaces will be added to them.

    So if you can test/review result code -- use this option.

-- Martin, 2026-04
```


## How it works

Melder generates Lua source code and prints it to stdout.

It is a file scanner. It gets all `*.lua` files from given `<modules_dir>`
directory (and subdirectories), converts their file names to module names
and loads their contents (source code).

Then it prints code to fill global `package.preload` table (see Lua documentation)
with entries like

```
_G.package.preload[<module_name>] =
  function(...)
<module_code>
  end
```

Module's code is not indented by default. Produced result is safe but ugly.

If `--indent` option is provided then... well then module's code is indented.
It will produce nice foldable output but it's not safe for code.

If code contains multi-line strings like

```Lua
print([[
Hello there!

Multi-line strings in Lua are so cool and hard to capture!

]])
```

then indentation spaces will change contents of string constant.

So use it if you can test or review result code. (Or just sure there are
no multi-line strings. Or really want to.)

And finally it prints activation line, which is a mere `require(<module_name>)`.

`<module_name>` is command-line argument `<root_module_name>`.


Credits:

  * Idea to embed source just as `function(...)` was shared by `mjmouse9999`
    at Lua [maillist][maillist_msg] 2026-04-24
  * Lua authors for ["preload" concept][preload_doc]

Sapienti sat.


## Requirements

  * Lua 5.3 (or 5.4, 5.5)
  * Linux file system
  * Commands (`sh`, `find`)


## Install/remove

  Clone repo.

  As usual all needed guts from [`workshop`][workshop] are included.


## Additional stuff

  * There is compiled binary in [`bin/`][bin]

    That's melded code of this tool compiled by `luac` (v5.3) and
    shebang line `#!/usr/local/bin/lua`.

    Practically it means that if you have Lua 5.3 (and under Linux)
    you can save this file to your `~/bin` and use it as `meld` command.

  * There are build scripts in [`deploy_maker/`][deploy_maker]

    That's my scripts I used to generate compiled binary.

    You can use them too but you'll need to clone [`workshop`][workshop]
    repo into `src/workshop` (replacing existing one) and (in case
    of problems) rollback `workshop`'s date to near current date,
    2026-04-25.

    That's because deploy generation requires more workshop's code
    than this tool.


## See also

* [`workshop`][workshop] -- My personal Lua framework on which this tool is based
* [My other repositories][contents]

[maillist_msg]: https://groups.google.com/g/lua-l/c/AuXFlvZr42M/m/dwO9Aob1AAAJ
[preload_doc]: https://lua.org/manual/5.5/manual.html#pdf-package.preload

[bin]: bin/
[deploy_maker]: deploy_maker/
[workshop]: https://github.com/martin-eden/workshop
[contents]: https://github.com/martin-eden/contents
