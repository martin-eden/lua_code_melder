[![DeepWiki][DeepWiki_Logo]][DeepWiki_Repo] (sometimes AI explains it better)

## What

| Created | Updated |  Size   | License |
|:-------:|:-------:|:-------:|:-------:|
| 2024-11 | 2026-06 | < 35 K  |  LGPL3  |

Command-line tool to aggregate all `*.lua` files in given directory
and subdirectories into one.

Compiles multi-file Lua program into one file.


## Usage

```
Merge all .lua files under given directory into one executable
code block and print it.

Usage

  meld <modules_dir> <root_module_name>

Example

  $ meld test_case/ test > ingots/test.lua

Parameters

  <modules_dir> -- Directory from which we search for .lua files.

  <root_module_name> -- Name of the "main" module which is called
    in generated code block.

-- Martin, 2026-06
```


## How it works

Melder generates Lua source code and prints it to stdout.

It is a file scanner. It gets all `*.lua` files from given `<modules_dir>`
directory (and subdirectories), converts their file names to module names
and loads their contents (source code).

Then it prints code to fill global `package.preload` table with entries like

```
_G.package.preload[<module_name>] =
function(...)
<module_code>
end
```

And finally it prints activation line, which is a mere `require(<root_module_name>)`.


## Requirements

  * Lua 5.3 (or 5.4, 5.5)
  * Linux file system
  * Commands (`sh`, `find`)


## Install/remove

  * Save file `meld` from [`bin/`][bin]
  * Place it where you like (I'm placing to `~/bin/`)


## Modify

  * Clone repo
  * Modify files in [`src/`][src]


## Rebuild

  * Clone [`workshop`][workshop] repo
  * Checkout it to date near `2026-06-16`
  * Modify `package.path` in [`build/create_deploy.lua`][create_deploy]
    so it can find your cloned `workshop` repo
  * Run [`build/rebuild.sh`][rebuild]


## Credits

  * Idea to embed source just as `function(...)` was shared by `mjmouse9999`
    at Lua [maillist][maillist_msg] 2026-04-24
  * Lua authors for "preload" concept


## See also

  * [`package.preload`][preload_doc] documentation
  * [Files lister][FilesLister] module
  * [`workshop`][workshop] -- My personal Lua framework on which this tool is based
  * [My other projects][contents]


[DeepWiki_Logo]: https://deepwiki.com/badge.svg
[DeepWiki_Repo]: https://deepwiki.com/martin-eden/lua_code_melder

[bin]: bin/
[src]: src/
[build]: build/
[create_deploy]: build/create_deploy.lua
[rebuild]: build/rebuild.sh

[maillist_msg]: https://groups.google.com/g/lua-l/c/AuXFlvZr42M/m/dwO9Aob1AAAJ

[preload_doc]: https://lua.org/manual/5.5/manual.html#pdf-package.preload
[FilesLister]: src/workshop/concepts/FilesLister/
[workshop]: https://github.com/martin-eden/workshop
[contents]: https://github.com/martin-eden/contents
