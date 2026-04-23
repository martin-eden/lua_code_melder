## What

(2024-11, 2026-04)

Command-line tool to aggregate all `*.lua` files in given directory
(and subdirectories) into one.


## Example

```
$ lua meld.lua

Merge all .lua files under given directory into one executable code block
and print it.

Usage

  meld.lua <modules_dir> <root_module_name>

Example

  $ lua meld.lua test_case/ test > ingots/test.lua

Parameters

  modules_dir -- Directory from which we search for .lua files

  root_module_name -- Name of the "main" module which is called
    in generated code block.

-- Martin, 2026-04
```


## How it works

Melder generates Lua source code and prints it to stdout.

It is a file scanner. It gets all `*.lua` files from given `<modules_dir>`
directory (and subdirectories), converts their file names to module names
and loads their contents (source code).

Then it prints table with them:
```
local Modules = {
  <module_name> = <module_code>,
  [...]
}
```

Then it prints code that compiles their code but not runs it.
Compiled code is stored in `_G.package.preload` table (see Lua documentation).
Hat tip from me to Lua authors for this nice design.

```Lua
do
  local add_module =
    function(module_name, module_code_str)
      local compiled_code = assert(load(module_code_str, module_name, 't'))

      _G.package.preload[module_name] =
        function(...)
          return compiled_code(...)
        end
    end

  for module_name, module_code_str in pairs(Modules) do
    add_module(module_name, module_code_str)
  end
end
```

And finally it prints activation line, which is a mere `require(<module_name>)`.

`<module_name>` is command-line argument `<root_module_name>`.

Sapienti sat.


## Requirements

  * Lua 5.5
  * Linux
  * Binutils (`find`)


## Install/remove

  Clone repo.

  As usual all needed guts from [`workshop`][workshop] are included.


## See also

* [`workshop`][workshop] -- My personal Lua framework on which this tool is based
* [My other repositories][contents]

[workshop]: https://github.com/martin-eden/workshop
[contents]: https://github.com/martin-eden/contents
