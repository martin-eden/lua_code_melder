# What

(2024-11)

Command-line tool to aggregate all `*.lua` files in current directory
(and subdirectories) into one.

## Example

Okay guys, our use case is some multi-file Lua project. At the end
we want to see one Lua file which can be distributed independently.

* Actually I have some test "project" at [test_case](test_case).
  Won't lose time typing about it.
* ```
  $ lua_code_melder/test_case$ lua test.lua
  > [Test representation.]
  ```
  Okay, looks legit.
* I'll go straight to "correct" commands. I encourage you to experiment
  with more obvious ways and understand what they do. Note the directory
  names to reproduce it.
* Melding the melder
  ```
  $ lua_code_melder/src$ lua meld.lua meld > ../ingots/meld.lua
  ```
* Melding the test project
  ```
  $ lua_code_melder/test_case$ lua ../ingots/meld.lua test > ../ingots/test.lua
  ```
* Test run of melded test project
  ```
  lua_code_melder/ingots$ lua test.lua
  [Test representation.]
  ```
  Still looks legit.

## Now the fun part (extended example)

* Melding the melded melder and melded test project
  ```
  $ lua_code_melder/ingots$ lua meld.lua > ../alloy.lua
  ```
* Running "test" part of alloy
  ```
  $ lua_code_melder$ lua alloy.lua test
  > [Test representation.]
  ```
* Running "melder" part of alloy
  ```
  $ lua_code_melder$ lua alloy.lua meld
  > [... lots of Lua code lines ...]
  ```

## How it works?

Melder is a file scanner. It gets all `*.lua` files from current dir,
converts their file name to module name and loads its text (source code).

It generates Lua source code file and prints it to stdout.

It writes them to table
```Lua
local Modules = {
  <ModuleName> = ModuleCode
}
```

Then it compiles their code but do not run it. Compiled code is
stored in `_G.package.preload` table (see Lua documentation).
Hat tip from me to Lua authors for this nice design.

```Lua
local AddModule =
  function(Name, Code)
    local CompiledCode = assert(load(Code, Name, 't'))

    _G.package.preload[Name] =
      function(...)
        return CompiledCode(...)
      end
  end

for ModuleName, ModuleCode in pairs(Modules) do
  AddModule(ModuleName, ModuleCode)
end
```

And finally it adds activation line, which is a mere `require(<module_name>)`.

`<module_name>` is that additional command-line argument: `test` or `meld`
in our examples.

If there are no command-line argument, activation line is `require(arg [1])`.
Yep. Call any module from library from command-line.

Sapienti sat.

## Requirements

  * Lua 5.4
  * Linux
  * Binutils (`find`)

  As usually all needed guts from `[workshop]` are included in this repo.
  You can just copy [melder](ingots/meld.lua) and not clone repo at all.

## See also

* [My other repositories][contents]

[contents]: https://github.com/martin-eden/contents
