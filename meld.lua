-- Aggregate Lua source files in current directory to one

--[[
  Author: Martin Eden
  Last mod.: 2026-04-23
]]

-- [[ Release
require('workshop.base')
--]]
--[[ Develop
package.path = package.path .. ';../../?.lua'
require('workshop.base')
--]]

--[[
  To do

    * Unmeld

      Unmeld works with file, Meld works with directory

  Done

    * Become happy with used [workshop] code

    * snake_case by default

      CamelCase for tables and their fields

    * Ability to run from another directory

]]

local Config =
  {
    ModulesDir = arg[1],
    RootModule = arg[2],
  }

local usage_help = [[

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
]]

local FilesLister = request('!.concepts.FilesLister.Interface')
local parse_path_name = request('!.concepts.path_name.parse')
local file_as_string = request('!.file_system.file.as_string')
local table_to_string = request('!.table.as_string')
local Lines = request('!.concepts.Lines.Interface')
local string_starts_with = request('!.string.starts_with')
local string_ends_with = request('!.string.ends_with')

local is_lua_file =
  function(file_name)
    return string_ends_with(file_name, '.lua')
  end

-- Convert pathname to Lua's require() module name
local get_module_name =
  function(file_name)
    local module_name

    -- Module name is file name without ".lua" at end
    local module_name_capture = '(.*)%.lua$'
    module_name = string.match(file_name, module_name_capture)

    return module_name
  end

--[[
  Files visitor

  For each Lua file in current directory:

    * Gets it's name as module name (for require())
    * Gets it's contents
    * Adds to Result table: Result[module_name] = file_contents
]]
local populate_modules
populate_modules =
  function(base_dir_name, module_name_prefix, Result)
    FilesLister:SetBaseDirectory(base_dir_name)

    local Files = FilesLister:GetFiles()

    for _, file_name in ipairs(Files) do
      if is_lua_file(file_name) then
        local full_file_name = base_dir_name .. file_name
        local file_contents = file_as_string(full_file_name)
        local module_name = module_name_prefix .. get_module_name(file_name)
        Result[module_name] = file_contents
      end
    end

    local Directories = FilesLister:GetDirectories()

    for _, subdir_name in ipairs(Directories) do
      populate_modules(
        base_dir_name .. subdir_name,
        module_name_prefix .. string.sub(subdir_name, 1, -2) .. '.',
        Result
      )
    end
  end

local get_modules =
  function(start_dir)
    start_dir = parse_path_name(start_dir).FullName

    local Result = {}

    populate_modules(start_dir, '', Result)

    return Result
  end

-- Start first line in table definition string from "local Modules = " ..
local add_modules_table_prefix =
  function(Lines)
    local first_line = Lines:GetFirstLine()
    assert(string_starts_with(first_line, '{'))
    first_line = 'local Modules = ' .. first_line
    Lines:RemoveFirstLine()
    Lines:AddFirstLine(first_line)
  end

local add_modules_population_code =
  function(Lines)
    local code_str = [[
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
end]]
    Lines:AddLastLine(code_str)
  end

local add_module_call =
  function(Lines, module_name)
    local module_call_str

    local module_call_fmt = "require('%s')"
    module_call_str = string.format(module_call_fmt, module_name)

    Lines:AddLastLine(module_call_str)
  end

-- Main
do
  if not Config.ModulesDir then
    io.write(usage_help)
    return
  end

  Lines:FromString(table_to_string(get_modules(Config.ModulesDir)))

  add_modules_table_prefix(Lines)
  Lines:AddLastLine('')
  add_modules_population_code(Lines)
  Lines:AddLastLine('')
  add_module_call(Lines, Config.RootModule)

  io.write(Lines:ToString())
end

--[[
  2024-11-20
  2026-04-22
  2026-04-23
]]
