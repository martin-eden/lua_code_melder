-- Aggregate Lua source files in current directory to one

--[[
  Author: Martin Eden
  Last mod.: 2026-04-24
]]

-- [[ Release
require('workshop.base')
--]]
--[[ Develop
package.path = package.path .. ';../../?.lua'
require('workshop.base')
--]]

local Config =
  {
    ModulesDir = arg[1],
    RootModule = arg[2],
    DoIndent = (arg[3] == '--indent'),
  }

local usage_help = [[

Merge all .lua files under given directory into one executable code block
and print it.

Usage

  meld.lua <modules_dir> <root_module_name> [--indent]

Example

  $ lua meld.lua test_case/ test > ingots/test.lua

Parameters

  <modules_dir> -- Directory from which we search for .lua files

  <root_module_name> -- Name of the "main" module which is called
    in generated code block.

  --indent -- Indent code of embedded modules for nice output.

    Indenting is not safe for code! If source code has multi-line strings
    then spaces will be added to them.

    So if you can test/review result code -- use this option.

-- Martin, 2026-04
]]

local FilesLister = request('!.concepts.FilesLister.Interface')
local parse_path_name = request('!.concepts.path_name.parse')
local file_as_string = request('!.file_system.file.as_string')
local LinesClass = request('!.concepts.Lines.Interface')
local string_ends_with = request('!.string.ends_with')
local ordered_pairs = request('!.table.ordered_pass')
local lua_quote_string = request('!.concepts.lua.quote_string')

local is_lua_file =
  function(file_name)
    return string_ends_with(file_name, '.lua')
  end

-- Convert file name (without dir) to Lua's require() module name
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

local add_module_registration =
  function(Lines, module_name, module_code, do_indent)
    local quoted_module_name = lua_quote_string(module_name)

    if do_indent then
      local ModuleLines = new(LinesClass)
      ModuleLines:FromString(module_code)
      ModuleLines:Indent()
      ModuleLines:Indent()
      module_code = ModuleLines:ToString()
    end

    Lines:AddLastLine('_G.package.preload[' .. quoted_module_name .. '] =')
    Lines:AddLastLine('  function(...)')
    Lines:AddLastLine(module_code)
    Lines:AddLastLine('  end')
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

  local Modules = get_modules(Config.ModulesDir)

  local Lines = new(LinesClass)

  for module_name, module_code in ordered_pairs(Modules) do
    add_module_registration(Lines, module_name, module_code, Config.DoIndent)
    Lines:AddLastLine('')
  end

  add_module_call(Lines, Config.RootModule)

  io.write(Lines:ToString())
end

--[[
  2024-11-20
  2026-04-22
  2026-04-23
  2026-04-24
]]
