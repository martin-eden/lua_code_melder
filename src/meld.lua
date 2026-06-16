-- Aggregate Lua source files in current directory to one

--[[
  Author: Martin Eden
  Last mod.: 2026-06-16
]]

--[[ Develop
package.path = package.path .. ';../../../?.lua'
--]]
require('workshop.base')

local Config =
  {
    modules_dir = arg[1],
    root_module = arg[2],
  }

local usage_help = [[
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
]]

-- Imports:
local add_dir_postfix = request('!.concepts.path_name.add_dir_postfix')
local FilesLister = request('!.concepts.FilesLister.Interface')
local file_to_str = request('!.convert.file_to_str')
local add_to_list = request('!.concepts.list.add_item')
local string_ends_with = request('!.string.ends_with')
local lua_quote_string = request('!.concepts.lua.quote_string')

local is_lua_file =
  function(file_name)
    return string_ends_with(file_name, '.lua')
  end

-- Convert file name (without dir) to Lua's require() module name
local get_module_name =
  function(file_name)
    -- Module name is file name without ".lua" at end

    local module_name_capture = '(.*)%.lua$'

    return string.match(file_name, module_name_capture)
  end

--[[
  Files visitor

  For each Lua file in current directory:

    * Gets it's name as module name (for require())
    * Gets it's contents
    * Adds to Result table: { module_name, file_contents }
]]
local get_modules
get_modules =
  function(base_dir_name, module_name_prefix, Result)
    FilesLister:SetBaseDirectory(base_dir_name)

    local Files = FilesLister:GetFiles()

    for _, file_name in ipairs(Files) do
      if not is_lua_file(file_name) then goto next end

      local module_name = module_name_prefix .. get_module_name(file_name)
      local module_code = file_to_str(base_dir_name .. file_name)

      add_to_list(Result, { module_name, module_code })

      :: next ::
    end

    local Directories = FilesLister:GetDirectories()

    for _, subdir_name in ipairs(Directories) do
      get_modules(
        base_dir_name .. subdir_name .. '/',
        module_name_prefix .. subdir_name .. '.',
        Result
      )
    end
  end

local get_modules =
  function(start_dir)
    local Result = { }

    get_modules(start_dir, '', Result)

    return Result
  end

local emit =
  function(str)
    io.write(str)

    local newline = '\n'

    if not string_ends_with(str, newline) then
      io.write(newline)
    end
  end

local add_module_registration =
  function(module_name, module_code)
    local quoted_module_name = lua_quote_string(module_name)

    local module_id = '( module ' .. module_name .. ' )'

    emit('-- ( ' .. module_id)
    emit('_G.package.preload[' .. quoted_module_name .. '] =')
    emit('function(...)')
    emit(module_code)
    emit('end')
    emit('-- ) ' .. module_id)
    emit('')
  end

local add_module_call =
  function(module_name)
    emit("require('" ..module_name .. "')")
  end

local meld =
  function(modules_dir, root_module)
    local Modules = get_modules(modules_dir)

    for _, Rec in ipairs(Modules) do
      local module_name = Rec[1]
      local module_code = Rec[2]

      add_module_registration(module_name, module_code)
    end

    add_module_call(root_module)
  end

-- Main:
do
  local modules_dir = Config.modules_dir
  local root_module = Config.root_module

  if not modules_dir or not root_module then
    io.write(usage_help)

    return
  end

  modules_dir = add_dir_postfix(modules_dir)

  meld(modules_dir, root_module)
end

--[[
  2024-11-20
  2026-04 # # #
  2026-06-04
  2026-06-16
]]
