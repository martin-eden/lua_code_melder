-- Aggregate Lua source files in current directory into one

--[[
  Author: Martin Eden
  Last mod.: 2026-09-23
]]

--[[ Develop
package.path = package.path .. ';../../../?.lua'
--]]
require('workshop.base')

--[[
  Files visitor

  For each Lua file in current directory:

    * Gets it's name as module name (for require())
    * Gets it's contents
    * Adds to Result table: { module_name, file_contents }
]]
local get_modules
do
  local is_lua_file
  local get_module_name
  do
    local lua_extension = '.lua'
    local str_ends_with = request('!.string.ends_with')
    is_lua_file =
      function(file_name)
        return str_ends_with(file_name, lua_extension)
      end
    do
      local parse_pathname = request('!.concepts.path_name.pathname_from_str')
      local str_remove_prefix = request('!.string.remove_prefix')
      local str_remove_postfix = request('!.string.remove_postfix')
      local rebase_pathname = request('!.concepts.path_name.rebase_to')
      local module_name_delimiter = '.'
      local list_to_str = request('!.concepts.list.to_string')

      -- Convert path name to Lua's require() module name
      get_module_name =
        function(path_name, base_dir)
          path_name = str_remove_prefix(path_name, base_dir)
          path_name = str_remove_postfix(path_name, lua_extension)
          path_name = rebase_pathname('./', path_name)

          return
            list_to_str(parse_pathname(path_name), module_name_delimiter)
        end
    end
  end
  local file_to_str = request('!.convert.file_to_str')
  local add_to_list = request('!.concepts.list.add_item')

  local get_files_list =
    request('!.file_system.directory.get_total_files_list')

  get_modules =
    function(base_dir)
      local Files = get_files_list(base_dir)

      local Result = { }

      for _, path_name in ipairs(Files) do
        if not is_lua_file(path_name) then goto next end

        local module_name = get_module_name(path_name, base_dir)
        local module_code = file_to_str(path_name)

        add_to_list(Result, { module_name, module_code })

        :: next ::
      end

      return Result
    end
end

-- Main:
do
  local modules_dir = arg[1]
  local root_module = arg[2]

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

-- Martin, 2026-08
]]

  local emit
  do
    local newline
    do
      local AsciiChars = request('!.concepts.Ascii.Chars')
      newline = AsciiChars.newline
    end
    local string_ends_with = request('!.string.ends_with')
    emit =
      function(str)
        io.write(str)
        if not string_ends_with(str, newline) then
          io.write(newline)
        end
      end
  end

  local meld
  do
    local add_module_registration
    local add_module_call
    do
      do
        local lua_quote_string = request('!.concepts.lua.quote_string')
        add_module_registration =
          function(module_name, module_code)
            local quoted_module_name = lua_quote_string(module_name)

            emit('-- ( module ' .. module_name)
            emit('package.preload[' .. quoted_module_name .. '] =')
            emit('function(...)')
            emit(module_code)
            emit('end')
            emit('-- )')
            emit('')
          end
      end
      add_module_call =
        function(module_name)
          emit("return require('" ..module_name .. "')")
        end
    end
    meld =
      function(modules_dir, root_module)
        local Modules = get_modules(modules_dir)

        for _, Rec in ipairs(Modules) do
          local module_name = Rec[1]
          local module_code = Rec[2]

          add_module_registration(module_name, module_code)
        end

        add_module_call(root_module)
      end
  end

  if not (modules_dir and root_module) then
    emit(usage_help)

    return
  end

  meld(modules_dir, root_module)
end

--[[
  2024 #
  2026 # # # # # #
  2026-08-13
  2026-08-14
]]
