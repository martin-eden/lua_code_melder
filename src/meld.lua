-- Aggregate Lua source files in current directory into one

--[[
  Author: Martin Eden
  Last mod.: 2026-11-16
]]

require('workshop.base')

--[[
  files visitor

  For each Lua file in current directory:

    * Gets it's name as module name (for require())
    * Gets it's contents
    * Adds to result table: { module_name, file_contents }
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

  local get_files_list =
    request('!.file_system.directory.get_total_files_list')
  local add_to_list = request('!.concepts.list.add_item')

  get_modules =
    function(base_dir)
      local files = get_files_list(base_dir)
      local result = { }
      for _, path_name in ipairs(files) do
        if not is_lua_file(path_name) then goto next end
        local module_name = get_module_name(path_name, base_dir)
        add_to_list(result, { module_name, path_name })
        :: next ::
      end
      return result
    end
end

-- Main:
do
  local modules_dir = arg[1]
  local root_module = arg[2]
  local output_file_name = arg[3]

  local usage_help = [[
Merge all .lua files in given directory into one Lua file.

Usage

  meld <modules_dir> <root_module_name> <output_file>

Example

  $ meld test_case/ test ingots/test.lua

Parameters

  <modules_dir> -- directory where we search for Lua files
  <root_module_name> -- name of the "main" module to call
  <output_file> -- output file name

-- Martin, 2026-11
]]

  local add_module_call =
    function(module_name, output)
      output:write("return require('" ..module_name .. "')" .. '\n')
    end

  if not (modules_dir and root_module and output_file_name) then
    io.write(usage_help)
    return
  end

  local modules = get_modules(modules_dir)
  local output_file = request('!.concepts.streams.output.file')
  output_file = output_file.create(output_file_name)
  local meld = request('!.mechs.meld')
  output_file:open()
  meld(modules, output_file)
  add_module_call(root_module, output_file)
  output_file:close()
end

--[[
  2024 #
  2026 # # # # # # # #
  2026-11-16
]]
