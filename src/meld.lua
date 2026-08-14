-- Aggregate Lua source files in current directory into one

--[[
  Author: Martin Eden
  Last mod.: 2026-08-14
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
local get_modules_root
do
  local FilesLister = request('!.concepts.FilesLister')
  local Result
  local add_separator = request('!.concepts.path_name.add_separator')
  local get_modules
  do
    local is_lua_file
    local get_module_name
    do
      local lua_extension = '.lua'
      do
        local string_ends_with = request('!.string.ends_with')
        is_lua_file =
          function(file_name)
            return string_ends_with(file_name, lua_extension)
          end
      end
      do
        local module_name_capture
        do
          local quote_regexp = request('!.lua.regexp.quote')
          -- Module name is file name without ".lua" at end
          module_name_capture = '(.*)' .. quote_regexp(lua_extension) .. '$'
        end
        local str_match = string.match
        -- Convert file name (without dir) to Lua's require() module name
        get_module_name =
          function(file_name)
            return str_match(file_name, module_name_capture)
          end
      end
    end
    local file_to_str = request('!.convert.file_to_str')
    local add_to_list = request('!.concepts.list.add_item')
    local pathname_sep
    do
      local PathEls = request('!.concepts.path_name.Syntels')
      pathname_sep = PathEls.separator
    end
    local module_name_sep = '.'
    get_modules =
      function(base_dir_name, module_name_prefix)
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
            base_dir_name .. subdir_name .. pathname_sep,
            module_name_prefix .. subdir_name .. module_name_sep
          )
        end
      end
  end

  get_modules_root =
    function(start_dir)
      FilesLister = FilesLister.create()
      Result = { }

      get_modules(add_separator(start_dir), '')

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

            local prefix_cmt
            local postfix_cmt
            do
              local module_id = '( module ' .. module_name .. ' )'
              prefix_cmt = '-- ( ' .. module_id
              postfix_cmt = '-- )'
            end

            emit(prefix_cmt)
            emit('package.preload[' .. quoted_module_name .. '] =')
            emit('function(...)')
            emit(module_code)
            emit('end')
            emit(postfix_cmt)
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
        local Modules = get_modules_root(modules_dir)

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
