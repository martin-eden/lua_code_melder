local Modules = {
  meld = [=[
-- Aggregate Lua source files to one

--[[
  Author: Martin Eden
  License: LGPL v3
  Last mod.: 2024-11-21
]]

require('workshop.base')

local FileLister = request('!.mechs.file_lister.interface')
local TableToCodeStr = request('!.table.as_string')
local FileAsString = request('!.file_system.file.as_string')
local Lines = request('!.concepts.Lines.Interface')
local StringToLines = request('!.string.to_lines')
local ParsePathName = request('!.concepts.path_name.parse')

local LooksLikeModule =
  function(FileName)
    local LuaFileRegexp = '%.lua$'
    if string.find(FileName, LuaFileRegexp) then
      return true
    end
    return false
  end

local GetModuleName =
  function(PathName)
    local ParsedName = ParsePathName(PathName)
    local DirName = ParsedName.directory
    local FileName = ParsedName.name
    assert(not ParsedName.is_directory)

    local ModuleName
    -- Module name is file name without ".lua" at end
    local ModuleNameRegexp = '(.*)%.lua$'
    ModuleName = string.match(FileName, ModuleNameRegexp)

    -- if we are in directory then
    if (DirName ~= '') then
      -- Module name is prefixed with directory path with "/" replaced to "."
      local ModuleNamePrefix = string.gsub(DirName, '/', '.')
      ModuleName = ModuleNamePrefix .. ModuleName
    end

    return ModuleName
  end

local PopulateModules
PopulateModules =
  function(DirName, Result)
    FileLister.start_dir = DirName
    FileLister:init()
    local Directories = FileLister:get_directories_list()
    local Files = FileLister:get_files_list()

    for Index, FileName in ipairs(Files) do
      if LooksLikeModule(FileName) then
        local FullFileName = DirName .. '/' .. FileName
        local ModuleCode = FileAsString(FullFileName)
        local ModuleName = GetModuleName(FullFileName)
        Result[ModuleName] = ModuleCode
      end
    end

    for Index, Directory in ipairs(Directories) do
      PopulateModules(DirName .. '/' .. Directory, Result)
    end
  end

local GetModules =
  function()
    local Result = {}
    PopulateModules('.', Result)
    return Result
  end

local Modules = GetModules()
local ModulesTableStr = TableToCodeStr(Modules)
Lines:FromString(ModulesTableStr)

local AddModulesTablePrefix =
  function(Lines)
    local FirstLine = Lines:GetFirstLine()
    assert(FirstLine:sub(1, 1) == '{')
    FirstLine = 'local Modules = ' .. FirstLine
    Lines:RemoveLineAt(1)
    Lines:AddFirstLine(FirstLine)
  end

local AddModulesPopulationCode =
  function(Lines)
    local PopulationCode = [[
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
end]]
    Lines:AddLastLine(PopulationCode)
  end

local AddModuleCall =
  function(ModuleName)
    local ModuleCallFmt = "require('%s')"
    local ModuleCallStr = string.format(ModuleCallFmt, ModuleName)
    Lines:AddLastLine(ModuleCallStr)
  end

AddModulesTablePrefix(Lines)
Lines:AddLastLine('')
AddModulesPopulationCode(Lines)
Lines:AddLastLine('')
if arg[1] then
  AddModuleCall(arg[1])
else
  Lines:AddLastLine('require(arg[1])')
end

io.write(Lines:ToString())

--[[
  2024-11-20
]]
]=],
  ['workshop.base'] = [=[
--[[
  Lua base libraries extension. Used almost in any piece of my code.

  This module installs global function "request" which is based on
  "require" and makes possible relative module names.

  Also this function tracks module dependencies. This allows to
  get dependencies list for any module. Which is used to create
  deploys without unused code.

  Price for tracking dependencies  is global table "dependencies"
  and function "get_require_name".

  Lastly, global functions are added for convenience. Such functions
  are "new" and families of "is_<type>" and "assert_<type>".
]]

-- Last mod.: 2024-03-02

-- Export request function:
local split_name =
  function(qualified_name)
    local prefix_name_pattern = '^(.+%.)([^%.]+)$'  -- a.b.c --> (a.b.) (c)
    local prefix, name = qualified_name:match(prefix_name_pattern)
    if not prefix then
      prefix = ''
      name = qualified_name
      if not name:find('^([^%.]+)$') then
        name = ''
      end
    end
    return prefix, name
  end

local unite_prefixes =
  function(base_prefix, rel_prefix)
    local init_base_prefix, init_rel_prefix = base_prefix, rel_prefix
    local list_without_tail_pattern = '(.+%.)[^%.]-%.$' -- a.b.c. --> (a.b.)
    local list_without_head_pattern = '[^%.]+%.(.+)$' -- a.b.c. --> (b.c.)
    while rel_prefix:find('^%^%.') do
      if (base_prefix == '') then
        error(
          ([[Link "%s" is outside caller's prefix "%s".]]):format(
            init_rel_prefix,
            init_base_prefix
          )
        )
      end
      base_prefix = base_prefix:match(list_without_tail_pattern) or ''
      rel_prefix = rel_prefix:match(list_without_head_pattern) or ''
    end
    return base_prefix .. rel_prefix
  end

local names = {}
local depth = 1

local get_caller_prefix =
  function()
    local result = ''
    if names[depth] then
      result = names[depth].prefix
    end
    return result
  end

local get_caller_name =
  function()
    local result = 'anonymous'
    if names[depth] then
      result = names[depth].prefix .. names[depth].name
    end
    return result
  end

local push =
  function(prefix, name)
    depth = depth + 1
    names[depth] = {prefix = prefix, name = name}
  end

local pop =
  function()
    depth = depth - 1
  end

local dependencies = {}
local add_dependency =
  function(src_name, dest_name)
    dependencies[src_name] = dependencies[src_name] or {}
    dependencies[src_name][dest_name] = true
  end

local base_prefix = split_name((...))

local get_require_name =
  function(qualified_name)
    local is_absolute_name = (qualified_name:sub(1, 2) == '!.')
    if is_absolute_name then
      qualified_name = qualified_name:sub(3)
    end
    local prefix, name = split_name(qualified_name)
    local caller_prefix =
      is_absolute_name and base_prefix or get_caller_prefix()
    prefix = unite_prefixes(caller_prefix, prefix)
    return prefix .. name, prefix, name
  end

local request =
  function(qualified_name)
    local src_name = get_caller_name()

    local require_name, prefix, name = get_require_name(qualified_name)

    push(prefix, name)
    local dest_name = get_caller_name()
    add_dependency(src_name, dest_name)
    local results = table.pack(require(require_name))
    pop()

    return table.unpack(results)
  end

local IsFirstRun = (_G.request == nil)

if IsFirstRun then
  _G.request = request
  _G.dependencies = dependencies
  _G.get_require_name = get_require_name

  --[[
    At this point we installed "request()", so it's usable from
    outer code.

    Below we call optional modules which install additional
    global functions.

    Functions made global because they are widely used in my code.

    They are inside other files. We use freshly added "request()"
    to load them and add them to dependencies of this module.

    We need add record to call stack with our name because these
    calls of "request()" are inside "if", so the call will not be
    done until actual execution.
  ]]

  -- First element is invocation module name, second - module file path
  local our_require_name = (...)

  push('', our_require_name)

  request('!.system.install_is_functions')()
  request('!.system.install_assert_functions')()
  _G.new = request('!.table.new')

  pop()
end

--[[
  2016-06
  2017-09
  2018-02
  2018-05
  2024-03
]]
]=],
  ['workshop.concepts.Counter.Interface'] = [=[
-- Counter class

return
  {
    -- Interface

    -- Initialization setup
    Init =
      function(self, StartValue)
        assert_integer(self.MinValue)
        assert_integer(self.MaxValue)
        assert(self.MinValue <= self.MaxValue)

        if is_nil(StartValue) then
          if is_nil(self.Value) then
            -- Fallback to min value
            self.Value = self.MinValue
          end
        else
          assert_integer(StartValue)
          if
            (StartValue < self.MinValue) or
            (StartValue > self.MaxValue)
          then
            local ErrorMsgFmt =
              '[Counter] Starting value out of range: (%d not in [%d, %d]).'
            local ErrorMsg =
              string.format(
                ErrorMsgFmt,
                StartValue,
                self.MinValue,
                self.MaxValue
              )
            error(ErrorMsg)
          end
          self.Value = StartValue
        end
      end,

    -- Return current value
    Get =
      function(self)
        return self.Value
      end,

    -- Move one unit forward
    Increase =
      function(self)
        if (self.Value >= self.MaxValue) then
          if (self.Value > self.MaxValue) then
            self.Value = self.MaxValue
          end
          return false
        end
        self.Value = self.Value + 1
        return true
      end,

    -- Move one unit backward
    Decrease =
      function(self)
        if (self.Value <= self.MinValue) then
          if (self.Value < self.MinValue) then
            self.Value = self.MinValue
          end
          return false
        end
        self.Value = self.Value - 1
        return true
      end,

    -- Intensities

    -- Current value
    Value = nil,

    -- Range min value
    MinValue = 0,

    -- Range max value
    MaxValue = 100,
  }

--[[
  2024-08-31
]]
]=],
  ['workshop.concepts.Indent.Interface'] = [=[
return
  {
    -- Interface

    -- Initialization setup
    Init =
      function(self, IndentValue, ChunkValue)
        -- Tune counter for our needs
        self.Counter.MinValue = 0
        self.Counter.MaxValue = 1000
        self.Counter:Init(IndentValue)

        -- Set chunk if passed
        if not is_nil(ChunkValue) then
          assert_string(ChunkValue)
          self.Chunk = ChunkValue
        end
      end,

    -- Increase indent
    Increase =
      function(self)
        return self.Counter:Increase()
      end,

    -- Decrease indent
    Decrease =
      function(self)
        return self.Counter:Decrease()
      end,

    -- Return current indent value (integer)
    GetDepth =
      function(self)
        return self.Counter:Get()
      end,

    -- Return current indent string
    GetString =
      function(self)
        return string.rep(self.Chunk, self:GetDepth())
      end,

    -- Intensities

    -- Indent counter
    Counter = request('!.concepts.Counter.Interface'),

    -- Indent chunk
    Chunk = '  ',
  }

--[[
  2024-08-31
]]
]=],
  ['workshop.concepts.Lines.AddFirstLine'] = [=[
-- Add string to start of list

-- Last mod.: 2024-10-31

-- Exports:
return
  function(self, String)
    self:InsertLineBefore(String, 1)
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.AddLastLine'] = [=[
-- Add string to end of list

-- Last mod.: 2024-10-31

-- Exports:
return
  function(self, String)
    self:InsertLineAfter(String, self:GetNumLines())
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.AssertValidIndex'] = [=[
-- Assert that value is valid index

-- Last mod.: 2024-11-01

-- Exports:
return
  function(self, Index)
    assert_integer(Index)

    local NumLines = self:GetNumLines()
    local IsEmpty = (NumLines == 0)

    if IsEmpty then
      assert((Index == 0) or (Index == 1))
    else
      assert(Index >= 1)
      assert(Index <= NumLines)
    end
  end

--[[
  2024-10-31
  2024-11-01
]]
]=],
  ['workshop.concepts.Lines.AssertValidValue'] = [=[
-- Assert that value is string

-- Last mod.: 2024-11-20

-- Exports:
return
  function(self, String)
    assert_string(String)
  end

--[[
  2024-10-31
  2024-11-20
]]
]=],
  ['workshop.concepts.Lines.FromString'] = [=[
-- Explode string to list of lines

-- Last mod.: 2024-10-31

-- Imports:
local StringToLines = request('!.string.to_lines')

-- Exports:
return
  function(self, String)
    self.Lines = StringToLines(String)
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.GetFirstLine'] = [=[
-- Return string from first line

-- Last mod.: 2024-10-31

-- Exports:
return
  function(self)
    return self:GetLineAt(1)
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.GetLastLine'] = [=[
-- Return string from last line

-- Last mod.: 2024-10-31

-- Exports:
return
  function(self)
    return self:GetLineAt(self:GetNumLines())
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.GetLineAt'] = [=[
-- Return line's contents by index

-- Last mod.: 2024-10-31

--[[
  Same note as for [GetNumLines]. We're using trivial wrappers for
  freedom for changes.
]]

-- Exports:
return
  function(self, Index)
    self:AssertValidIndex(Index)

    return self.Lines[Index]
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.GetNumLines'] = [=[
-- Return number of lines

-- Last mod.: 2024-10-31

--[[
  It may seem stupid..

  We don't use private fields. We store lines at <.Lines>.
  You can just #<.Lines> to get length.

  But we want freedom to rename our "Lines" field. Or even change it
  to some object. So good code is:

    Lines:FromString('123\n456')
    if Lines:IsOneLine() then
      io.write('( ', Lines:GetFirstLine(), ' )\n')
    else
      Lines:Indent()
      Lines:AddStringToStart('(')
      Lines:AddStringToEnd(')')
      io.write(Lines:ToString())
    end
]]

-- Exports:
return
  function(self)
    return #self.Lines
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.Indent'] = [=[
-- Indent lines

-- Last mod.: 2024-10-31

--[[
  In our class we're not separating indentation from lines text.
  So we'll modify lines text.
]]

-- Exports:
return
  function(self)
    local IndentChunk = self.IndentChunk

    for Index, Line in ipairs(self.Lines) do
      self.Lines[Index] = IndentChunk .. Line
    end
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.InsertLineAfter'] = [=[
-- Insert string after line at given index

-- Last mod.: 2024-10-31

-- Exports:
return
  function(self, String, Index)
    --[[
      We can't hire InsertLineBefore() to do this job for us
      because <Index> needs to be (<Index> + 1) and it won't
      be valid.
    ]]

    self:AssertValidValue(String)
    self:AssertValidIndex(Index)

    table.insert(self.Lines, Index + 1, String)
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.InsertLineBefore'] = [=[
-- Insert string before line at given index

-- Last mod.: 2024-10-31

--[[
  Method is named "Insert Line Before", but requires string?

  Yes, not "Insert String Before". Because we're adding line
  (string with newline). Argument is string, yes. But it can
  be whatever type we want.

  We're naming function by what it does, not by what arguments
  it accepts.
]]

-- Exports:
return
  function(self, String, Index)
    self:AssertValidValue(String)
    self:AssertValidIndex(Index)

    table.insert(self.Lines, Index, String)
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.Interface'] = [=[
-- List of lines interface

-- Last mod.: 2024-10-31

-- "Line" is a string with newline. "String" is a line without newline.

--[[
  This class is about storing list of lines.

  Lines are stored as strings (without newlines).

  Text output facilities work with lines, not with multiline
  strings. Among other things they want indentation. We're
  providing indentation. They want add header or footer. We're
  providing that.
]]

-- Exports:
return
  {
    -- Import/export:

    -- Import: Explode string to list of lines
    FromString = request('FromString'),

    -- Export: Implode list of lines to string
    ToString = request('ToString'),

    -- Access:

    -- Meta: get number lines
    GetNumLines = request('GetNumLines'),

    -- MetaMeta: is one line?
    IsOneLine = request('IsOneLine'),

    -- Access: get string at given line
    GetLineAt = request('GetLineAt'),

    -- Access: set string at given line
    SetLineAt = request('SetLineAt'),

    -- Meta: get first line string
    GetFirstLine = request('GetFirstLine'),

    -- Meta: get last line string
    GetLastLine = request('GetLastLine'),

    -- Modification:

    -- Add string to start of list
    AddFirstLine = request('AddFirstLine'),

    -- Add string to end of list
    AddLastLine = request('AddLastLine'),

    -- Insert string before line at given index
    InsertLineBefore = request('InsertLineBefore'),

    -- Insert string after line at given index
    InsertLineAfter = request('InsertLineAfter'),

    -- Remove line
    RemoveLineAt = request('RemoveLineAt'),

    -- Indentation:

    -- Indent
    Indent = request('Indent'),

    -- Unindent
    Unindent = request('Unindent'),

    -- Internals:

    -- Internal: list of lines
    Lines = {},

    -- Config: indent chunk
    IndentChunk = '  ',

    -- Internal: assert that value is valid index
    AssertValidIndex = request('AssertValidIndex'),

    -- Internal: assert that value is string without newlines
    AssertValidValue = request('AssertValidValue'),
  }

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.IsOneLine'] = [=[
-- Return true when we have just one line

-- Last mod.: 2024-10-31

-- Exports:
return
  function(self)
    return (self:GetNumLines() == 1)
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.RemoveLineAt'] = [=[
-- Remove line from list

-- Last mod.: 2024-10-31

-- Exports:
return
  function(self, Index)
    self:AssertValidIndex(Index)

    table.remove(self.Lines, Index)
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.SetLineAt'] = [=[
-- Set line's contents

-- Last mod.: 2024-10-31

-- Exports:
return
  function(self, String, Index)
    self:AssertValidValue(String)
    self:AssertValidIndex(Index)

    self.Lines[Index] = String
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.ToString'] = [=[
-- Implode list of lines to string

-- Last mod.: 2024-10-31

-- Imports:
local LinesToString = request('!.string.from_lines')

-- Exports:
return
  function(self)
    return LinesToString(self.Lines)
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.Lines.Unindent'] = [=[
-- Unindent lines

-- Last mod.: 2024-10-31

--[[
  Just removes indentation chunk from every line. If it can.
]]

-- Exports:
return
  function(self)
    local IndentChunk = self.IndentChunk
    local IndentChunkLen = string.len(IndentChunk)

    for Index, Line in ipairs(self.Lines) do
      local LinePrefix = string.sub(Line, 1, IndentChunkLen)
      if (LinePrefix == IndentChunk) then
        local LinePostfix = string.sub(Line, IndentChunkLen + 1)
        self.Lines[Index] = LinePostfix
      end
    end
  end

--[[
  2024-10-31
]]
]=],
  ['workshop.concepts.List.ToString'] = [=[
-- Concatenate list of string values to string

-- Last mod.: 2024-10-20

return
  function(List, Separator)
    Separator = Separator or ''

    -- Meh, in Lua it's simple
    return table.concat(List, Separator)
  end

--[[
  2024-10-20
  2024-10-24
]]
]=],
  ['workshop.concepts.lua.is_identifier'] = [[
local keywords = request('keywords')

return
  function(s)
    return
      is_string(s) and
      s:match('^[%a_][%w_]*$') and
      not keywords[s]
  end
]],
  ['workshop.concepts.lua.keywords'] = [[
local map_values = request('!.table.map_values')

return
  map_values(
    {
      'nil',
      'true',
      'false',
      'not',
      'and',
      'or',
      'do',
      'end',
      'local',
      'function',
      'goto',
      'if',
      'then',
      'elseif',
      'else',
      'while',
      'repeat',
      'until',
      'for',
      'in',
      'break',
      'return',
    }
  )
]],
  ['workshop.concepts.lua.quote_string'] = [=[
local quote_escaped = request('quote_string.linear')
local quote_long = request('quote_string.intact')
local quote_dump = request('quote_string.dump')

local content_funcs = request('!.string.content_attributes')
local has_control_chars = content_funcs.has_control_chars
local has_backslashes = content_funcs.has_backslashes
local has_single_quotes = content_funcs.has_single_quotes
local has_double_quotes = content_funcs.has_double_quotes
local is_nonascii = content_funcs.is_nonascii
local has_newlines = content_funcs.has_newlines

local binary_entities_lengths =
  {
    [1] = true,
    [2] = true,
    [4] = true,
    [6] = true,
    [8] = true,
    [10] = true,
    [16] = true,
  }

return
  function(s)
    assert_string(s)

    local quote_func = quote_escaped

    if binary_entities_lengths[#s] and is_nonascii(s) then
      quote_func = quote_dump
    elseif
      has_backslashes(s) or
      has_newlines(s) or
      (
        has_single_quotes(s) and has_double_quotes(s)
      )
    then
      quote_func = quote_long
    end

    local result = quote_func(s)
    return result
  end

--[[
  2016-09
  2017-08
  2024-11
]]
]=],
  ['workshop.concepts.lua.quote_string.custom_quotes'] = [=[
return
  {
    ['\x07'] = [[\a]],
    ['\x08'] = [[\b]],
    ['\x09'] = [[\t]],
    ['\x0a'] = [[\n]],
    ['\x0b'] = [[\v]],
    ['\x0c'] = [[\f]],
    ['\x0d'] = [[\r]],
    ['"'] = [[\"]],
    ["'"] = [[\']],
    ['\\'] = [[\\]],
  }
]=],
  ['workshop.concepts.lua.quote_string.dump'] = [=[
--[[
  Quote given string as by substituting all characters to their
  hex values.

  Handy for representing binary numbers:
    '\xB2\x7F\x02\xEE' has more sense than '²\x7F\x02î'
]]

local quote_char = request('quote_char')

return
  function(s)
    assert_string(s)
    return "'" .. s:gsub('.', quote_char) .. "'"
  end
]=],
  ['workshop.concepts.lua.quote_string.intact'] = [====[
-- Quote string in long quotes

-- Last mod.: 2024-11-19

--[==[
  Long quotes

  Long quotes is multi-character quotes in Lua. String data inside them
  are not processed. So no need to worry about quoting some "special"
  characters.

  That's all the same:

    > s = [[
    Hello!]]
    > s = [[Hello!]]
    > s = [=[Hello!]=]
]==]

local has_newlines = request('!.string.content_attributes').has_newlines

return
  function(s)
    assert_string(s)

    -- (1)
    s = s .. ']'

    local eq_chunk = ''
    local postfix
    while true do
      postfix = ']' .. eq_chunk .. ']'
      if not s:find(postfix, 1, true) then
        break
      end
      eq_chunk = eq_chunk .. '='
    end

    local prefix = '[' .. eq_chunk .. '['

    -- (2)
    local first_char = s:sub(1, 1)
    if
      (first_char == '\x0D') or
      (first_char == '\x0A')
    then
      prefix = prefix .. first_char
    end

    -- (3)
    if has_newlines(s) then
      prefix = prefix .. '\x0A'
    end

    return prefix .. s .. eq_chunk .. ']'
  end

--[===[
  [1] Quoted result string will have following structure:

    "[" "="^N "[" ["\n"] s "]" "="^N "]"

    We may safely concatenate "]" to <s> before determining <N>.
    This is done to avoid following cases:

      s     | unpatched   |  patched
      ------+-------------+--------------
      "]"   | "[[]]]"     | "[=[]]=]"
      "]]=" | "[=[]]=]=]" | "[==[]]=]==]"

    Case pointed by Roberto Ierusalimschy 2018-12-14 in Lua Mail
    List.

  [2] Heading newline dropped from long string quote. So we need
    duplicate it to preserve second copy. Before it all variants
    of newlines are converted to 0x0A.

    Tricky case here is if we have 0x0D we cant add 0x0A or it still
    be interpreted as one newline.

    Nice workaround is duplicate first byte of newline:

      0D    | 0D 0D    | \
      0A    | 0A 0A    |   still two line
      0D 0A | 0D 0D 0A |   delimiters!
      0A 0D | 0A 0A 0D | /

    Case and solution pointed by Andrew Gierth 2018-12-15 in Lua
    Mail List.

  [3]
    If string is multiline like "Hey\n  buddy!\n" then we want to
    represent it as
      > [[
      > Hey
      >   buddy!
      > ]]

    Not as
      > [[Hey
      >   buddy!
      > ]]

    For the sake of readability.
  ]=]

]===]

--[[
  2017-03
  2018-12
  2024-11
]]
]====],
  ['workshop.concepts.lua.quote_string.linear'] = [=[
local quote_char = request('quote_char')
local custom_quotes = request('custom_quotes')

--[[
  [1]
    I do not want to remember <custom_quotes> mapping (to understand
    that "\f" in output means ASCII code 0x0C. Also I do not like when
    "\" maps to "\\", I prefer "\x5c". Without using <custom_quote>
    table you get longer but easier to understand data representation.
]]
return
  function(s)
    local result = s
    --(1)
    -- result = result:gsub([[\]], custom_quotes['\\'])
    result = result:gsub([[\]], quote_char)
    -- result = result:gsub('[%c]', custom_quotes)
    result = result:gsub('[%c]', quote_char)
    --/

    local cnt_q1 = 0
    for i in result:gmatch("'") do
      cnt_q1 = cnt_q1 + 1
    end
    local cnt_q2 = 0
    for i in result:gmatch('"') do
      cnt_q2 = cnt_q2 + 1
    end
    if (cnt_q1 <= cnt_q2) then
      result = "'" .. result:gsub("'", custom_quotes["'"]) .. "'"
    else
      result = '"' .. result:gsub('"', custom_quotes['"']) .. '"'
    end
    return result
  end
]=],
  ['workshop.concepts.lua.quote_string.quote_char'] = [=[
--[[
  Return string with escaped hex byte for given string with byte.

  So for ' ' it will return '\x20'.

  This function intended for use as gsub() match handler.
]]

return
  function(c)
    return ([[\x%02X]]):format(c:byte(1, 1))
  end
]=],
  ['workshop.concepts.lua_table.save'] = [[
-- Serialize lua table as string with lua table definition.

-- Not suitable for tables with cross-links in keys or values.

local c_table_serializer = request('save.interface')
local compile = request('!.struc.compile')

return
  function(t, options)
    assert_table(t)
    local table_serializer = new(c_table_serializer, options)
    table_serializer:init()
    local ast = table_serializer:get_ast(t)
    local result = table_serializer:serialize_ast(ast)
    return result
  end
]],
  ['workshop.concepts.lua_table.save.get_ast'] = [=[
-- Provide data tree for Lua table without cycles (for tree)

--[[
  /*
    That's a pseudocode. Implementation uses lowercase field names.
  */

  MakeTree(Data)
  ~~~~~~~~~~~~~~
    if not IsTable(Data)
      if
        @OnlyRestorableItems and
        (TypeOf(Data) not in @RestorableTypes)

        return

      Result.Type = TypeOf(Data)
      Result.Value = Data
      return

    if @NamedValues[Data]
      Result.Type = "name"
      Result.Value = Name of node. Used in cycled table serializer.
      return

    AssertTable(Data)

    Result.Type = "table"
    for (Key, Val) in Data
      Result[i] = { Key = MakeTree(Key), Value = MakeTree(Val) }
]]

local RestorableTypes =
  {
    ['boolean'] = true,
    ['number'] = true,
    ['string'] = true,
    ['table'] = true,
  }

return
  function(self, data)
    local result
    local data_type = type(data)
    if (data_type == 'table') then
      if self.value_names[data] then
        result =
          {
            type = 'name',
            value = self.value_names[data],
          }
      else
        result = {}
        result.type = 'table'
        for key, value in self.table_iterator(data) do
          if (
            self.OnlyRestorableItems and
              (
                not RestorableTypes[type(key)] or
                not RestorableTypes[type(value)]
              )
            )
          then
            goto next
          end
          local key_slot = self:get_ast(key)
          local value_slot = self:get_ast(value)
          result[#result + 1] =
            {
              key = key_slot,
              value = value_slot,
            }
        ::next::
        end
      end
    else
      result =
        {
          type = data_type,
          value = data,
        }
    end
    return result
  end

--[[
  2018-02
  2020-09
  2022-01
  2024-08
]]
]=],
  ['workshop.concepts.lua_table.save.init'] = [[
return
  function(self)
    self.text_block = new(self.c_text_block)
    self.text_block:init()
    self.install_node_handlers(self.node_handlers, self.text_block)
  end
]],
  ['workshop.concepts.lua_table.save.install_node_handlers.minimal'] = [=[
-- Minificating table serializer methods

-- Last mod.: 2024-11-11

local text_block

local add =
  function(s)
    text_block:add_curline(s)
  end

local node_handlers = {}

local raw_compile = request('!.struc.compile')

local compile =
  function(t)
    add(raw_compile(t, node_handlers))
  end

local is_identifier = request('!.concepts.lua.is_identifier')
local compact_sequences = true

node_handlers.table =
  function(node)
    if (#node == 0) then
      add('{}')
      return
    end
    local last_integer_key = 0
    add('{')
    for i = 1, #node do
      if (i > 1) then
        add(',')
      end
      local key, value = node[i].key, node[i].value
      -- skip key part?
      if
        compact_sequences and
        (key.type == 'number') and
        is_integer(key.value) and
        (key.value == last_integer_key + 1)
      then
        last_integer_key = key.value
      else
        -- may mention key without brackets?
        if
          (key.type == 'string') and
          is_identifier(key.value)
        then
          add(key.value)
        else
          add('[')
          compile(key)
          add(']')
        end
        add('=')
      end
      compile(value)
    end
    add('}')
  end

do
  local serialize_tostring =
    function(node)
      add(tostring(node.value))
    end

  node_handlers.boolean = serialize_tostring
  node_handlers['nil'] = serialize_tostring
end

--[[
  tostring() of 1/0, 2/0, ... yields unloadable "inf".
  (-1/0, -2/0, ...) -> "-inf". (0/0, -0/0, -0/-0.0, ...) -> "-nan".
  We can compare "inf" values (1/0 == 2/0 -> true) but
  can't compare "nan" values (0/0 == 0/0 -> false).
  For "inf" cases we emit loadable "1/0" or "-1/0".
]]
node_handlers.number =
  function(node)
    if (node.value == 1/0) then
      add('1/0')
    elseif (node.value == -1/0) then
      add('-1/0')
    else
      add(tostring(node.value))
    end
  end

do
  local quote = request('!.lua.string.quote')

  local serialize_quoted =
    function(node)
      local quoted_string = quote(tostring(node.value))
      -- Quite ugly handling indexing [[[s]]] case: convert to [ [[s]]]
      if not text_block:on_clean_line() then
        local text_line = text_block.line_with_text:get_line()
        if
          (text_line:sub(-1) == '[') and
          (quoted_string:sub(1, 1) == '[')
        then
          add(' ')
        end
      end
      add(quoted_string)
    end

  local quoted_datatypes = {'string', 'function', 'thread', 'userdata'}

  for i = 1, #quoted_datatypes do
    node_handlers[quoted_datatypes[i]] = serialize_quoted
  end
end

node_handlers.name =
  function(node)
    compile(node.value)
  end

local force_merge = request('!.table.merge_and_patch')

return
  function(a_node_handlers, a_text_block, options)
    node_handlers = force_merge(a_node_handlers, node_handlers)
    text_block = a_text_block
    if options and is_boolean(options.compact_sequences) then
      compact_sequences = options.compact_sequences
    end
  end

--[[
  2017-05
  2019-06
]]
]=],
  ['workshop.concepts.lua_table.save.install_node_handlers.readable'] = [=[
-- Implementation of "readable" Lua table serialization

-- Last mod.: 2024-11-11

local RawCompile = request('!.struc.compile')
local IsName = request('!.concepts.lua.is_identifier')

local Handlers = {}

-- State (
-- Virtual printer interface
local Printer
-- Do not emit integer indices when possible
local CompactSequences = true
-- )

-- Mostly aliasing printers methods (
local GoToEmptyLine =
  function()
    Printer:request_clean_line()
  end

local Indent =
  function()
    Printer:inc_indent()
  end

local Unindent =
  function()
    Printer:dec_indent()
  end

local Emit =
  function(s)
    Printer:add_curline(s)
  end
-- )

local Compile =
  function(Tree)
    Emit(RawCompile(Tree, Handlers))
  end

Handlers.table =
  function(Node)
    -- Shortcut: empty table
    if (#Node == 0) then
      Emit('{}')
      return
    end

    --[[
      One-element table

      We'll put it on one line and wont write trailing delimiter.
    ]]
    local TheOneAndOnly = (#Node == 1)

    -- Array part tracking for <CompactSequences>
    local LastIntKey = 0

    Emit('{')
    Indent()

    for Idx, El in ipairs(Node) do
      local Key, Value = El.key, El.value

      if not TheOneAndOnly then
        GoToEmptyLine()
      end

      --[[
        if CompactSequences
          Do not emit integer index while we are on array part
      ]]
      local IsOnArray =
        is_integer(Key.value) and (Key.value == LastIntKey + 1)

      if CompactSequences and IsOnArray then
        LastIntKey = Key.value
      else
        -- No brackets required for identifiers
        if IsName(Key.value) then
          Emit(Key.value)
        else
          Emit('[')
          Compile(Key)
          Emit(']')
        end
        Emit(' = ')
      end

      Compile(Value)

      if not TheOneAndOnly then
        Emit(',')
      end
    end

    if not TheOneAndOnly then
      GoToEmptyLine()
    end

    Unindent()
    Emit('}')
  end

local ForceMerge = request('!.table.merge_and_patch')
local InstallMinimalHandlers = request('minimal')

-- Exports:
return
  function(a_Handlers, a_Printer, Options)
    InstallMinimalHandlers(a_Handlers, a_Printer, Options)
    Handlers = ForceMerge(a_Handlers, Handlers)
    Printer = a_Printer
    if is_table(Options) and is_boolean(Options.compact_sequences) then
      CompactSequences = options.compact_sequences
    end
  end

--[[
  2018-02-05
  2024-08-09
]]
]=],
  ['workshop.concepts.lua_table.save.interface'] = [[
return
  {
    init = request('init'),

    get_ast = request('get_ast'),
    serialize_ast = request('serialize_ast'),

    OnlyRestorableItems = false,

    node_handlers = {},
    c_text_block = request('!.mechs.text_block.interface'),
    text_block = nil,
    value_names = {},
    table_iterator = request('!.table.ordered_pass'),
    install_node_handlers = request('install_node_handlers.readable'),
  }
]],
  ['workshop.concepts.lua_table.save.serialize_ast'] = [[
local compile = request('!.struc.compile')

return
  function(self, ast)
    compile(ast, self.node_handlers)
    return self.text_block:get_text() .. '\n'
  end
]],
  ['workshop.concepts.path_name.parse'] = [=[
--[[
  Parse string with Unix pathname. Return table.

  {
    path: table - sequence of strings
    directory: string
    name: string or nil
    is_directory: bool
    is_absolute: bool
  }

  ".." element: It does not interpret "a/b/.." as "a/" because
  "b" may be a symlink to another directory.

  "is_directory" field may be false negative.

  Examples:

    /a
      path = {'a'}
      directory = '/'
      name = 'a'
      is_directory = false
      is_absolute = true
    ././//a/./
      path = {'a'}
      directory = 'a/'
      name = 'a'
      is_directory = true
      is_absolute = false
    a/b/..
      path = {'a', 'b', '..'}
      directory = 'a/b/../'
      name = '..'
      is_directory = true
      is_absolute = false
    /
      path = {}
      directory = '/'
      is_directory = true
      is_absolute = true
]]

local split_string = request('!.string.split')

return
  function(path_name)
    assert_string(path_name)

    local result =
      {
        path = nil,
        directory = nil,
        name = nil,
        is_directory = false,
        is_absolute = false,
      }

    -- If starts with "/" than path is absolute.
    if (path_name:sub(1, 1) == '/') then
      result.is_absolute = true
    end

    -- Replace "//" with "/".
    path_name = path_name:gsub('//+', '/')

    -- Replace "/./" with "/".
    local num_repl
    repeat
      path_name, num_repl = path_name:gsub('/%./', '/')
    until (num_repl == 0)

    -- Strip "./" from start.
    path_name = path_name:gsub('^%./', '')

    -- Replace in tail "/." to "/".
    if (path_name:sub(-2) == '/.') then
      path_name = path_name:gsub('/%.$', '/')
    end

    -- Directory ends on "/" or on "..".
    if
      (path_name:sub(-1) == '/') or
      (path_name:sub(-2) == '..')
    then
      result.is_directory = true
    end

    result.path = split_string(path_name, '/')
    result.name = result.path[#result.path]

    if result.is_directory then
      result.directory = table.concat(result.path, '/')
    else
      result.directory = table.concat(result.path, '/', 1, #result.path - 1)
    end

    if
      (result.directory ~= '') or
      result.is_absolute
    then
      result.directory = result.directory .. '/'
    end

    return result
  end
]=],
  ['workshop.file_system.file.as_string'] = [[
local safe_open = request('safe_open')
local exists = request('exists')

return
  function(file_name, create_missing_file)
    if not exists(file_name) and create_missing_file then
      safe_open(file_name, 'w+'):write(''):close()
    end
    local f = safe_open(file_name, 'rb')
    local result = f:read('a')
    f:close()
    return result
  end
]],
  ['workshop.file_system.file.exists'] = [[
return
  function(file_name)
    local file_handle = io.open(file_name, 'r')
    local result = (file_handle ~= nil)
    if result then
      io.close(file_handle)
    end
    return result
  end
]],
  ['workshop.file_system.file.safe_open'] = [[
-- Open file and return handle or throw an error.

-- Last mod.: 2017-10-12

return
  function(file_name, mode)
    assert_string(file_name)
    mode = mode or 'rb'
    assert_string(mode)
    local f_object, err_msg = io.open(file_name, mode)
    if not f_object then
      error(err_msg, 2)
    end
    return f_object
  end
]],
  ['workshop.lua.data_mathtypes'] = [=[
-- Return list of numeric type names

--[[
  Output

    table

      List with number type names as they are returned
      by math.type() function.

  Note

    Used in code generation.
]]

-- Last mod.: 2024-08-06

return
  {
    'integer',
    'float',
  }

--[[
  2024-03-02
]]
]=],
  ['workshop.lua.data_types'] = [=[
-- Return list with names of all Lua data types

--[[
  Output

    table

      List of strings with type names as they are returned
      by type() function.

  Note

    Used in code generation.
]]

-- Last mod.: 2024-08-06

return
  {
    'nil',
    'boolean',
    'number',
    'string',
    'function',
    'thread',
    'userdata',
    'table',
  }

--[[
  2018-02
]]
]=],
  ['workshop.lua.regexp.magic_char_pattern'] = [[
local magic_chars = request('magic_chars')
local magic_char_patttern = '[' .. magic_chars:gsub('.', '%%%0') .. ']'
return magic_char_patttern
]],
  ['workshop.lua.regexp.magic_chars'] = [[
return '^$()[]%.?*+-'
]],
  ['workshop.lua.regexp.quote'] = [=[
--[[
  Quote all special regexp symbols in regexp-string.
  Returns regexp-string.

  Useful when you wish to pass to gsub() string with special
  characters with their literal meaning.

  Usually you don't want to apply it more than once.

  Example:

    /*
      Regexp meaning:
        '.' - any char
        '%.' - "."
        '%%%.' - "%."
    */
    quote('.') -> '%.'
    quote('%.') -> '%%%.'
]]

-- Last mod.: 2024-10-20

local magic_char_pattern = request('magic_char_pattern')

return
  function(s)
    return s:gsub(magic_char_pattern, '%%%0')
  end

--[[
  2018-02-05
  2024-10-20
]]
]=],
  ['workshop.lua.string.quote'] = [[
return request('!.concepts.lua.quote_string')
]],
  ['workshop.mechs.cmdline.get_cmd_listdirs'] = [=[
--[[
  Return shell command to list all directories in given directory.

  GNU/Bash assumed.
]]

-- Last mod.: 2024-10-21

return
  function(dir_name)
    return ('find %q -maxdepth 1 -type d'):format(dir_name)
  end

--[[
  2019-12-01
  2024-10-21
]]
]=],
  ['workshop.mechs.cmdline.get_cmd_listfiles'] = [=[
--[[
  Return shell command to list all files (not directories)
  in given directory.

  GNU/Bash assumed.
]]

-- Last mod.: 2024-10-21

return
  function(dir_name)
    return ('find %q -maxdepth 1 -type f'):format(dir_name)
  end

--[[
  2019-12-01
  2024-10-21
]]
]=],
  ['workshop.mechs.file_lister.align_start_dir'] = [=[
--[[
  Assures that <.start_dir> is somewhat meaningful.
]]

return
  function(self)
    if not self.start_dir or (self.start_dir == '') then
      self.start_dir = '.'
    end
    assert_string(self.start_dir)
  end
]=],
  ['workshop.mechs.file_lister.get_directories_list'] = [=[
--[[
  Return list of directory names in <.start_dir>.
]]

local get_cmd_listdirs = request('!.mechs.cmdline.get_cmd_listdirs')
local get_program_output_lines = request('!.system.get_program_output_lines')

return
  function(self)
    local cmd_get_dirs = get_cmd_listdirs(self.start_dir)
    local result = get_program_output_lines(cmd_get_dirs)
    self:simplify_file_names(result)
    self:remove_start_dir(result)
    return result
  end
]=],
  ['workshop.mechs.file_lister.get_files_list'] = [=[
--[[
  Return list of file names in <.start_dir>.
]]

local get_cmd_listfiles = request('!.mechs.cmdline.get_cmd_listfiles')
local get_program_output_lines = request('!.system.get_program_output_lines')

return
  function(self)
    local cmd_get_files = get_cmd_listfiles(self.start_dir)
    local result = get_program_output_lines(cmd_get_files)
    self:simplify_file_names(result)
    return result
  end
]=],
  ['workshop.mechs.file_lister.init'] = [[
return
  function(self)
    self:align_start_dir()
  end
]],
  ['workshop.mechs.file_lister.interface'] = [[
return
  {
    start_dir = '.',
    init = request('init'),
    get_files_list = request('get_files_list'),
    get_directories_list = request('get_directories_list'),

    remove_dir_prefix = request('remove_dir_prefix'),
    simplify_file_names = request('simplify_file_names'),
    align_start_dir = request('align_start_dir'),
    remove_start_dir = request('remove_start_dir'),
  }
]],
  ['workshop.mechs.file_lister.remove_dir_prefix'] = [=[
--[[
  Returns <file_name> without <.start_dir> part.

  Input: <self> <file_name>
]]

local quote_regexp = request('!.lua.regexp.quote')

return
  function(self, file_name)
    local prefix = self.start_dir
    if (prefix:sub(-1) ~= '/') then
      prefix = prefix .. '/'
    end
    local pattern = '^' .. quote_regexp(prefix)
    return file_name:gsub(pattern, '')
  end
]=],
  ['workshop.mechs.file_lister.remove_start_dir'] = [=[
--[[
  Given list of found directories, exclude entry with <.start_dir>.
]]

return
  function(self, dir_list)
    for i = 1, #dir_list do
      -- if (start_dir == '/') then after remove_dir_prefix() (dir_list[i] == '')
      if (dir_list[i] == self.start_dir) or (dir_list[i] == '') then
        table.remove(dir_list, i)
        return
      end
    end
  end
]=],
  ['workshop.mechs.file_lister.simplify_file_names'] = [=[
--[[
  Simplifies given list of file names by removing <.start_dir> prefix.
]]

return
  function(self, file_names)
    for i = 1, #file_names do
      file_names[i] = self:remove_dir_prefix(file_names[i])
    end
  end
]=],
  ['workshop.mechs.text_block.dec_indent'] = [[
return
  function(self)
    self.Indent:Decrease()
  end
]],
  ['workshop.mechs.text_block.inc_indent'] = [[
return
  function(self)
    self.Indent:Increase()
  end
]],
  ['workshop.mechs.text_block.init'] = [[
return
  function(self)
    self.processed_text = {}

    self.Indent:Init(self.next_line_indent, self.indent_chunk)

    self.line_with_text:init(self.Indent:GetString())

    self.num_line_feeds = 0
  end
]],
  ['workshop.mechs.text_block.interface'] = [[
return
  {
    -- text:
    line_with_text = request('line.interface'),
    processed_text = {},
    num_line_feeds = 0,

    store_textline = request('text.store_textline'),
    add_textline = request('text.add_textline'),
    add_curline = request('text.add_curline'),

    new_line = request('text.new_line'),
    request_clean_line = request('text.request_clean_line'),
    request_empty_line = request('text.request_empty_line'),

    on_clean_line = request('text.on_clean_line'),

    include = request('text.include'),

    get_text = request('text.get_text'),

    -- indents:
    indent_chunk = '  ',
    next_line_indent = 0,
    inc_indent = request('inc_indent'),
    dec_indent = request('dec_indent'),

    -- text length:
    max_text_width = 0,
    max_block_width = 0,
    get_text_width = request('text.get_text_width'),
    get_block_width = request('text.get_block_width'),

    init = request('init'),

    -- Intestines
    Indent = request('!.concepts.Indent.Interface'),
  }
]],
  ['workshop.mechs.text_block.line.add'] = [[
-- Add string to text
return
  function(self, s)
    self.text = self.text .. s
  end
]],
  ['workshop.mechs.text_block.line.get_line'] = [[
-- Return string with indent and text
return
  function(self)
    if self:is_empty() then
      return ''
    end

    return self.indent .. self.text
  end
]],
  ['workshop.mechs.text_block.line.get_line_length'] = [[
-- Return length of indented text
return
  function(self)
    if self:is_empty() then
      return 0
    end

    return utf8.len(self.indent) + self:get_text_length()
  end
]],
  ['workshop.mechs.text_block.line.get_text_length'] = [[
-- Return length of text without indent
return
  function(self)
    return utf8.len(self.text) or #self.text
  end
]],
  ['workshop.mechs.text_block.line.init'] = [[
-- Set indent string and empty text
return
  function(self, IndentValue)
    assert_string(IndentValue)
    self.indent = IndentValue

    self.text = ''
  end
]],
  ['workshop.mechs.text_block.line.interface'] = [=[
-- Indented line interface

return
  {
    -- Contract:

    -- Set indent, empty text
    init = request('init'),

    -- Text is empty?
    is_empty = request('is_empty'),

    -- Get length of text
    get_text_length = request('get_text_length'),

    -- Get length of indented text
    get_line_length = request('get_line_length'),

    -- Get indented text
    get_line = request('get_line'),

    -- Add string to text
    add = request('add'),

    -- Intestines:

    -- Indent string
    indent = '',
    -- Text string
    text = '',
  }

--[[
  2017-09
  2024-09
]]
]=],
  ['workshop.mechs.text_block.line.is_empty'] = [[
-- Return true if text is empty

return
  function(self)
    return (self.text == '')
  end
]],
  ['workshop.mechs.text_block.text.add_curline'] = [=[
return
  function(self, s)
    if (self.num_line_feeds > 0) and (s ~= '') then
      --[[
        We're going to add some text to currently empty line.
        So <line_with_text> will point to this text. Save previous
        text from this object.
      ]]
      self:store_textline()
    end
    self.line_with_text:add(s)
  end
]=],
  ['workshop.mechs.text_block.text.add_textline'] = [[
return
  function(self, s)
    self.line_with_text:add(s)
  end
]],
  ['workshop.mechs.text_block.text.get_block_width'] = [[
return
  function(self)
    return
      math.max(self.max_block_width, self.line_with_text:get_line_length())
  end
]],
  ['workshop.mechs.text_block.text.get_text'] = [[
return
  function(self)
    self:store_textline()
    local result = table.concat(self.processed_text)
    return result
  end
]],
  ['workshop.mechs.text_block.text.get_text_width'] = [[
return
  function(self)
    return
      math.max(self.max_text_width, self.line_with_text:get_text_length())
  end
]],
  ['workshop.mechs.text_block.text.include'] = [[
return
  function(self, block, do_glue_border_lines)
    if not do_glue_border_lines then
      self:new_line()
    end
    self:store_textline()

    table.move(
      block.processed_text,
      1,
      #block.processed_text,
      #self.processed_text + 1,
      self.processed_text
    )

    self.line_with_text = block.line_with_text
  end
]],
  ['workshop.mechs.text_block.text.new_line'] = [[
return
  function(self)
    self.num_line_feeds = self.num_line_feeds + 1
  end
]],
  ['workshop.mechs.text_block.text.on_clean_line'] = [[
return
  function(self)
    return
      (self.num_line_feeds > 0) or
      (
        (self.num_line_feeds == 0) and
        (self.line_with_text.text == '')
      )
  end
]],
  ['workshop.mechs.text_block.text.request_clean_line'] = [[
return
  function(self)
    if not self:on_clean_line() then
      self:new_line()
    end
  end
]],
  ['workshop.mechs.text_block.text.request_empty_line'] = [[
return
  function(self)
    if not self:on_clean_line() then
      self:new_line()
    end
    if (self.num_line_feeds == 1) then
      self:new_line()
    end
  end
]],
  ['workshop.mechs.text_block.text.store_textline'] = [[
local trim = request('!.string.trim')

return
  function(self)
    local line_with_text = self.line_with_text

    line_with_text.text = trim(line_with_text.text)

    self.max_block_width = self:get_block_width()
    self.max_text_width = self:get_text_width()

    self.processed_text[#self.processed_text + 1] = line_with_text:get_line()
    for i = 1, self.num_line_feeds do
      self.processed_text[#self.processed_text + 1] = '\n'
    end
    self.num_line_feeds = 0

    line_with_text:init(self.Indent:GetString())
  end
]],
  ['workshop.string.content_attributes'] = [=[
local has_control_chars =
  function(s)
    return s:find('%c') and true
  end

local has_backslashes =
  function(s)
    return s:find([[%\]]) and true
  end

local has_single_quotes =
  function(s)
    return s:find([[%']]) and true
  end

local has_double_quotes =
  function(s)
    return s:find([[%"]]) and true
  end

local is_nonascii =
  function(s)
    return s:find('[^%w%s_%p]')
  end

local has_newlines =
  function(s)
    return s:find('[\n\r]')
  end

return
  {
    has_control_chars = has_control_chars,
    has_backslashes = has_backslashes,
    has_single_quotes = has_single_quotes,
    has_double_quotes = has_double_quotes,
    is_nonascii = is_nonascii,
    has_newlines = has_newlines,
  }

--[[
  2016-09
  2017-02
  2017-08
  2024-11
]]
]=],
  ['workshop.string.from_lines'] = [=[
-- Concatenate sequence of lines to single string

--[[
  Input

    table

      Sequence of strings without newlines

  Output

    string

      Concatenated string from sequence, separated by newlines.
      Result string will always end with newline.
]]

-- Last mod.: 2024-03-02

--[[
  Good design is not about implementing complex things.
  Good design is providing uniform interface to many things.

  2024-02-28, -03-02
]]

return
  function(Lines)
    assert_table(Lines)

    local Result = ''

    Result = table.concat(Lines, '\n')

    Result = Result .. '\n'

    return Result
  end

--[[
  2024-02-28
]]
]=],
  ['workshop.string.get_next_line'] = [=[
-- Get line from given string and start position

--[[
  Input

    <s>: string
      Given string.

    | <start_pos>: int = 1
      Position where to start extraction of next line.

  Output

    Return argument for next iteration and result or nil:

    If <start_pos> is beyond <s>, return

      <nil>, <nil>

    Else return

      <int>
        Position where next line starts. Expected to become <start_pos>
        value for next call.

      <str>
        Extracted line (without newline character).

  Special cases

    1. No tail newline:

      If there is no tail newline

        Handle that part as if newline was present

    2. Empty string

      If <s> is empty

        Return ('', 1)
]]

-- Last mod.: 2024-02-28

return
  function(s, start_pos)
    -- If <start_pos> is nil we consider it as a first iteration.
    if (is_nil(start_pos)) then
      -- For empty line return empty line. Make sure it's the last iteration.
      if (s == '') then
        return 1, ''
      else
        start_pos = 1
      end
    end

    assert_string(s)

    assert_integer(start_pos)

    -- Throw error for negative indexes:
    if (start_pos <= 0) then
      error('get_next_line(): start index must be natural number')
    end

    -- Termination condition:
    if (start_pos > #s) then
      return nil, nil
    end

    -- Here <start_pos> is index inside string <s>.

    local pattern = '(.-)\n'
    local entry_start, entry_finish, capture = string.find(s, pattern, start_pos)

    if is_nil(capture) then
      --[[
        String doesn't ends with newline.

        Treat remained string suffix as line.
      ]]
      entry_start = start_pos
      entry_finish = #s
      capture = string.sub(s, entry_start, entry_finish)
    end

    return entry_finish + 1, capture
  end

--[[
  2018-02-06
  2024-02-28
]]
]=],
  ['workshop.string.lines'] = [=[
--[[
  Generic-for iterator for lines in string.

  Interface:
    input:
      base, start_index
    output:
      func, base, start_index

  Common use case is

    local lines = request(<...>)
    for next_pos, line in lines(s) do
      <...>
    end
]]

local get_next_line = request('get_next_line')

return
  function(s, start_pos)
    return get_next_line, s, start_pos
  end
]=],
  ['workshop.string.split'] = [=[
--[[
  Split string in pieces.

  Gets source string with delimiter and delimiter string.
  Returns table with sequence of pieces, without delimiter string.
  Delimiter may not be present at end of string. So result for
  ('a;b', ';') is same as for ('a;b;', ';').

  Input:
    s: string - source string
    opt delim: string - delimiter string
      default: "\n"

  Output:
    table - list of strings
]]

return
  function(s, delim)
    assert_string(s)
    local delim = delim or '\n'
    assert_string(delim)
    local result = {}
    local last_pos = 1
    for line, _last_pos in string.gmatch(s, '(.-)' .. delim .. '()') do
      result[#result + 1] = line
      last_pos = _last_pos
    end
    result[#result + 1] = s:sub(last_pos)
    return result
  end
]=],
  ['workshop.string.to_lines'] = [=[
-- Convert string to list of lines

-- Last mod.: 2024-11-20

-- Imports:
local LinesIterator = request('!.string.lines')

--[[
  Explode string to list of lines.

  For empty string '', we're returning empty table {}.
  Not table with empty string {''}.
]]
local StringToLines =
  function(s)
    assert_string(s)

    -- Special case: return "{}" for empty string
    if (s == '') then
      return {}
    end

    local Result = {}

    for NextReadIdx, Line in LinesIterator(s) do
      table.insert(Result, Line)
    end

    return Result
  end

-- Exports:
return StringToLines

--[[
  2024-02
  2024-03
  2024-11
]]
]=],
  ['workshop.string.trim'] = [[
local trim_head = request('trim_head')
local trim_tail = request('trim_tail')

return
  function(s)
    return trim_head(trim_tail(s))
  end
]],
  ['workshop.string.trim_head'] = [[
return
  function(s)
    local result
    if (s:sub(1, 1) == ' ') then
      local start_pos = 2
      while (s:sub(start_pos, start_pos) == ' ') do
        start_pos = start_pos + 1
      end
      result = s:sub(start_pos)
    else
      result = s
    end
    return result
  end
]],
  ['workshop.string.trim_tail'] = [=[
-- Remove spaces at end of string

-- Last mod.: 2024-10-24

return
  function(s)
    assert_string(s)

    local result

    if (s:sub(-1, -1) == ' ') then
      local finish_pos = #s - 1
      while (s:sub(finish_pos, finish_pos) == ' ') do
        finish_pos = finish_pos - 1
      end
      result = s:sub(1, finish_pos)
    else
      result = s
    end

    return result
  end

--[[
  2017-01-20
]]
]=],
  ['workshop.struc.compile'] = [=[
-- Exotic table-to-string conversion

--[[
  Input

    Node: string or table
      if table then
        <Node.type> should be present

    NodeHandlers: table of functions
      key: <Node.type>

  Idea is to

    compile(
      { 'A', {type = 'Special', Value = 'X'}}, 'B' },
      { Special = function(Node) return ' -= ' .. Node.Value .. ' =- '}
    ) -> 'A -= X =- B'
]]

-- Last mod.: 2024-10-21

local compile_core = request('compile_core')
local ListToString = request('!.concepts.List.ToString')

return
  function(Node, NodeHandlers)
    if is_string(Node) then
      return Node
    else
      assert_table(Node)
    end

    NodeHandlers = NodeHandlers or {}
    assert_table(NodeHandlers)

    local Result = {}

    compile_core(Node, NodeHandlers, Result)
    Result = ListToString(Result)

    return Result
  end

--[[
  2017-02-13
  2017-05-09
  2017-08-27
  2018-08-08
  2024-10-21
]]
]=],
  ['workshop.struc.compile_core'] = [[
local compile
compile =
  function(node, node_handlers, result)
    if is_string(node) then
      result[#result + 1] = node
    elseif is_table(node) then
      local node_handler = node_handlers[node.type]

      if node.type and not node_handler then
        local msg =
          ('No node handler for type "%s".'):format(node.type)
        io.stderr:write(msg)
      end

      if node_handler then
        result[#result + 1] = node_handler(node)
      else
        for i = 1, #node do
          compile(node[i], node_handlers, result)
        end
      end
    end
  end

return compile
]],
  ['workshop.system.get_program_output_lines'] = [=[
--[[
  Execute shell command in given string.
  Return command output as list of lines.
]]

-- Last mod.: 2024-11-20

local StringToLines = request('!.string.to_lines')

return
  function(Command)
    assert_string(Command)
    local OutputPipe = io.popen(Command, 'r')
    local OutputText = OutputPipe:read('a')
    OutputPipe:close()

    local Result = StringToLines(OutputText)

    return Result
  end

--[[
  2017-08-11
  2024-02-11 Documentation change
  2024-11-20
]]
]=],
  ['workshop.system.install_assert_functions'] = [=[
-- Function to spawn "assert_<type>" family of global functions

local data_types = request('!.lua.data_types')
local data_mathtypes = request('!.lua.data_mathtypes')

local generic_assert =
  function(type_name)
    -- assert_string(type_name)
    assert(type(type_name) == 'string')

    local checker_name = 'is_'.. type_name
    local checker = _G[checker_name]

    -- assert_function(checker)
    assert(type(checker) == 'function')

    return
      function(val)
        if not checker(val) then
          local err_msg =
            string.format('assert_%s(%s)', type_name, tostring(val))
          error(err_msg)
        end
      end
  end

return
  function()
    for _, type_name in ipairs(data_types) do
      local global_name = 'assert_' .. type_name
      _G[global_name] = generic_assert(type_name)
    end

    for _, number_type_name in ipairs(data_mathtypes) do
      local global_name = 'assert_' .. number_type_name
      _G[global_name] = generic_assert(number_type_name)
    end
  end

--[[
  2018-02
  2020-01
  2022-01
  2024-03
]]
]=],
  ['workshop.system.install_is_functions'] = [=[
-- Function to spawn "is_<type>" family of global functions.

--[[
  It spawns "is_nil", "is_boolean", ... for all Lua data types.
  Also it spawns "is_integer" and "is_float" for number type.
]]

--[[
  Design

    f(:any) -> bool

    Original design was

      f(:any) -> bool, (string or nil)

      Use case was "assert(is_number(x))" which will automatically
      provide error message when "x" is not a number.

      Today I prefer less fancy designs. Caller has enough information
      to build error message itself.
]]

-- Last mod.: 2024-03-02

local data_types = request('!.lua.data_types')
local data_mathtypes = request('!.lua.data_mathtypes')

local type_is =
  function(type_name)
    return
      function(val)
        return (type(val) == type_name)
      end
  end

local number_is =
  function(type_name)
    return
      function(val)
        --[[
          math.type() throws error for non-number types.
          This function returns "false" for non-number types.
        ]]
        if not is_number(val) then
          return false
        end
        return (math.type(val) == type_name)
      end
  end

return
  function()
    for _, type_name in ipairs(data_types) do
      _G['is_' .. type_name] = type_is(type_name)
    end
    for _, math_type_name in ipairs(data_mathtypes) do
      _G['is_' .. math_type_name] = number_is(math_type_name)
    end
  end

--[[
  2018-02
  2020-01
  2022-01
  2024-03 Changed design
]]
]=],
  ['workshop.table.as_string'] = [[
return request('!.concepts.lua_table.save')
]],
  ['workshop.table.clone'] = [=[
local cloned = {}

local clone
clone =
  function(node)
    if (type(node) == 'table') then
      if cloned[node] then
        return cloned[node]
      else
        local result = {}
        cloned[node] = result
        for k, v in pairs(node) do
          result[clone(k)] = clone(v)
        end
        setmetatable(result, getmetatable(node))
        return result
      end
    else
      return node
    end
  end

return
  function(node)
    cloned = {}
    return clone(node)
  end

--[[
* Metatables are shared, not cloned.

* This code optimized for performance.

  Main effect gave changing "is_table" to explicit type() check.
]]
]=],
  ['workshop.table.get_key_vals'] = [[
return
  function(t)
    assert_table(t)
    local result = {}
    for k, v in pairs(t) do
      result[#result + 1] = {key = k, value = v}
    end
    return result
  end
]],
  ['workshop.table.hard_patch'] = [=[
-- Shortcut to overwrite values in destination table according to patch

-- Last mod.: 2024-11-11

-- Imports:
local Patch = request('patch')

local HardPatch =
  function(Dest, PatchTable)
    return Patch(Dest, PatchTable, false)
  end

-- Exports:
return HardPatch

--[[
  2024-11-11
]]
]=],
  ['workshop.table.map_values'] = [=[
-- Map table values to keys

--[[
  Useful when you want to check presence and have list.

    { 'A', 'A', a = 'A', b = 'A'} ->
    { [1] = true, [2] = true, A = true }
]]

-- Last mod.: 2024-10-20

return
  function(t)
    assert_table(t)

    local Result = {}

    for k, v in pairs(t) do
      Result[v] = true
    end

    return Result
  end

--[[
  2016-09-06
  2024-10-20
]]
]=],
  ['workshop.table.merge'] = [=[
-- Merge one table onto another

--[[
  Union:
    ({ a = 'A'}, { b = 'B' }) -> { a = 'A', b = 'B' }

  Source values preserved:
    ({ a = 'A'}, { a = 'a' }) -> { a = 'A' }
]]

local MergeTable =
  function(Result, Additions)
    assert_table(Result)
    if (Additions == nil) then
      return Result
    end

    assert_table(Additions)
    for Addition_Key, Addition_Value in pairs(Additions) do
      if is_nil(Result[Addition_Key]) then
        Result[Addition_Key] = Addition_Value
      end
    end

    return Result
  end

-- Exports:
return MergeTable

--[[
  2016-06
  2016-09
  2017-09
  2019-12
  2024-08
]]
]=],
  ['workshop.table.merge_and_patch'] = [=[
-- Merge destination table. Override existing fields in source table

-- Last mod.: 2024-11-11

-- Imports:
local Merge = request('merge')
local HardPatch = request('hard_patch')

local MergeAndPatch =
  function(Dest, Source)
    Merge(Dest, Source)
    HardPatch(Dest, Source)
    return Dest
  end

-- Exports:
return MergeAndPatch

--[[
  2024-11-11
]]
]=],
  ['workshop.table.new'] = [=[
--[[
  Clone table <base_obj>. Optionally override fields in clone with
  fields from <overriden_params>.

  Returns cloned table.
]]

local clone = request('clone')
local patch = request('patch')

return
  function(base_obj, overriden_params)
    assert_table(base_obj)
    local result = clone(base_obj)
    if is_table(overriden_params) then
      patch(result, overriden_params)
    end
    return result
  end
]=],
  ['workshop.table.ordered_pass'] = [[
local default_comparator = request('ordered_pass.default_comparator')
local get_key_vals = request('get_key_vals')

-- Sort <t> and return iterator function to pass that sorted <t>
return
  function(t, comparator)
    assert_table(t)
    comparator = comparator or default_comparator
    assert_function(comparator)

    local key_vals = get_key_vals(t)
    table.sort(key_vals, comparator)

    local i = 0
    local sorted_next =
      function()
        i = i + 1
        if key_vals[i] then
          return key_vals[i].key, key_vals[i].value
        end
      end

    return sorted_next, t
  end
]],
  ['workshop.table.ordered_pass.default_comparator'] = [[
local val_rank =
  {
    string = 1,
    number = 2,
    other = 3,
  }

local comparable_types =
  {
    number = true,
    string = true,
  }

return
  function(a, b)
    local a_key = a.key
    local a_key_type = type(a_key)
    local rank_a = val_rank[a_key_type] or val_rank.other

    local b_key = b.key
    local b_key_type = type(b_key)
    local rank_b = val_rank[b_key_type] or val_rank.other

    if (rank_a ~= rank_b) then
      return (rank_a < rank_b)
    else
      if comparable_types[a_key_type] and comparable_types[b_key_type] then
        return (a_key < b_key)
      else
        return (tostring(a_key) < tostring(b_key))
      end
    end
  end
]],
  ['workshop.table.patch'] = [=[
-- Apply patch to table

-- Last mod.: 2024-11-11

--[[
  Basically it means that we're writing every entity from patch table to
  destination table.

  If no key in destination table, we'll explode.

  Additional third parameter means that we're not overwriting
  entity in destination table if it's value type is same as
  in patch's entity.

  That's useful when we want to force values to given types but
  keep values if they have correct type:

    ({ x = 42, y = '?' }, { x = 0, y = 0 }, false) -> { x = 0, y = 0 }
    ({ x = 42, y = '?' }, { x = 0, y = 0 }, true) -> { x = 42, y = 0 }

  Examples:

    Always overwriting values:

      ({ a = 'A' }, { a = '_A' }, false) -> { a = '_A' }

    Overwriting values if different types:

      ({ a = 'A' }, { a = '_A' }, true) -> { a = 'A' }
      ({ a = 0 }, { a = '_A' }, true) -> { a = '_A' }

    Nested values are supported:

      ({ b = { bb = 'BB' } }, { b = { bb = '_BB' } }, false) ->
      { b = { bb = '_BB' } }
]]

local Patch
Patch =
  function(MainTable, PatchTable, IfDifferentTypesOnly)
    assert_table(MainTable)
    assert_table(PatchTable)

    for PatchKey, PatchValue in pairs(PatchTable) do
      local MainValue = MainTable[PatchKey]

      -- Missing key in destination
      if is_nil(MainValue) then
        local ErrorMsg =
          string.format(
            [[Destination table doesn't have key "%s".]],
            tostring(PatchKey)
          )

        error(ErrorMsg, 2)
      end

      local DoPatch = true

      if IfDifferentTypesOnly then
        MainValueType = type(MainValue)
        PatchValueType = type(PatchValue)
        DoPatch = (MainValueType ~= PatchValueType)
      end

      if DoPatch then
        -- Recursive call when we're writing table to table
        if is_table(MainValue) and is_table(PatchValue) then
          Patch(MainValue, PatchValue)
        -- Else just overwrite value
        else
          MainTable[PatchKey] = PatchValue
        end
      end
    end
  end

-- Exports:
return Patch

--[[
  2016-09
  2024-02
  2024-11
]]
]=],
}

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

require('meld')
