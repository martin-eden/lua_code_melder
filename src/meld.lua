-- Aggregate Lua source files to one

--[[
  Author: Martin Eden
  License: LGPL v3
  Last mod.: 2024-11-20
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
    ParsedName = ParsePathName(PathName)
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
