local Modules = {
  ['modules.a'] = [=[
-- Sample module for melding test

-- Last mod.: 2024-11-19

-- Imports:
local Represent = require('modules.a.Represent')

-- Exports:
return
  {
    Represent = Represent,
    --
    Data = '[Test representation.]',
  }

--[[
  2024-11-19
]]
]=],
  ['modules.a.Represent'] = [=[
-- Test function for melder

-- Print <.Data>
local Represent =
  function(self)
    print(self.Data)
  end

-- Exports:
return Represent

--[[
  2024-11-19
]]
]=],
  test = [=[
-- Test code for melder

-- Last mod.: 2024-11-20

local ModuleA = require('modules.a')

if not ModuleA then
  print('Failed.')
  return
end

ModuleA:Represent()

--[[
  2024-11-19
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

require('test')
