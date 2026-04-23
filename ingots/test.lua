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
end

require('test')
