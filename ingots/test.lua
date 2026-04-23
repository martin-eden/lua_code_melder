local Modules = {
  ['modules.a'] = [[
local Represent = require('modules.a.Represent')

return
  {
    Represent = Represent,
    Data = '[Test representation.]',
  }
]],
  ['modules.a.Represent'] = [[
return
  function(self)
    print(self.Data)
  end
]],
  test = [[
-- Test code for melder

local ModuleA = require('modules.a')

if not ModuleA then
  print('Failed.')
  return
end

ModuleA:Represent()
]],
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
