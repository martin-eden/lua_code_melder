-- Test code for melder

local ModuleA = require('modules.a')

if not ModuleA then
  print('Failed.')
  return
end

ModuleA:Represent()
