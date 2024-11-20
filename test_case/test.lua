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
