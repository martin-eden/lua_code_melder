-- Load modules to get a list of all required Lua files

--[[
  Author: Martin Eden
  Last mod.: 2026-11-16
]]

--[[
  How to use

  * Include root modules in <modulesList>

  * Do one of

    * Copy this file to root Lua source directory

    * Call this file from Lua source directory:

        $ lua ../builder/deploy.lua

  Make sure that main Lua file executes without errors when
  loaded as module. If needed, make changes to it to behave so.
]]

--
local modules = { 'meld' }
--
package.path = package.path .. ';../../../?.lua'
--
local module_recs
do
  arg[1] = '.'
  arg[2] = 'meld'
  arg[3] = '/tmp/melded.lua'
  local observe_modules = require('workshop.system.observe_modules')
  module_recs = observe_modules(modules)
end
--
require('workshop.base')

local filelist
do
  local add_to = request('!.concepts.list.add_item')
  filelist = { }
  for _, module in ipairs(module_recs) do
    add_to(filelist, module[2])
  end
end

local deploy = request('!.mechs.deploy')

deploy(filelist)
--

--[[
  202?
  2026 # # # # #
]]
