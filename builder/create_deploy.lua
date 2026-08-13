-- Load modules to get a list of all required Lua files

--[[
  Author: Martin Eden
  Last mod.: 2026-08-13
]]

--[[
  How to use

  * Include root modules in <ModulesList>

  * Do one of

    * Call this file from root Lua source directory:
        $ lua ../builder/create_deploy.lua

    * Copy this file to root Lua source directory

  Make sure that main Lua file executes without errors when
  loaded as module. If needed, make changes to it to behave so.
]]

package.path = package.path .. ';../../../?.lua'
require('workshop.base')

local deploy = request('!.mechs.deploy')

local ModulesList =
  {
    'workshop.base',
    'meld',
  }

deploy(ModulesList)

--[[
  202?
  2026 # # #
  2026-08-13
]]
