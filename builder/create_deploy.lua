-- Load modules to get a list of all required Lua files

--[[
  Author: Martin Eden
  Last mod.: 2026-08-14
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

--[[
  It's tricky here

  "meld" in ModulesList means that we will require() this module.
  And it means we just execute it's code. And then process
  internal table with module dependencies.

  "meld" id command-line tool, without arguments it just prints
  usage text. It does not load modules required for work.

  So we'll call it with some dummy load to get those modules.
]]

arg[1] = '.'
arg[2] = 'meld'

deploy(ModulesList)

--[[
  202?
  2026 # # #
  2026-08-13
  2026-08-14
]]
