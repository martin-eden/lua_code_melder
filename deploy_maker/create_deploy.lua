-- [Template] Load modules to get a list of all required Lua files

--[[
  Author: Martin Eden
  Last mod.: 2026-04-25
]]

--[[
  That's my internal script to fetch required files from [workshop] and
  generate "deploy.sh" Bash script that will copy them into ./deploy/
  directory.

  Bash script generation requires more code from [workshop] than is
  supplied with tool.

  So if you want to run it yourself:

    * Remove src/workshop/ from this repo
    * Clone [workshop] into src/workshop of this repo
    * Rollback it's version to date near date of tool's code

      It's tricky and needs to be done manually.

      [workshop] has no backward- or forward-compatibility
      so you need version from that time.

      You can skip this step but you've been warned!

    * Place this file inside src/

      This step is done by external "create_deploy.sh" Bash script.
]]

--[[
  How it works

  At loading via request() module dependencies are stored in
  some global table. create_deploy_script() uses that table to
  write Bash script which will copy module files to "./deploy/".
]]

require('workshop.base')

local create_deploy_script = request('!.system.create_deploy_script')

local ModulesList =
  {
    'workshop.base',
    'meld',
  }

create_deploy_script(ModulesList)

--[[
  202?
  2026-01-21
  2026-04-23
  2026-04-25
]]
