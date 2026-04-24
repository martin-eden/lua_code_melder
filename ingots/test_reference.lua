_G.package.preload['modules.a.Represent'] =
  function(...)
    return
      function(self)
        print(debug.traceback())
        print(self.Data)
      end
  end

_G.package.preload['modules.a'] =
  function(...)
    local Represent = require('modules.a.Represent')

    return
      {
        Represent = Represent,
        Data = '[Test representation.]',
      }
  end

_G.package.preload.test =
  function(...)
    local ModuleA = require('modules.a')

    ModuleA:Represent()
  end

require('test')
