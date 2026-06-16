-- ( ( module test )
_G.package.preload['test'] =
function(...)
local ModuleA = require('modules.a')

ModuleA:Represent()
end
-- ) ( module test )

-- ( ( module modules.a )
_G.package.preload['modules.a'] =
function(...)
local Represent = require('modules.a.Represent')

return
  {
    Represent = Represent,
    Data = '[Test representation.]',
  }
end
-- ) ( module modules.a )

-- ( ( module modules.a.Represent )
_G.package.preload['modules.a.Represent'] =
function(...)
return
  function(self)
    print(self.Data)
  end
end
-- ) ( module modules.a.Represent )

require('test')
