-- ( ( module test )
package.preload['test'] =
function(...)
local ModuleA = require('modules.a')

ModuleA:Represent()
end
-- )

-- ( ( module modules.a )
package.preload['modules.a'] =
function(...)
local Represent = require('modules.a.Represent')

return
  {
    Represent = Represent,
    Data = '[Test representation.]',
  }
end
-- )

-- ( ( module modules.a.Represent )
package.preload['modules.a.Represent'] =
function(...)
return
  function(self)
    print(self.Data)
  end
end
-- )

return require('test')
