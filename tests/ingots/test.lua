-- ( module test
package.preload['test'] =
function(...)
local module_a = require('modules.a')

module_a:represent()
end
-- )

-- ( module modules.a
package.preload['modules.a'] =
function(...)
local represent = require('modules.a.represent')

return
  {
    [1] = '[test representation]',
    represent = represent,
  }
end
-- )

-- ( module modules.a.represent
package.preload['modules.a.represent'] =
function(...)
return
  function(me)
    print(me[1])
  end
end
-- )

return require('test')
