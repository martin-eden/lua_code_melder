#! /bin/bash

#
# Pack script into executable Lua file
#

#
# Author: Martin Eden
# Last mod.: 2026-04-25
#

#
# As said in "create_deploy.lua" you'll need full [workshop] version
# of that time placed inside src/ (instead of current version).
#

cp create_deploy.lua ../src
cd ..

cd src
lua create_deploy.lua
rm create_deploy.lua
bash deploy.sh
rm deploy.sh
lua meld.lua deploy/ meld > ../deploy_maker/meld.melded.lua
pwd
rm -r deploy
cd ..

cd deploy_maker
luac -o meld.melded.luac -s meld.melded.lua
rm meld.melded.lua

shebang='#! /usr/local/bin/lua'
echo $shebang > meld.melded.shebang_luac
cat meld.melded.luac >> meld.melded.shebang_luac
rm meld.melded.luac

mv meld.melded.shebang_luac meld
chmod +x meld

mv meld ../bin

# 2026-04-25
