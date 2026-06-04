#!/bin/sh

#
# Pack script into executable Lua file
#

#
# Author: Martin Eden
# Last mod.: 2026-06-04
#

set -eu

cd ../src

rm -rf workshop/

lua ../build/create_deploy.lua

bash deploy.sh
rm deploy.sh

mv deploy/workshop/ .
rm -rf deploy/

# Combine all Lua code
lua meld.lua . meld > ../bin/meld.melded.lua

cd ../bin

# Compile Lua code to reduce size
luac -o meld.melded.luac -s  meld.melded.lua
rm meld.melded.lua

# Add shebang to compiled code
shebang='#!/usr/local/bin/lua'
echo "$shebang" > meld.melded.shebang.luac
cat meld.melded.luac >> meld.melded.shebang.luac
rm meld.melded.luac

mv meld.melded.shebang.luac meld

chmod +x meld

# 2026-04-25
# 2026-06-04
