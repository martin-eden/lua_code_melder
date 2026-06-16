#!/bin/sh

# Pack script into executable Lua file

#
# Author: Martin Eden
# Last mod.: 2026-06-16
#

#
# Results are placed in "bin/"
#
# We will create executable shell file "meld" there.
# It's plain Lua code, shebang line and "executable" attribute.
#
# Toolchain uses my "lua code formatter" tool to strip comments.
#
#   https://github.com/martin-eden/lua_code_formatter
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

# Use Lua code formatter to remove comments and indent code
reformat_lua meld.melded.lua meld.melded.stripped.lua --~keep-comments
rm meld.melded.lua

# Add shebang to compiled code
shebang='#!/usr/local/bin/lua'"\n"
echo "$shebang" > meld.melded.stripped.shebanged.lua
cat meld.melded.stripped.lua >> meld.melded.stripped.shebanged.lua
rm meld.melded.stripped.lua

mv meld.melded.stripped.shebanged.lua meld

chmod +x meld

# Regenerate test output
./meld ../tests/test_case test > ../tests/ingots/test.lua

# 2026-04-25
# 2026-06-04
# 2026-06-16
