#!/bin/sh

# Pack script into executable Lua file

#
# Author: Martin Eden
# Last mod.: 2026-08-14
#

#
# Results are placed in "deploy/"
#
# We will create executable shell file "meld" there.
# It's shebang line and plain Lua code.
#
# Toolchain uses my "lua code formatter" tool to strip comments.
#
#   https://github.com/martin-eden/lua_code_formatter
#

set -e -u

#
# src/
#

cd ../src

rm -r -f workshop/

lua ../builder/create_deploy.lua > /dev/null

mv deploy/workshop/ .
rm -r -f deploy/

# Combine all Lua code
lua meld.lua . meld > ../deploy/meld.melded.lua

#
# deploy/
#

cd ../deploy

# Use Lua code formatter to remove comments and indent code
reformat_lua \
  meld.melded.lua \
  meld.melded.stripped.lua \
  --~keep-comments \
  --right-margin=72
rm meld.melded.lua

# Add shebang to compiled code
echo '#!/usr/local/bin/lua'"\n" > meld.melded.stripped.shebanged.lua
cat meld.melded.stripped.lua >> meld.melded.stripped.shebanged.lua
rm meld.melded.stripped.lua

mv meld.melded.stripped.shebanged.lua meld

chmod +x meld

# Regenerate test output
./meld ../tests/test_case test > ../tests/ingots/test.lua

# 2026-04-25
# 2026-06-04
# 2026-06-16
