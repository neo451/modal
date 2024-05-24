local split
split = require("pl.stringx").split
require("moon.all")
local githubPath
githubPath = function(base, subpath)
   if subpath == nil then
      subpath = ""
   end
   local _, path
   do
      local _obj_0 = split(base, "github:")
      _, path = _obj_0[1], _obj_0[2]
   end
   if #split(path, "/") == 2 then
      path = path .. "/main"
   end
   return ("https://raw.githubusercontent.com/%s/%s"):format(path, subpath)
end

p(githubPath("github:yaxu/clean-breaks"))
