local M = require "modal"

-- TODO: cache locals
-- TODO: generate the def file at build tiem, figure out how to properly load them in coding and performing (learn luaLS!)
-- TODO: use this in repl

local type_var = 96

local function gen_arg()
   type_var = type_var + 1
   return string.char(type_var)
end

local param_format = "---@param %s %s"
local return_format = "---@return %s"

local function get_type(ttab)
   local tstr = ttab[1]
   if ttab.type then
      return "table"
   end
   if tstr == "a" or tstr == "b" then -- not in {Pattern Time ....}
      return "any"
   end
   return tstr
end

local function gen(name)
   local argtypes = M.t[name]
   local arity = #argtypes
   local argstr = ""
   local typestr = ""
   for i = 1, arity do
      local arg_name = argtypes[i].name
      typestr = typestr .. string.format(param_format, arg_name, get_type(argtypes[i])) .. "\n"
      if i == arity then
         argstr = argstr .. arg_name
      else
         argstr = argstr .. arg_name .. ", "
      end
   end
   -- typestr = typestr .. string.format(return_format, argtypes.ret[1]) .. "\n"
   typestr = typestr .. string.format(return_format, "Pattern") .. "\n"
   local fstr = string.format("%s = function(%s) end", name, argstr)
   type_var = 96
   return typestr .. fstr .. "\n"
end

-- for i, _ in pairs(M.t) do
--    -- print(gen(i))
--    local ok, res = pcall(gen, i)
--    if ok then
--       print(res)
--    end
-- end

return gen
