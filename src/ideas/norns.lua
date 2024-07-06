local t_concat = table.concat
local params = require("modal").params
local state = {
   A = {
      "[sd bd]",
      "fast 2",
      "room [9.2]",
   }, -- fast 2 $ s [sd bd] # room 9.2
   B = {
      "sd = 808sd",
      "[bd 'sd]",
   }, -- sd = "808sd"; s [bd 'sd]
   M = {
      "mode: stack", -- ['a, 'b ...]
      "mode: arrange",
      "16 a",
      "32 b",
   },
}
-- require "moon.all"

---takes a table of strings and concat it correctly
---@param name string
---@param scene any
local function compile_maxi(name, scene)
   local after = {}
   local trans = {}
   local decl = { "" }
   local center
   -- local center = table.remove(scene, 1)
   for _, v in pairs(scene) do
      local method = v:match "^%w+"
      print(method)
      if not method then
         center = "s " .. v
      elseif params[method] then
         after[#after + 1] = "# " .. v
      elseif v:find "=" then
         print(v)
         decl[#decl + 1] = v .. "; "
      else
         trans[#trans + 1] = v .. " $"
      end
   end
   -- p(after)
   -- p(trans)
   -- p(decl)
   -- p(center)
   return ("%s%s = (%s %s %s)"):format(t_concat(decl), name:lower(), t_concat(trans, " "), center, t_concat(after, " "))
end

-- res = compile_maxi("B", a[1])
res = compile_maxi("C", state[2])
print(res)
