local parse
parse = require("xi.mini.grammar").parse
local reduce, op
do
  local _obj_0 = require("xi.fun")
  reduce, op = _obj_0.reduce, _obj_0.op
end
local Visitor
do
  local _class_0
  local _base_0 = {
    visit = function(self, node)
      local type = node[1]
      local method = self[type]
      if node[3] ~= nil then
        local children = { }
        for i = 3, #node do
          children[#children + 1] = node[i]
        end
        for i, subnode in ipairs(children) do
          children[i] = self:visit(subnode)
        end
        return method(self, node, children)
      else
        return method(self, node, "")
      end
    end,
    root = function(self, _, children)
      return children[1]
    end,
    sequence = function(self, _, children)
      local group, other_groups, other_seqs
      group, other_groups, other_seqs = children[1], children[2], children[3]
      if other_groups ~= "" then
        local elements = {
          {
            type = "element",
            value = {
              type = "polyrhythm",
              seqs = {
                group
              }
            },
            modifiers = { }
          }
        }
        for _index_0 = 1, #other_groups do
          local item = other_groups[_index_0]
          elements[#elements + 1] = {
            type = "element",
            value = {
              type = "polyrhythm",
              seqs = {
                item
              }
            },
            modifiers = { }
          }
        end
        group = {
          type = "sequence",
          elements = elements
        }
      end
      if other_seqs ~= "" then
        local n_seq = {
          group
        }
        for _index_0 = 1, #other_seqs do
          local item = other_seqs[_index_0]
          n_seq[#n_seq + 1] = item
        end
        return {
          type = "random_sequence",
          elements = n_seq
        }
      end
      return group
    end,
    group = function(self, _, children)
      local elements, other_elements
      elements, other_elements = children[1], children[2]
      local n_elements = {
        elements
      }
      if other_elements ~= "" then
        for _index_0 = 1, #other_elements do
          local item = other_elements[_index_0]
          n_elements[#n_elements + 1] = item
        end
      end
      return {
        type = "sequence",
        elements = n_elements
      }
    end,
    other = function(self, _, children)
      return children or ""
    end,
    element = function(self, _, children)
      local value, eculid_modifier, modifiers, elongate
      value, eculid_modifier, modifiers, elongate = children[1], children[2], children[3], children[4]
      local weight_mods, n_mods = { }, { }
      for _index_0 = 1, #modifiers do
        local mod = modifiers[_index_0]
        if mod.op == "weight" then
          table.insert(weight_mods, mod)
        else
          table.insert(n_mods, mod)
        end
      end
      local weight_mod = weight_mods[1] or {
        type = "modifier",
        op = "weight",
        value = elongate
      }
      if weight_mod.value ~= 1 then
        table.insert(n_mods, weight_mod)
      end
      return {
        type = "element",
        value = value,
        modifiers = n_mods,
        euclid_modifier = eculid_modifier
      }
    end,
    m_element = function(self, _, children)
      local value, eculid_modifier
      value, eculid_modifier = children[1], children[2]
      return {
        type = "element",
        value = value,
        modifiers = { },
        euclid_modifier = eculid_modifier
      }
    end,
    polyrhythm_subseq = function(self, _, children)
      return {
        type = "polyrhythm",
        seqs = children[1]
      }
    end,
    polymeter_subseq = function(self, _, children)
      local seqs, steps
      seqs, steps = children[1], children[2]
      return {
        type = "polymeter",
        seqs = seqs,
        steps = steps or 1
      }
    end,
    polymeter_steps = function(self, _, children)
      return children[1]
    end,
    polymeter1_subseq = function(self, _, children)
      return {
        type = "polymeter",
        seqs = children[1],
        steps = 1
      }
    end,
    subseq_body = function(self, _, children)
      local seq, other_seqs
      seq, other_seqs = children[1], children[2]
      local n_subseqs = {
        seq
      }
      if other_seqs ~= "" then
        for _index_0 = 1, #other_seqs do
          local item = other_seqs[_index_0]
          table.insert(n_subseqs, item)
        end
      end
      return n_subseqs
    end,
    elongate = function(self, node, _)
      return #node[2] / 2 + 1
    end,
    element_value = function(self, _, children)
      return children[1]
    end,
    term = function(self, _, children)
      if type(children[1]) == "number" then
        return {
          type = "number",
          value = children[1]
        }
      end
      return children[1]
    end,
    rest = function(self, _, _)
      return {
        type = "rest"
      }
    end,
    word_with_index = function(self, _, children)
      local word, index
      word, index = children[1], children[2]
      return {
        type = "word",
        value = word,
        index = index or 0
      }
    end,
    index = function(self, _, children)
      return children[1]
    end,
    euclid_modifier = function(self, _, children)
      if children == "" then
        return 
      end
      local k, n, rotation
      k, n, rotation = children[1], children[2], children[3]
      if k ~= nil and n ~= nil then
        return {
          type = "euclid_modifier",
          k = k,
          n = n,
          rotation = rotation
        }
      end
    end,
    euclid_rotation_param = function(self, _, children)
      return children[1]
    end,
    modifiers = function(self, _, children)
      if children == "" then
        return { }
      end
      local mods, degrade_mods, weight_mods = { }, { }, { }
      local count_deg_mods, value_deg_mods = { }, { }
      for _index_0 = 1, #children do
        local mod = children[_index_0]
        if mod.op == "degrade" then
          table.insert(degrade_mods, mod)
        elseif mod.op == "weight" then
          table.insert(weight_mods, mod)
        else
          table.insert(mods, mod)
        end
      end
      if #degrade_mods > 0 then
        for _index_0 = 1, #degrade_mods do
          local mod = degrade_mods[_index_0]
          if mod.value.op == "count" then
            table.insert(count_deg_mods, mod)
          else
            table.insert(value_deg_mods, mod)
          end
        end
      end
      if #value_deg_mods > 0 then
        table.insert(mods, value_deg_mods[#value_deg_mods])
      elseif #count_deg_mods > 0 then
        local sum = 0
        for _index_0 = 1, #count_deg_mods do
          local mod = count_deg_mods[_index_0]
          sum = sum + mod.value.value
        end
        table.insert(mods, {
          type = "modifier",
          op = "degrade",
          value = {
            type = "degrade_arg",
            op = "count",
            value = sum
          }
        })
      end
      table.insert(mods, weight_mods[#weight_mods])
      return mods
    end,
    modifier = function(self, _, children)
      return children[1]
    end,
    fast = function(self, _, children)
      return {
        type = "modifier",
        op = "fast",
        value = children[1]
      }
    end,
    slow = function(self, _, children)
      return {
        type = "modifier",
        op = "slow",
        value = children[1]
      }
    end,
    _repeat = function(self, _, children)
      local count = reduce(op.add, 0, children)
      return {
        type = "modifier",
        op = "repeat",
        count = count
      }
    end,
    repeatn = function(self, _, children)
      return children[1]
    end,
    repeat1 = function(self, _, _)
      return 1
    end,
    degrade = function(self, _, children)
      return {
        type = "modifier",
        op = "degrade",
        value = children[1]
      }
    end,
    degrader = function(self, _, children)
      return {
        type = "degrade_arg",
        op = "value",
        value = children[1]
      }
    end,
    degraden = function(self, _, children)
      return {
        type = "degrade_arg",
        op = "count",
        value = children[1]
      }
    end,
    degrade1 = function(self, _, _)
      return {
        type = "degrade_arg",
        op = "count",
        value = 1
      }
    end,
    weight = function(self, _, children)
      return {
        type = "modifier",
        op = "weight",
        value = children[1]
      }
    end,
    word = function(self, node, _)
      local _exp_0 = node[2]
      if "t" == _exp_0 then
        return true
      elseif "f" == _exp_0 then
        return false
      else
        return node[2]
      end
    end,
    number = function(self, _, children)
      return children[1]
    end,
    integer = function(self, node, _)
      return tonumber(node[2])
    end,
    real = function(self, node, _)
      return tonumber(node[2])
    end,
    pos_integer = function(self, node, _)
      return tonumber(node[2])
    end,
    pos_real = function(self, node, _)
      return tonumber(node[2])
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function() end,
    __base = _base_0,
    __name = "Visitor"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Visitor = _class_0
end
local visit
visit = function(code)
  local raw_ast = parse(code)
  return Visitor:visit(raw_ast)
end
return {
  Visitor = Visitor,
  visit = visit
}
