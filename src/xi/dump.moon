
---- moondump.moon ----
-- A reasonably bullet-proof table dumper that writes out in Moon syntax;
-- can limit the number of items dumped out, and cycles are detected.
-- No attempt at doing pretty indentation here, but it does try to produce
-- 'nice' looking output by separating the hash and the array part.
--
--  dump = require 'moondump'
--  ...
--  print dump t  -- default, limit 1000, respect tostring
--  print dump t, limit:10000,raw:true   -- ignore tostring
--
quote  = (v) ->
    if type(v) == 'string'
        '%q'\format(v)
    else
        tostring(v)

--- return a string representation of a Lua value.
-- Cycles are detected, and a limit on number of items can be imposed.
-- @param t the table
-- @param options
--    limit on items, default 1000
--    raw ignore tostring
-- @return a string
(t,options) ->
    options = options or {}
    limit = options.limit or 1000
    buff = tables:{[t]:true}
    k,tbuff = 1,nil

    put = (v) ->
        buff[k] = v
        k += 1

    put_value = (value) ->
        if type(value) ~= 'table'
            put quote value
            if limit and k > limit
                buff[k] = "..."
                error "buffer overrun"
        else
            if not buff.tables[value] -- cycle detection!
                buff.tables[value] = true
                tbuff value
            else
                put "<cycle>"
        put ','

    tbuff = (t) ->
        mt = getmetatable t unless options.raw
        if type(t) ~= 'table' or mt and mt.__tostring
            put quote t
        else
            put '{'
            indices = #t > 0 and {i,true for i = 1,#t}
            for key,value in pairs t -- first do the hash part
                if indices and indices[key] then continue
                if type(key) ~= 'string' then
                    key = '['..tostring(key)..']'
                elseif key\match '%s'
                    key = quote key
                put key..':'
                put_value value

            if indices -- then bang out the array part
                for v in *t
                    put_value v

            if buff[k - 1] == "," then k -= 1
            put '}'


    -- we pcall because that's the easiest way to bail out if there's an overrun.
    pcall tbuff,t
    table.concat(buff)
