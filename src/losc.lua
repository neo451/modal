local Serializer = {}

--- Require a function for packing.
-- @return A suitable packing function as explained in the header of this file.
function Serializer.pack()
   if string.pack then
      return string.pack
   end
   local ok, _ = pcall(require, "struct")
   if ok then
      return require("struct").pack
   else
      --TODO:
      -- return require(relpath .. ".lib.struct").pack
   end
end

--- Require a function for unpacking.
-- @return A suitable unpacking function as explained in the header of this file.
function Serializer.unpack()
   if string.unpack then
      return string.unpack
   end
   local ok, _ = pcall(require, "struct")
   if ok then
      return require("struct").unpack
   else
      -- return require(relpath .. ".lib.struct").unpack
   end
end

local _pack = Serializer.pack()
local _unpack = Serializer.unpack()
local has_string_pack = string.pack and true or false

local Timetag = {}

-- 70 years in seconds (1970 - 1900)
local NTP_SEC_OFFSET = 2208988800
-- 2^32
local TWO_POW_32 = 4294967296

local function tt_add(timetag, seconds)
   local sec = math.floor(seconds)
   local frac = math.floor(TWO_POW_32 * (seconds - sec) + 0.5)
   sec = sec + timetag.content.seconds
   frac = frac + timetag.content.fractions
   return Timetag.new_raw(sec, frac)
end

Timetag.__index = Timetag

--- Add a time offset to a Timetag.
-- This overloads the `+` operator for Timetags and should not be called directly.
-- @usage local tt = Timetag.new(os.time()) + 0.25
Timetag.__add = function(a, b)
   if type(a) == "number" then
      return tt_add(b, a)
   end
   if type(b) == "number" then
      return tt_add(a, b)
   end
end

--- Low level API
-- @section low-level-api

--- Create a new Timetag.
--
-- @param[opt] tbl Table with timetag content.
-- @param[opt] seconds Seconds since January 1st 1900 in the UTC timezone.
-- @param[opt] fractions Fractions expressed as 1/2^32 of a second.
--
-- If there are no arguments a timetag with special value of "immediate" will be returned.
function Timetag.new_raw(...)
   local self = setmetatable({}, Timetag)
   local args = { ... }
   -- 0x0000000000000001 equals "now", so this is the default.
   self.content = { seconds = 0, fractions = 1 }
   if #args >= 1 then
      if type(args[1]) == "table" then
         self.content = args[1]
      elseif type(args[1]) == "number" and not args[2] then
         self.content.seconds = args[1]
      elseif type(args[1]) == "number" and type(args[2]) == "number" then
         self.content.seconds = args[1]
         self.content.fractions = args[2]
      end
   end
   return self
end

local Types = {}

--- Type pack functions.
--
-- Custom pack functions can be added to this table and standard functions can
-- be re-assigned if necessary.
--
-- This table can be called to pack a value in protected mode (pcall).
-- @usage local ok, data = Types.pack('s', 'hello')
-- if ok then
--   -- do something with data.
-- end
Types.pack = {}
setmetatable(Types.pack, {
   __call = function(self, type, value)
      return pcall(self[type], value)
   end,
})

--- Type unpack functions.
--
-- Custom unpack functions can be added to this table and standard functions
-- can be re-assigned if necessary.
--
-- This table can be called to unpack a value in protected mode (pcall).
-- @usage local ok, value, index = Types.unpack('s', data, 1)
-- if ok then
--   -- do something with value/index.
-- end
Types.unpack = {}
setmetatable(Types.unpack, {
   __call = function(self, type, data, offset)
      return pcall(self[type], data, offset)
   end,
})

--- Get available types.
-- @tparam table tbl `Types.unpack` or `Types.pack`
-- @return Table with available types.
-- @usage local types = Types.types(Types.pack)
-- @usage local types = Types.types(Types.unpack)
function Types.get(tbl)
   local types = {}
   for k, _ in pairs(tbl) do
      types[#types + 1] = k
   end
   return types
end

local function strsize(s)
   return 4 * (math.floor(#s / 4) + 1)
end

local function blobsize(b)
   return 4 * (math.floor((#b + 3) / 4))
end

--- Atomic types.
-- @section atomic-types

--- 32-bit big-endian two's complement integer.
-- @param value The value to pack.
-- @return Binary string buffer.
Types.pack.i = function(value)
   return _pack(">i4", value)
end

--- 32-bit big-endian two's complement integer.
-- @param data The data to unpack.
-- @param[opt] offset Initial offset into data.
-- @return value, index of the bytes read + 1.
Types.unpack.i = function(data, offset)
   return _unpack(">i4", data, offset)
end

--- 32-bit big-endian IEEE 754 floating point number.
-- @param value The value to pack.
-- @return Binary string buffer.
Types.pack.f = function(value)
   return _pack(">f", value)
end

--- 32-bit big-endian IEEE 754 floating point number.
-- @param data The data to unpack.
-- @param[opt] offset Initial offset into data.
-- @return value, index of the bytes read + 1.
Types.unpack.f = function(data, offset)
   return _unpack(">f", data, offset)
end

--- String (null terminated)
-- @param value The value to pack.
-- @return Binary string buffer.
Types.pack.s = function(value)
   local len = strsize(value)
   local fmt = "c" .. len
   value = value .. string.rep(string.char(0), len - #value)
   return _pack(">" .. fmt, value)
end

--- String (null terminated)
-- @param data The data to unpack.
-- @param[opt] offset Initial offset into data.
-- @return value, index of the bytes read + 1.
Types.unpack.s = function(data, offset)
   local fmt = has_string_pack and "z" or "s"
   local str = _unpack(">" .. fmt, data, offset)
   return str, strsize(str) + (offset or 1)
end

--- Blob (arbitrary binary data)
-- @param value The value to pack.
-- @return Binary string buffer.
Types.pack.b = function(value)
   local size = #value
   local aligned = blobsize(value)
   local fmt = "c" .. aligned
   value = value .. string.rep(string.char(0), aligned - size)
   return _pack(">I4" .. fmt, size, value)
end

--- Blob (arbitrary binary data)
-- @param data The data to unpack.
-- @param[opt] offset Initial offset into data.
-- @return value, index of the bytes read + 1.
Types.unpack.b = function(data, offset)
   local size, blob
   size, offset = _unpack(">I4", data, offset)
   blob, offset = _unpack(">c" .. size, data, offset)
   return blob, offset + blobsize(blob) - size
end

--- Extended types.
-- @section extended-types

if has_string_pack then
   --- 64 bit big-endian two's complement integer.
   --
   -- **NOTE** This type is only supported for lua >= 5.3.
   -- @param value The value to pack.
   -- @return Binary string buffer.
   Types.pack.h = function(value)
      return _pack(">i8", value)
   end
end

if has_string_pack then
   --- 64 bit big-endian two's complement integer.
   --
   -- **NOTE** This type is only supported for lua >= 5.3.
   -- @param data The data to unpack.
   -- @param[opt] offset Initial offset into data.
   -- @return value, index of the bytes read + 1.
   Types.unpack.h = function(data, offset)
      return _unpack(">i8", data, offset)
   end
end

--- Timetag (64-bit integer divided into upper and lower part)
-- @param value Table with seconds and fractions.
-- @return Binary string buffer.
-- @see losc.timetag
Types.pack.t = function(value)
   return Timetag.pack(value)
end

--- Timetag (64-bit integer divided into upper and lower part)
-- @param data The data to unpack.
-- @param[opt] offset Initial offset into data.
-- @return value, index of the bytes read + 1.
-- @see losc.timetag
Types.unpack.t = function(data, offset)
   return Timetag.unpack(data, offset)
end

--- 64-bit big-endian IEEE 754 floating point number.
-- @param value The value to pack.
-- @return Binary string buffer.
Types.pack.d = function(value)
   return _pack(">d", value)
end

--- 64-bit big-endian IEEE 754 floating point number.
-- @param data The data to unpack.
-- @param[opt] offset Initial offset into data.
-- @return value, index of the bytes read + 1.
Types.unpack.d = function(data, offset)
   return _unpack(">d", data, offset)
end

--- Boolean true.
-- This type does not have a corresponding `pack` method.
-- @param _ Not used.
-- @param[opt] offset Initial offset into data.
-- @return true (boolean) and byte offset (not incremented).
Types.unpack.T = function(_, offset)
   return true, offset or 0
end

--- Boolean false.
-- This type does not have a corresponding `pack` method.
-- @param _ Not used.
-- @param[opt] offset Initial offset into data.
-- @return false (boolean) and byte offset (not incremented).
Types.unpack.F = function(_, offset)
   return false, offset or 0
end

--- Nil.
-- This type does not have a corresponding `pack` method.
-- @param _ Not used.
-- @param[opt] offset Initial offset into data.
-- @return false (since nil cannot be represented in a lua table) and byte offset (not incremented).
Types.unpack.N = function(_, offset)
   -- TODO: decide on what to return here..
   return false, offset or 0
end

--- Infinitum.
-- This type does not have a corresponding `pack` method.
-- @param _ Not used.
-- @param[opt] offset Initial offset into data.
-- @return math.huge and byte offset (not incremented).
Types.unpack.I = function(_, offset)
   return math.huge, offset or 0
end
-- local _pack = Serializer.pack()
-- local _unpack = Serializer.unpack()

--- High level API
-- @section high-level-api

--- New using a seconds and fractions.
--
-- Given nil arguments will return a timetag with special value "immediate".
--
-- @tparam[opt] integer seconds Timestamp seconds.
-- @tparam[opt] integer fractions Timestamp fractions.
-- @tparam[opt] integer precision The fraction precision. default 1000 (`milliseconds`)
-- @usage local tt = Timetag.new() -- immediate
-- @usage local tt = Timetag.new(os.time())
-- @usage local tt = Timetag.new(tv.sec, tv.usec, 1e6)
-- @see Timetag.new_raw
function Timetag.new(seconds, fractions, precision)
   precision = precision or 1000
   if not seconds and not fractions then
      return Timetag.new_raw()
   end
   local secs, frac
   secs = (seconds or 0) + NTP_SEC_OFFSET
   frac = math.floor((fractions or 0) * (TWO_POW_32 / precision) + 0.5)
   return Timetag.new_raw(secs, frac)
end

--- Create a new OSC Timetag from a timestamp.
--
-- @param time The timestamp to use.
-- @param[opt] precision The fraction precision. default 1000
-- @return A Timetag object.
-- @usage local tt = Timetag.new_from_timestamp(time)
function Timetag.new_from_timestamp(time, precision)
   precision = precision or 1000
   local seconds = math.floor(time / precision)
   local fracs = math.floor(precision * (time / precision - seconds) + 0.5)
   return Timetag.new(seconds, fracs)
end

--- Get a timestamp value with arbitrary precision.
-- @param precision The precision to use. default 1000 (`milliseconds`)
-- @return Timestamp value.
-- @usage
-- local tt = Timetag.new(os.time(), 500)
-- local timestamp = tt:timestamp()
function Timetag:timestamp(precision)
   return Timetag.get_timestamp(self.content, precision)
end

--- Get seconds.
-- @return Timetag seconds.
function Timetag:seconds()
   return self.content.seconds
end

--- Get fractions.
-- @return Timetag fractions.
function Timetag:fractions()
   return self.content.fractions
end

--- Low level API
-- @section low-level-api

--- Get a timestamp with arbitrary precision.
-- @param tbl Table with seconds and fractions.
-- @param[opt] precision The fraction precision. default 1000
-- @return Timetag value.
function Timetag.get_timestamp(tbl, precision)
   precision = precision or 1000
   local seconds = precision * math.max(0, tbl.seconds - NTP_SEC_OFFSET)
   local fractions = math.floor(precision * (tbl.fractions / TWO_POW_32) + 0.5)
   return seconds + fractions
end

--- Pack an OSC Timetag.
--
-- The returned object is suitable for sending via a transport layer such as
-- UDP or TCP.
--
-- @tparam table tbl The timetag to pack.
-- @return OSC data packet (byte string).
-- @usage
-- local tt = {seconds = os.time(), fractions = 0}
-- local data = Timetag.pack(tt)
function Timetag.pack(tbl)
   local data = {}
   data[#data + 1] = _pack(">I4", tbl.seconds)
   data[#data + 1] = _pack(">I4", tbl.fractions)
   return table.concat(data, "")
end

--- Unpack an OSC Timetag.
--
-- @param data The data to unpack.
-- @param offset The initial offset into data.
-- @return First is a table with seconds and fractions, second is index of the bytes read + 1.
function Timetag.unpack(data, offset)
   local seconds, fractions
   seconds, offset = _unpack(">I4", data, offset)
   fractions, offset = _unpack(">I4", data, offset)
   return { seconds = seconds, fractions = fractions }, offset
end
local Message = {}
Message.__index = Message

--- High level API
-- @section high-level-api

--- Create a new OSC message.
--
-- @tparam[opt] string|table msg OSC address or table constructor.
--
-- @return An OSC message object.
-- @see losc.types
-- @usage
-- local msg = Message.new()
-- @usage
-- local msg = Message.new('/some/addr')
-- @usage
-- local tbl = {address = '/some/addr', types = 'ifs', 1, 2.0, 'hi'}
-- local msg = Message.new(tbl)
function Message.new(msg)
   local self = setmetatable({}, Message)
   self.content = {}
   self.content.address = ""
   self.content.types = ""
   if msg then
      if type(msg) == "string" then
         if msg:sub(1, 1) ~= "/" then
            msg = "/" .. msg
         end
         Message.address_validate(msg)
         self.content.address = msg
      elseif type(msg) == "table" then
         Message.tbl_validate(msg)
         self.content = msg
      end
   end
   return self
end

--- Add arguments to the message.
--
-- @param type OSC type string.
-- @param[opt] item Item to add.
-- @see losc.types
-- @usage message:add('i', 123)
-- @usage message:add('T')
function Message:add(type, item)
   self.content.types = self.content.types .. type
   if item then
      self.content[#self.content + 1] = item
   else
      if type == "T" or type == "F" then
         self.content[#self.content + 1] = type == "T"
      elseif type == "N" then
         self.content[#self.content + 1] = false
      elseif type == "I" then
         self.content[#self.content + 1] = math.huge
      end
   end
end

--- Message iterator.
--
-- Iterate over message types and arguments.
--
-- @return iterator using index, type, argument.
-- @usage for i, type, arg in message:iter() do
--   print(i, type, arg)
-- end
function Message:iter()
   local tbl = { self.content.types, self.content }
   local function msg_it(t, i)
      i = i + 1
      local type = t[1]:sub(i, i)
      local arg = t[2][i]
      if type ~= nil and arg ~= nil then
         return i, type, arg
      end
      return nil
   end
   return msg_it, tbl, 0
end

--- Get the OSC address.
-- @return The OSC address.
function Message:address()
   return self.content.address
end

--- Get the OSC type string.
-- @return OSC type string or empty string.
function Message:types()
   return self.content.types
end

--- Get the OSC arguments.
-- @return Table with arguments.
function Message:args()
   local args = {}
   for _, a in ipairs(self.content) do
      args[#args + 1] = a
   end
   return args
end

--- Validate a message.
-- @tparam table|string message The message to validate. Can be in packed or unpacked form.
function Message.validate(message)
   assert(message)
   if type(message) == "string" then
      Message.bytes_validate(message)
   elseif type(message) == "table" then
      Message.tbl_validate(message.content or message)
   end
end

--- Low level API
-- @section low-level-api

--- Validate an OSC message address.
-- @tparam string addr The address to validate.
function Message.address_validate(addr)
   assert(not addr:find "[%s#*,%[%]{}%?]", "Invalid characters found in address.")
end

--- Validate a table to be used as a message constructor.
-- @tparam table tbl The table to validate.
function Message.tbl_validate(tbl)
   assert(tbl.address, 'Missing "address" field.')
   Message.address_validate(tbl.address)
   assert(tbl.types, 'Missing "types" field.')
   assert(#tbl.types == #tbl, "Types and arguments mismatch")
end

--- Validate a binary string to see if it is a valid OSC message.
-- @tparam string bytes The byte string to validate.
-- @tparam[opt] integer offset Byte offset.
function Message.bytes_validate(bytes, offset)
   local value
   assert(#bytes % 4 == 0, "OSC message data must be a multiple of 4.")
   value, offset = Types.unpack.s(bytes, offset)
   assert(value:sub(1, 1) == "/", "Invalid OSC address.")
   value = Types.unpack.s(bytes, offset)
   assert(value:sub(1, 1) == ",", "Error: malformed type tag.")
end

--- Pack a table to a byte string.
--
-- The returned object is suitable for sending via a transport layer such as
-- UDP or TCP.
--
-- Call `Message.validate()` before passing arguments to this function to
-- ensure that the table is suitable for packing.
--
-- @param tbl The content to pack.
-- @return OSC data packet (byte string).
function Message.pack(tbl)
   local packet = {}
   local address = tbl.address
   local types = tbl.types
   -- types
   packet[#packet + 1] = Types.pack.s(address)
   packet[#packet + 1] = Types.pack.s("," .. types)
   -- arguments
   local index = 1
   for type in types:gmatch "." do
      local item = tbl[index]
      if item ~= nil then
         if Types.pack[type] then
            packet[#packet + 1] = Types.pack[type](item)
         end
         index = index + 1
      end
   end
   return table.concat(packet, "")
end

--- Unpack OSC message byte string.
--
-- Call `Message.validate()` before passing arguments to this function to
-- ensure that the table is suitable for unpacking.
--
-- @param data The data to unpack.
-- @param offset The initial offset into data.
-- @return table with the content of the OSC message.
function Message.unpack(data, offset)
   local value
   local message = {}
   -- address
   value, offset = Types.unpack.s(data, offset)
   message.address = value
   -- type tag
   value, offset = Types.unpack.s(data, offset)
   local types = value:sub(2) -- remove prefix
   message.types = types
   -- arguments
   for type in types:gmatch "." do
      if Types.unpack[type] then
         value, offset = Types.unpack[type](data, offset)
         message[#message + 1] = value
      end
   end
   return message, offset
end

local Bundle = {}
Bundle.__index = Bundle

local ts = Timetag.get_timestamp

--- Pack a Bundle recursively.
local function _pack(bundle, packet)
   packet[#packet + 1] = Types.pack.s "#bundle"
   packet[#packet + 1] = Types.pack.t(bundle.timetag)
   for _, item in ipairs(bundle) do
      if item.address and item.types then
         local message = Message.pack(item)
         packet[#packet + 1] = Types.pack.i(#message)
         packet[#packet + 1] = message
      elseif item.timetag then
         if ts(item.timetag) < ts(bundle.timetag) then
            error "Bundle timetag is less than enclosing bundle."
         end
         local bndl = Bundle.pack(item)
         packet[#packet + 1] = Types.pack.i(#bndl)
         packet[#packet + 1] = bndl
      end
   end
   return table.concat(packet, "")
end

--- Unpack a Bundle recursively.
local function _unpack(data, bundle, offset, length)
   local value, _
   _, offset = Types.unpack.s(data, offset)
   value, offset = Types.unpack.t(data, offset)
   bundle.timetag = value
   length = length or #data
   while offset < length do
      -- content length
      value, offset = Types.unpack.i(data, offset)
      local head = data:sub(offset, offset)
      if head == "#" then
         value, offset = _unpack(data, {}, offset, offset + value - 1)
         bundle[#bundle + 1] = value
      elseif head == "/" then
         value, offset = Message.unpack(data, offset)
         bundle[#bundle + 1] = value
      end
   end
   return bundle, offset
end

--- High level API
-- @section high-level-api

--- Create a new OSC bundle.
--
-- Arguments can be one form of:
--
-- 1. nil (return empty bundle object).
-- 2. Timetag.
-- 3. Timetag, message/bundle objects.
--
-- @param[opt] ... arguments.
-- @return Bundle object.
function Bundle.new(...)
   local self = setmetatable({}, Bundle)
   local args = { ... }
   self.content = {}
   if #args >= 1 then
      self.content.timetag = args[1].content
      for index = 2, #args do
         self:add(args[index])
      end
   end
   return self
end

--- Adds an item to the bundle.
-- @param item Can be a Message or another bundle.
function Bundle:add(item)
   self.content[#self.content + 1] = item.content
end

--- Get or set the bundle Timetag.
-- @param[opt] tt A Timetag object.
-- If no parameter is given it returns the current Timetag.
function Bundle:timetag(tt)
   if tt then
      self.content.timetag = tt.content
   else
      return Timetag.new_raw(self.content.timetag)
   end
end

--- Validate a bundle.
-- @tparam table|string bundle The bundle to validate. Can be in packed or unpacked form.
function Bundle.validate(bundle)
   assert(bundle)
   if type(bundle) == "string" then
      Bundle.bytes_validate(bundle)
   elseif type(bundle) == "table" then
      Bundle.tbl_validate(bundle.content or bundle)
   end
end

--- Low level API
-- @section low-level-api

--- Validate a table that can be used as an OSC bundle.
-- @param tbl The table to validate.
function Bundle.tbl_validate(tbl)
   assert(type(tbl.timetag) == "table", "Missing OSC Timetag.")
end

--- Validate a byte string that can be unpacked to an OSC bundle.
-- @param data The byte string to validate.
-- @param[opt] offset Byte offset.
function Bundle.bytes_validate(data, offset)
   local value
   assert(#data % 4 == 0, "OSC bundle data must be a multiple of 4.")
   value, offset = Types.unpack.s(data, offset or 1)
   assert(value == "#bundle", "Missing bundle marker")
   value = Types.unpack.t(data, offset)
   assert(type(value) == "table", "Missing bundle timetag")
end

--- Pack an OSC bundle.
--
-- The returned object is suitable for sending via a transport layer such as
-- UDP or TCP.
--
-- @param tbl The content to pack.
-- @return OSC data packet (byte string).
function Bundle.pack(tbl)
   local packet = {}
   return _pack(tbl, packet)
end

--- Unpack an OSC bundle byte string.
--
-- @param data The data to unpack.
-- @param offset The initial offset into data.
-- @return table with the content of the OSC bundle.
function Bundle.unpack(data, offset)
   local bundle = {}
   return _unpack(data, bundle, offset or 1)
end

local Packet = {}

--- Check if a packet is a bundle or a message.
-- @tparam string|table packet The packet to check.
-- @return True if packet is a bundle otherwise false.
function Packet.is_bundle(packet)
   if type(packet) == "string" then
      local value = Types.unpack.s(packet)
      return value == "#bundle"
   elseif type(packet) == "table" then
      packet = packet.content or packet
      return type(packet.timetag) == "table"
   end
end

--- Validate a packet. Can be a message or a bundle.
-- @tparam string|table packet The packet to validate.
function Packet.validate(packet)
   if Packet.is_bundle(packet) then
      Bundle.validate(packet)
   else
      Message.validate(packet)
   end
end

--- Pack a Bundle or Message.
-- @param tbl The table to pack.
-- @return OSC data packet (byte string).
function Packet.pack(tbl)
   if Packet.is_bundle(tbl) then
      Bundle.validate(tbl)
      return Bundle.pack(tbl.content or tbl)
   else
      Message.validate(tbl)
      return Message.pack(tbl.content or tbl)
   end
end

--- Unpack an OSC packet.
-- @tparam string data The data to unpack.
-- @return table with the content of the OSC message (bundle or message).
function Packet.unpack(data)
   if Packet.is_bundle(data) then
      Bundle.validate(data)
      return Bundle.unpack(data)
   else
      Message.validate(data)
      return Message.unpack(data)
   end
end

local Pattern = {}

-- local ts = Timetag.get_timestamp

--- Escape magic lua characters from a pattern.
-- @tparam string pattern The pattern to escape.
-- @return A string with all magic lua characters escaped and OSC wildcards
-- converted to lua pattern matching wildcards.
function Pattern.escape(pattern)
   -- escape lua magic chars (order matters)
   pattern = pattern:gsub("%%", "%%%%")
   pattern = pattern:gsub("%.", "%%.")
   pattern = pattern:gsub("%(", "%%(")
   pattern = pattern:gsub("%)", "%%)")
   pattern = pattern:gsub("%+", "%%+")
   pattern = pattern:gsub("%$", "%%$")
   -- convert osc wildcards to lua patterns
   pattern = pattern:gsub("%*", ".*")
   pattern = pattern:gsub("%?", ".")
   pattern = pattern:gsub("%[!", "[^")
   pattern = pattern:gsub("%]", "]+")
   return pattern
end

local function match(key, address)
   local result = address:match(key) == address
   -- try and match group instead
   if not result and key:find "{" then
      local index = 1
      local tmps = ""
      for c in key:gmatch "." do
         local a = address:sub(index, index)
         if a == c then
            tmps = tmps .. c
            index = index + 1
         end
      end
      result = tmps == address
   end
   return result
end

local function invoke(message, timestamp, plugin)
   local address = message.address
   local now = plugin:now():timestamp(plugin.precision)
   local ignore_late = plugin.options.ignore_late or false
   if ignore_late and timestamp > 0 and timestamp < now then
      return
   end
   if plugin.handlers then
      for _, handler in pairs(plugin.handlers) do
         if match(handler.pattern, address) then
            plugin:schedule(timestamp - now, function()
               handler.callback {
                  timestamp = now,
                  message = message,
                  remote_info = plugin.remote_info or {},
               }
            end)
         end
      end
   end
end

local function dispatch(packet, plugin)
   if Packet.is_bundle(packet) then
      for _, item in ipairs(packet) do
         if Packet.is_bundle(item) then
            if ts(item.timetag, plugin.precision) >= ts(packet.timetag, plugin.precision) then
               dispatch(item, plugin)
            else
               error "Bundle timestamp is older than timestamp of enclosing bundle"
            end
         else
            invoke(item, ts(packet.timetag, plugin.precision), plugin)
         end
      end
   else
      invoke(packet, 0, plugin)
   end
end

--- Dispatch OSC packets.
-- @tparam string data Packed OSC data byte string.
-- @tparam table plugin The plugin to dispatch the message through.
function Pattern.dispatch(data, plugin)
   local packet = Packet.unpack(data)
   dispatch(packet, plugin)
end

-- local losc = {
--    _VERSION = "losc v1.0.1",
--    _URL = "https://github.com/davidgranstrom/losc",
--    _DESCRIPTION = "Open Sound Control (OSC) library for lua/luajit.",
--    _LICENSE = [[
--     MIT License
--
--     Copyright (c) 2021 David Granström
--
--     Permission is hereby granted, free of charge, to any person obtaining a copy
--     of this software and associated documentation files (the "Software"), to deal
--     in the Software without restriction, including without limitation the rights
--     to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--     copies of the Software, and to permit persons to whom the Software is
--     furnished to do so, subject to the following conditions:
--
--     The above copyright notice and this permission notice shall be included in all
--     copies or substantial portions of the Software.
--
--     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--     IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--     FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--     AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--     LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--     OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--     SOFTWARE.
--   ]],
-- }
losc.__index = losc

--- Create a new instance.
-- @tparam[options] table options Options.
-- @usage local osc = losc.new()
-- @usage local osc = losc.new {plugin = plugin.new()}
function losc.new(options)
   local self = setmetatable({}, losc)
   self.handlers = {}
   if options then
      if options.plugin then
         self:use(options.plugin)
      end
   end
   return self
end

--- Create a new Message.
-- @tparam[opt] string|table args OSC address or table constructor.
-- @return message object
-- @see losc.message
-- @usage local message = losc.new_message()
-- @usage local message = losc.new_message('/address')
-- @usage local message = losc.new_message{address = '/test', types = 'iif', 1, 2, 3}
function losc.new_message(args)
   local ok, message = pcall(Message.new, args)
   if not ok then
      error(message)
   end
   return message
end

--- Create a new OSC bundle.
-- @param[opt] ... arguments.
-- @return bundle object
-- @see losc.bundle
-- @usage
-- local tt = losc:now()
-- local bundle = losc.new_bundle()
-- bundle:timetag(tt)
-- -- packet can be a message or another bundle
-- bundle:add(packet)
-- @usage
-- local tt = losc:now()
-- local bundle = losc.new_bundle(tt)
-- bundle:add(packet)
-- @usage
-- local tt = losc:now()
-- local bundle = losc.new_bundle(tt, packet, packet2)
function losc.new_bundle(...)
   local ok, bundle = pcall(Bundle.new, ...)
   if not ok then
      error(bundle)
   end
   return bundle
end

--- Specify a plugin to use as transport layer.
-- @param plugin The plugin to use, pass nil to disable current plugin.
function losc:use(plugin)
   self.plugin = plugin
   if plugin then
      self.plugin.handlers = self.handlers
   end
end

--- Get an OSC timetag with the current timestamp.
-- Will fall back to `os.time()` if `now()` is not implemented by the plugin
-- in use.
-- @usage local tt = losc:now()
-- @usage
-- -- 0.25 seconds into the future.
-- local tt = losc:now() + 0.25
function losc:now()
   if self.plugin and self.plugin.now then
      return self.plugin:now()
   end
   return Timetag.new(os.time(), 0)
end

--- Opens an OSC server.
-- This function might be blocking depending on the plugin in use.
-- @param[opt] ... Plugin specific arguments.
-- @return status, plugin handle or error
-- @usage losc:open()
function losc:open(...)
   if not self.plugin then
      error '"open" must be implemented using a plugin.'
   end
   return pcall(self.plugin.open, self.plugin, ...)
end

--- Closes an OSC server.
-- @param[opt] ... Plugin specific arguments.
-- @return status, nil or error
-- @usage losc:close()
function losc:close(...)
   if not self.plugin then
      error '"close" must be implemented using a plugin.'
   end
   return pcall(self.plugin.close, self.plugin, ...)
end

--- Send an OSC packet.
-- @param[opt] ... Plugin specific arguments.
-- @return status, nil or error
-- @usage
-- -- can be message or bundle.
-- local packet = losc.new_message{address = '/x', types = 'i', 1}
-- losc:send(packet)
-- -- additional plugin arguments (can vary between plugins)
-- losc:send(packet, 'localhost', 9000)
function losc:send(...)
   if not self.plugin then
      error '"send" must be implemented using a plugin.'
   end
   return pcall(self.plugin.send, self.plugin, ...)
end

--- Add an OSC handler.
-- @param pattern The pattern to match on.
-- @param func The callback to run if a message is received.
-- The callback will get a single argument `data` from where the message can be retrieved.
-- @usage
-- osc:add_handler('/pattern', function(data)
--   -- message table, can be converted to Message if needed.
--   local message = data.message
--   -- timestamp when message was received, can be converted to Timetag if needed.
--   local timestamp = data.timestamp
--   -- table with remote (sender) info, can be empty depending on plugin.
--   local remote_info = data.remote_info
-- end)
-- @usage
-- osc:add_handler('/pattern', function(data)
--   -- arguments can be accessed by index from the message table
--   local arg1 = data.message[1]
--   local arg2 = data.message[2]
--   -- iterate over incoming OSC arguments
--   for _, argument in ipairs(data.message) do
--     print(argument)
--   end
-- end)
-- @usage
-- -- Pattern matching (groups)
-- osc:add_handler('/param/{x,y}/123', function(data) end)
-- -- Pattern matching (sequence)
-- osc:add_handler('/param/[a-f]/123', function(data) end)
-- -- Pattern matching (sequence)
-- osc:add_handler('/param/[!a-f]/123', function(data) end)
-- -- Pattern matching (wildcard)
-- osc:add_handler('/param/*/123', function(data) end)
-- osc:add_handler('*', function(data) end)
function losc:add_handler(pattern, func)
   self.handlers[pattern] = {
      pattern = Pattern.escape(pattern),
      callback = func,
   }
   if self.plugin then
      self.plugin.handlers = self.handlers
   end
end

--- Remove an OSC handler.
-- @param pattern The pattern for the handler to remove.
-- @usage losc:remove_handler('/handler/to/remove')
function losc:remove_handler(pattern)
   self.handlers[pattern] = nil
   if self.plugin then
      self.plugin.handlers[pattern] = nil
   end
end

--- Remove all registered OSC handlers.
-- @usage losc:remove_all()
function losc:remove_all()
   self.handlers = {}
   if self.plugin then
      self.plugin.handlers = {}
   end
end
losc.Timetag = Timetag
losc.Bundle = Bundle
losc.Message = Message
losc.Pattern = Pattern
losc.Packet = Packet

return losc
