-- stng_decomp_C02A
-- Shows graphics decompression addresses in Lua Script window of Gens emulator for Genesis game Star Trek - The Next Generation (U) (REV01) [!]
-- Usage: 
-- Emulator Gens r57shell Mod (available at romhacking.net). Tools -> Lua Scripting -> New Lua Script Window -> Browse... Choose this script. Launch ROM in emulator and observe log information.

local DECOMP_ADDR = 0xC02A

-- ---------- Utilities ----------
local function tohex32(v)
  if v == nil then return "(nil)" end
  if type(v) ~= "number" then return tostring(v) end
  if v < 0 then v = v + 0x100000000 end
  v = math.floor(v % 0x100000000)
  return string.format("0x%08X", v)
end

local function get_framecount_safe()
  local ok, v = pcall(function() if emu and emu.framecount then return emu.framecount() end end)
  if ok and v then return v end
  ok, v = pcall(function() if gens and gens.framecount then return gens.framecount() end end)
  if ok and v then return v end
  ok, v = pcall(function() if framecount then return framecount() end end)
  if ok and v then return v end
  return -1
end

local function read_u8_safe(addr)
  if not addr then return nil end
  local fn_variants = {
    function(a) return memory.read_u8(a) end,
    function(a) return memory.readbyte(a) end,
    function(a) return memory.read_byte(a) end,
    function(a) return memory.getbyte and memory.getbyte(a) end,
    function(a) return memory.read_u8 and memory.read_u8(a) end,
  }
  for _, fn in ipairs(fn_variants) do
    local ok, val = pcall(function() return fn(addr) end)
    if ok and (val ~= nil) then
      return tonumber(val)
    end
  end
  return nil
end

local function read_u32_be_safe(addr)
  if not addr then return nil end
  local try32 = {
    function(a) return memory.read_u32_be(a) end,
    function(a) return memory.read_u32(a) end,
    function(a) return memory.read_dword and memory.read_dword(a) end,
    function(a) return memory.get_u32 and memory.get_u32(a) end,
  }
  for _, fn in ipairs(try32) do
    local ok, val = pcall(function() return fn(addr) end)
    if ok and (val ~= nil) then
      local n = tonumber(val)
      if n then
        if n < 0 then n = n + 0x100000000 end
        return math.floor(n % 0x100000000)
      end
    end
  end
  -- fallback to 4 byte reads
  local b0 = read_u8_safe(addr)
  local b1 = read_u8_safe((addr or 0) + 1)
  local b2 = read_u8_safe((addr or 0) + 2)
  local b3 = read_u8_safe((addr or 0) + 3)
  if b0 and b1 and b2 and b3 then
    return ((b0 * 0x1000000) + (b1 * 0x10000) + (b2 * 0x100) + b3) % 0x100000000
  end
  return nil
end

local function read_a0_register()
  local tries = {
    function() return memory.getregister("a0") end,
    function() return memory.getregister("m68k.a0") end,
    function() return memory.getregister("68k.a0") end,
    function() return getregister and getregister("a0") end,
    function() return cpu and cpu.register and cpu.register("a0") end,
  }
  for _, fn in ipairs(tries) do
    local ok, val = pcall(function() return fn() end)
    if ok and (val ~= nil) then return tonumber(val) end
  end
  return nil
end

-- ---------- Reset marker (no clearing) ----------
local last_reset_frame = nil

local function print_reset_divider()
  local cur_frame = get_framecount_safe() or -1
  -- prevent duplicate prints if emulator fires many reset callbacks in one frame
  if cur_frame ~= -1 and cur_frame == last_reset_frame then
    return
  end
  last_reset_frame = cur_frame

  -- Print a visible divider + reset info + header. Do NOT clear previous console lines.
  print(string.rep("=", 41))
  if cur_frame ~= -1 then
    print(string.format("=== RESET detected at frame %d: New log session starts here ===", cur_frame))
  else
    print("=== RESET detected: New log session starts here ===")
  end
  print(string.format("Log of graphics decompression routine calls @0x%04X:", DECOMP_ADDR))
  print(string.rep("-", 41))
end

-- Try to register emulator reset/start hooks (stop after first successful registration)
local reset_registered = false
local reset_register_attempts = {
  function() if emu and emu.registerreset then return pcall(function() emu.registerreset(print_reset_divider) end) end end,
  function() if emu and emu.registerstart then return pcall(function() emu.registerstart(print_reset_divider) end) end end,
  function() if gens and gens.registerreset then return pcall(function() gens.registerreset(print_reset_divider) end) end end,
  function() if event and event.onreset then return pcall(function() event.onreset(print_reset_divider) end) end end,
  function() if event and event.register then return pcall(function() event.register("reset", print_reset_divider) end) end end,
}
for _, fn in ipairs(reset_register_attempts) do
  local ok, res = pcall(function() return fn() end)
  if ok and res then
    reset_registered = true
    break
  end
end

-- If no reset hook was available, print header once now (so user has initial header)
if not reset_registered then
  print(string.format("Log of graphics decompression routine calls @0x%04X:", DECOMP_ADDR))
end

-- ---------- Decomp exec callback ----------
local function on_decomp_exec()
  local a0 = read_a0_register()
  local size = nil
  if a0 then size = read_u32_be_safe(a0) end
  local frame = get_framecount_safe() or -1
  local s_src = a0 and tohex32(a0) or "(A0 read failed)"
  local s_size = (size ~= nil) and tohex32(size) or "(size read failed)"
  print(string.format("Source=%s  Size=%s  Frame=%d", s_src, s_size, frame))
end

-- Try to register exec callback on DECOMP_ADDR
local registered_exec = false
local reg_attempts = {
  function() if memory and memory.registerexec then return pcall(function() memory.registerexec(DECOMP_ADDR, on_decomp_exec) end) end end,
  function() if registerexec then return pcall(function() registerexec(DECOMP_ADDR, on_decomp_exec) end) end end,
  function() if gens and gens.registerexec then return pcall(function() gens.registerexec(DECOMP_ADDR, on_decomp_exec) end) end end,
  function() if memory and memory.registerexecute then return pcall(function() memory.registerexecute(DECOMP_ADDR, on_decomp_exec) end) end end,
}
for _, f in ipairs(reg_attempts) do
  local ok, res = pcall(function() return f() end)
  if ok and res then
    registered_exec = true
    break
  end
end

if not registered_exec then
  print(string.format("Warning: couldn't register exec callback at 0x%04X. Callback registration failed on this build.", DECOMP_ADDR))
  print("You will still get resets marked (if frame fallback detects a reset), but log entries require registerexec API.")
end

-- ---------- Frame loop: fallback reset detection ----------
local last_frame = get_framecount_safe()
while true do
  local cur_frame = get_framecount_safe()
  if cur_frame and last_frame and cur_frame < last_frame then
    print_reset_divider()
  end
  last_frame = cur_frame
  emu.frameadvance()
end
