--------------------------------------------------------------------------------
-- tclone.lua: tclone benchmark
--------------------------------------------------------------------------------

local type, pairs, assert, error, unpack, next, newproxy
    = type, pairs, assert, error, unpack, next, newproxy

local math_randomseed, math_random
    = math.randomseed, math.random

local string_char = string.char
local table_concat = table.concat
-- local coroutine_create = coroutine.create

--------------------------------------------------------------------------------

math_randomseed(123456)

--------------------------------------------------------------------------------

--
-- Current lua-nucleo version (a166af)
--
local tclone_nucleo
do
  local function impl(t, visited)
    local t_type = type(t)
    if t_type ~= "table" then
      return t
    end

    assert(not visited[t], "recursion detected")
    visited[t] = true

    local r = { }
    for k, v in pairs(t) do
      r[impl(k, visited)] = impl(v, visited)
    end

    visited[t] = nil

    return r
  end

  tclone_nucleo = function(t)
    return impl(t, { })
  end
end

--
-- tclone2 by Dimiter "malkia" Stanev
-- http://article.gmane.org/gmane.comp.lang.lua.general/82378
--
local tclone2
do
  local function impl(t, visited, rtimes)
    local t_type = type(t)
    if t_type ~= "table" then
      return t
    end

    -- Don't remember all visited[t] levels
    -- Just remember every once in a 128 times
    -- If there is a recursion it'll still be detected
    -- But 128 stack levels deeper
    assert(not visited[t], "recursion detected (with some latency)")

    if rtimes == 128 then
      rtimes = 1
      visited[t] = t
    end

    local r = { }
    for k, v in pairs(t) do
      r[impl(k, visited, rtimes + 1)] = impl(v, visited, rtimes + 1)
    end

    if rtimes == 1 then
      visited[t] = nil
    end

    return r
  end

  tclone2 = function(t)
    return impl(t, { }, 1)
  end
end

--
-- tclone5 by Dimiter "malkia" Stanev
-- http://article.gmane.org/gmane.comp.lang.lua.general/82379
--
local tclone5
do
  local function impl(t, visited, rtimes)
    if visited[t] then
      error("recursion detected")
    end

    if rtimes == 128 then
      rtimes = 1
      visited[t] = true
    end

    local r = { }

    for k, v in pairs(t) do
      if type(k) == "table"  then
        if type(v) == "table" then
          r[impl(k, visited, rtimes + 1)] = impl(v, visited, rtimes + 1)
        else
          r[impl(k, visited, rtimes + 1)] = v
        end
      elseif type(v) == "table" then
        r[k] = impl(v, visited, rtimes + 1)
      else
        r[k] = v
      end
    end

    if rtimes == 1 then
      visited[t] = nil
    end

    return r
  end

  tclone5 = function(t)
    if type(t) == "table" then
      return impl(t, { }, 1)
    end

    return t
  end
end

--
-- tclone6 by Dimiter "malkia" Stanev
-- http://article.gmane.org/gmane.comp.lang.lua.general/82601
-- With a fix in while condition
--
local tclone6
do
  local function impl(t, visited, rtimes)
    if visited[t] then
      error("recursion detected")
    end

    if rtimes == 128 then
      rtimes = 1
      visited[t] = true
    end

    local r = { }
    local k, v = next(t)
    while k ~= nil do
      if type(k) == "table"  then
        if type(v) == "table" then
          r[impl(k, visited, rtimes + 1)] = impl(v, visited, rtimes + 1)
        else
          r[impl(k, visited, rtimes + 1)] = v
        end
      elseif type(v) == "table" then
        r[k] = impl(v, visited, rtimes + 1)
      else
        r[k] = v
      end
      k, v = next(t, k)
    end

    if rtimes == 1 then
      visited[t] = nil
    end

    return r
  end

  tclone6 = function(t)
    if type(t) == "table" then
      return impl(t, { }, 1)
    end

    return t
  end
end
--------------------------------------------------------------------------------

-- TODO: From lua-nucleo/test/table.lua.
-- Make that avaliable to other projects and reuse.
local function gen_random_dataset(num, nesting, visited, random)
  random = random or math_random

  nesting = nesting or 1
  visited = visited or {}
  num = num or random(0, 10)

  local gen_str = function()
    local len = random(1, 64)
    local t = {}
    for i = 1, len do
      t[i] = string_char(random(0, 255))
    end
    return table_concat(t)
  end

  local gen_bool = function() return random() >= 0.5 end
  local gen_udata = function() return newproxy() end
  local gen_func = function() return function() end end
--  local gen_thread = function() return coroutine_create(function() end) end
  local gen_nil = function() return nil end
  local gen_visited_link = function()
    if #visited > 1 then
      return visited[random(1, #visited)]
    else
      return gen_str()
    end
  end

  local generators =
  {
    gen_bool;
    gen_bool;
    gen_bool;
    function() return random(-10, 10) end;
    gen_str;
    gen_str;
    gen_str;
   --[[ gen_thread;
    gen_thread;
    gen_func;
    gen_func;
    gen_udata;
    gen_udata;--]]
    --gen_visited_link;
    function()
      if nesting >= 10 then
        return nil
      end
      local t = { }
      visited[#visited + 1] = t
      local n = random(0, 10 - nesting)
      for i = 1, n do
        local k = gen_random_dataset(1, nesting + 1, visited, random)
        if k == nil then
          k = "(nil)"
        end
        t[k] = gen_random_dataset(1, nesting + 1, visited, random)
      end
      return t
    end
  }

  local t = {}
  visited[#visited + 1] = t

  for i = 1, num do
    local n = random(1, #generators)
    t[i] = generators[n]()
  end

  return unpack(t, 1, num)
end

--------------------------------------------------------------------------------

local DATA = { gen_random_dataset(20) }

--------------------------------------------------------------------------------

bench = { }

--------------------------------------------------------------------------------

bench.lua_nucleo = function()
  local data = tclone_nucleo(DATA)
  assert(data ~= DATA)
  -- TODO: Check equality.
end

bench.tclone2 = function()
  local data = tclone2(DATA)
  assert(data ~= DATA)
  -- TODO: Check equality.
end

bench.tclone5 = function()
  local data = tclone5(DATA)
  assert(data ~= DATA)
  -- TODO: Check equality.
end

bench.tclone6 = function()
  local data = tclone5(DATA)
  assert(data ~= DATA)
  -- TODO: Check equality.
end

--------------------------------------------------------------------------------

return bench
