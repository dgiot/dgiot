-- TODO: ?! Is this benchmark still relevant?

local tostring, assert, loadstring, ipairs
    = tostring, assert, loadstring, ipairs

local table_concat = table.concat
local math_floor = math.floor

local noop = function() end

local plain_call = function(a) noop() end

local make_plain_call = function()
  return plain_call
end

local make_elseifs = function(n)
  local buf = { }
  local _ = function(v) buf[#buf + 1] = tostring(v) end
  _ "local noop = function() end "
  _ "return function(a)"
  _ " if a == 'a" _(1) _"' then noop()"
  for i = 2, n do
    _ " elseif a == 'a" _(i) _"' then noop()"
  end
  _ " else error('unknown param') end"
  _ " end"
  return assert(loadstring(table_concat(buf)))()
end

local make_callmaps = function(n)
  local buf = { }
  local _ = function(v) buf[#buf + 1] = tostring(v) end
  _ "local noop = function() end "
  _ "local map = {"
  for i = 1, n do
    _ "a" _(i) _ " = noop;"
  end
  _ "} "
  _ "return function(a) assert(map[a])() end"
  return assert(loadstring(table_concat(buf)))()
end

local bench_fn = function(fn, n)
  return function()
    fn("a" .. math_floor(n / 2 + 0.5))
  end
end

local mark_fn = function(make, n)
  return bench_fn(make(n), n)
end

bench = { }

for _, i in ipairs { 1, 5, 10, 15, 20, 100, 250, 500, 1000 } do
  bench["plain_"..i] = mark_fn(make_plain_call, i)
  bench["callmap_"..i] = mark_fn(make_callmaps, i)
  bench["elseif_"..i] = mark_fn(make_elseifs, i)
end

return bench
