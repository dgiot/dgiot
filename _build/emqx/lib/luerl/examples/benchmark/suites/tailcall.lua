local ret = function(t)
  t[1] = true
  return t
end

local noret = function(t)
  t[1] = true
end

bench = { }

bench.tailcall_local = function()
  local t = {}
  return ret(t)
end

bench.tailcall_nolocal = function()
  return ret({})
end

bench.notailcall_return = function()
  local t = {}
  ret(t)
  return t
end

bench.notailcall_noreturn = function()
  local t = {}
  noret(t)
  return t
end

return table.pack(bench)
