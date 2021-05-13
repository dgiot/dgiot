local empty_string = ""

bench = { }

bench.noop = function()
  local a = ""
  return true
end

bench.empty_constant = function()
  local a = ""
  return a == ""
end

bench.empty_upvalue = function()
  local a = ""
  return a == empty_string
end

bench.empty_size = function()
  local a = ""
  return #a == 0
end

bench.nonempty_constant = function()
  local a = "nonempty"
  return a == ""
end

bench.nonempty_upvalue = function()
  local a = "nonempty"
  return a == empty_string
end

bench.nonempty_size = function()
  local a = "nonempty"
  return #a == 0
end

return bench
