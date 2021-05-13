bench = {}

local function no_ret()
end

local function ret_nil()
  return nil
end

local function ret_true()
  return true
end

local function ret_self()
  return ret_self
end

bench.no_ret = function()
  local a = no_ret()
end

bench.ret_nil = function()
  local a = ret_nil()
end

bench.ret_true = function()
  local a = ret_true()
end

bench.ret_self = function()
  local a = ret_self()
end

return bench
