local noop = function() end
local vararg_callback = function(...) end

local call_noargs = function(fn) fn() end
local call_vararg = function(fn, ...) fn(...) end
local call_3 = function(fn, a, b, c) fn(a, b, c) end

NUM_ITER = 1000

bench = { }

bench.noop = noop

bench.vararg_callback = vararg_callback

bench.call_noargs_noop_nil = function()
  call_noargs(noop)
end

bench.call_noargs_vararg_nil = function()
  call_noargs(vararg_callback)
end

bench.call_vararg_noop_nil = function()
  call_noargs(noop)
end

bench.call_vararg_vararg_nil = function()
  call_noargs(vararg_callback)
end

bench.call_3_noop_nil = function()
  call_3(noop)
end

bench.call_3_vararg_nil = function()
  call_3(vararg_callback)
end

bench.call_noargs_noop_3 = function()
  call_noargs(noop, 1, 2, 3)
end

bench.call_noargs_vararg_3 = function()
  call_noargs(vararg_callback, 1, 2, 3)
end

bench.call_vararg_noop_3 = function()
  call_noargs(noop, 1, 2, 3)
end

bench.call_vararg_vararg_3 = function()
  call_noargs(vararg_callback, 1, 2, 3)
end

bench.call_3_noop_3 = function()
  call_3(noop, 1, 2, 3)
end

bench.call_3_vararg_3 = function()
  call_3(vararg_callback, 1, 2, 3)
end

return bench
