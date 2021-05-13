local setmetatable = setmetatable

local mt =
{
  __call = function(t, v)
    t[#t + 1] = v
  end
}

local call_setmetatable = function()
  return setmetatable({ }, mt)
end

local create_closure = function()
  local t = {}
  return function(v) t[#t + 1] = v end
end

local mt_obj = call_setmetatable()
local fn_obj = create_closure()

bench = { }

bench.call_setmetatable = call_setmetatable

bench.create_closure = create_closure

-- bench.use_setmetatable = function()
--  mt_obj("boo!")
-- end

bench.use_closure = function()
  fn_obj("boo!")
end

return bench
