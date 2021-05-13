local rawget = rawget

local t = { true, a = true }

local get = function(t, k)
  return t[k]
end

bench = { }

bench.nonnil_num_plain = function()
  return t[1]
end

bench.nonnil_num_get = function()
  return get(t, 1)
end

bench.nonnil_num_rawget = function()
  return rawget(t, 1)
end

bench.nonnil_str_plain = function()
  return t["a"]
end

bench.nonnil_str_sugar = function()
  return t.a
end

bench.nonnil_str_get = function()
  return get(t, "a")
end

bench.nonnil_str_rawget = function()
  return rawget(t, "a")
end

bench.nil_num_plain = function()
  return t[2]
end

bench.nil_num_get = function()
  return get(t, 2)
end

bench.nil_num_rawget = function()
  return rawget(t, 2)
end

bench.nil_str_plain = function()
  return t["b"]
end

bench.nil_str_sugar = function()
  return t.b
end

bench.nil_str_get = function()
  return get(t, "b")
end

bench.nil_str_rawget = function()
  return rawget(t, "b")
end

return bench
