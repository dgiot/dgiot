local select = select

local select_test = function(...)
  local nargs = select("#", ...)
  local r = { }
  for i = 1, nargs do
    r[#r + 1] = select(i, ...) * 2
  end
  return r
end

local table_test = function(...)
  local nargs = select("#", ...) -- Still have to do this in case of nils
  local args = { ... }
  local r = { }
  for i = 1, nargs do
    r[#r + 1] = args[i] * 2
  end
  return r
end

bench = { }

bench.select = function()
  return select_test(3, 5, 1, 9, 7)
end

bench.table = function()
  return table_test(3, 5, 1, 9, 7)
end

return bench
