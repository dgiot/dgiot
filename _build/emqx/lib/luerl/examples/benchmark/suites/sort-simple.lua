local table_sort = table.sort
local math_random, math_randomseed = math.random, math.randomseed

--------------------------------------------------------------------------------

math_randomseed(12345)

--------------------------------------------------------------------------------

-- TODO: Benchmark some pure-lua qsort

local DATA_SIZE = 1e1

local generate_data = function()
  local t = { }
  for i = 1, DATA_SIZE do
    t[i] = math_random()
  end
  return t
end

local less = function(lhs, rhs)
  return lhs < rhs
end

local bubble_sort = function(t)
  for i = 2, #t do
    local switched = false
    for j = #t, i, -1 do
      if t[j] < t[j - 1] then
        t[j], t[j - 1] = t[j - 1], t[j]
        switched = true
      end
    end
    if switched == false then
      return t
    end
  end
  return t
end

local bubble_sort_cb = function(t, less)
  for i = 2, #t do
    local switched = false
    for j = #t, i, -1 do
      if less(t[j], t[j - 1]) then
        t[j], t[j - 1] = t[j - 1], t[j]
        switched = true
      end
    end
    if switched == false then
      return t
    end
  end
  return t
end

--------------------------------------------------------------------------------

bench = { }

bench.generate_only = function()
  local data = generate_data()
  return true
end

bench.tsort_nocallback = function()
  local data = generate_data()
  return table_sort(data)
end

bench.tsort_callback = function()
  local data = generate_data()
  return table_sort(data, less)
end

bench.bubble_nocallback = function()
  local data = generate_data()
  return bubble_sort(data)
end

bench.bubble_callback = function()
  local data = generate_data()
  return bubble_sort_cb(data, less)
end

return bench
