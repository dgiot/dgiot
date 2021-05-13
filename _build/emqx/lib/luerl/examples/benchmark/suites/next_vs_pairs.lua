local pairs, next = pairs, next

local t = {}
for i = 1, 50 do
  t[i] = i -- Array part
  t[i * 256] = i -- Hash part
end

bench = {}

bench.pairs = function()
  local sum = 0
  for k, v in pairs(t) do
    sum = sum + v
  end
end

bench.next = function()
  local sum = 0
  local k, v = next(t)
  while k ~= nil do
    sum = sum + v
    k, v = next(t, k)
  end
end

return bench
