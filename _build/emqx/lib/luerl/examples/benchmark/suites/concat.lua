local table_concat, table_insert = table.concat, table.insert

NUM_ITERATIONS = 1

bench = {}

bench.raw_concat = function()
  local self = "string 0\n"
  for i = 1, 1000 do
    self = self .. "string " .. i .. "\n"
  end
  return self
end

bench.raw_plus_1 = function()
  local self = { "string 0\n" }
  for i = 1, 1000 do
    self[#self + 1] = "string "
    self[#self + 1] = i
    self[#self + 1] = "\n"
  end
  return table_concat(self)
end

bench.raw_insert = function()
  local self = { "string 0\n" }
  for i = 1, 1000 do
    table_insert(self, "string ")
    table_insert(self, i )
    table_insert(self, "\n" )
  end
  return table_concat(self)
end

bench.mixed_plus_1 = function()
  local self = {"string 0\n"}
  for i = 1,1000 do
    self[#self + 1] = "string " .. i .. "\n"
  end
  return table_concat(self)
end

bench.mixed_insert = function()
  local self = {"string 0\n"}
  for i = 1, 1000 do
    table_insert(self, "string " .. i .. "\n")
  end
  return table_concat(self)
end

return bench
