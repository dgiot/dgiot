local assert, loadstring = assert, loadstring
local pairs, ipairs, next = pairs, ipairs, next
local table_concat = table.concat

--------------------------------------------------------------------------------

local DATA =
{
  006.635; 009.210; 011.345; 013.277; 015.086; 016.812; 018.475; 020.090;
  021.666; 023.209; 024.725; 026.217; 027.688; 029.141; 030.578; 032.000;
  033.409; 034.805; 036.191; 037.566; 038.932; 040.289; 041.638; 042.980;
  044.314; 045.642; 046.963; 048.278; 049.588; 050.892; 052.191; 053.486;
  054.776; 056.061; 057.342; 058.619; 059.893; 061.162; 062.428; 063.691;
  064.950; 066.206; 067.459; 068.710; 069.957; 071.201; 072.443; 073.683;
  074.919; 076.154; 077.386; 078.616; 079.843; 081.069; 082.292; 083.513;
  084.733; 085.950; 087.166; 088.379; 089.591; 090.802; 092.010; 093.217;
  094.422; 095.626; 096.828; 098.028; 099.228; 100.425; 101.621; 102.816;
  104.010; 105.202; 106.393; 107.583; 108.771; 109.958; 111.144; 112.329;
  113.512; 114.695; 115.876; 117.057; 118.236; 119.414; 120.591; 121.767;
  122.942; 124.116; 125.289; 126.462; 127.633; 128.803; 129.973; 131.141;
  132.309; 133.476; 134.642;
}

local DATA_SIZE = #DATA

--------------------------------------------------------------------------------

local accum_unrolled
do
  local buf = { "return function(t, c) c = c or 0; " }
  for i = 1, DATA_SIZE do
    buf[#buf + 1] = "c = c + t["..i.."]; "
  end
  buf[#buf + 1] = "return c; end"

  local fn = assert(loadstring(table_concat(buf)))

  accum_unrolled = assert(fn())
end

local accum_numeric_for = function(t, c)
  c = c or 0
  for i = 1, #t do
    c = c + t[i]
  end
  return c
end

local accum_numeric_while = function(t, c)
  c = c or 0
  local i = 1
  local v = t[i]
  while v ~= nil do
    c = c + v
    i = i + 1
    v = t[i]
  end
  return c
end

local accum_ipairs = function(t, c)
  c = c or 0
  for _, v in ipairs(t) do
    c = c + v
  end
  return c
end

local accum_pairs = function(t, c)
  c = c or 0
  for _, v in pairs(t) do
    c = c + v
  end
  return c
end

local accum_next = function(t, c)
  c = c or 0
  local k, v = next(t)
  while k ~= nil do
    c = c + v
    k, v = next(t, k)
  end
  return c
end

--------------------------------------------------------------------------------

bench = { }

bench.unrolled = function()
  return accum_unrolled(DATA)
end

bench.numeric_while = function()
  return accum_numeric_while(DATA)
end

bench.numeric_for = function()
  return accum_numeric_for(DATA)
end

bench.ipairs = function()
  return accum_ipairs(DATA)
end

bench.pairs = function()
  return accum_pairs(DATA)
end

bench.next = function()
  return accum_next(DATA)
end

return table.pack(bench)
