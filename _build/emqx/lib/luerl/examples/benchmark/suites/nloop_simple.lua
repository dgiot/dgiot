local ipairs = ipairs

local t5, t25, t50 = {}, {}, {}

do
  for i = 1, 10 do
    t5[i] = i
  end
  t5[6] = nil

  for i = 1, 30 do
    t25[i] = i
  end
  t25[26] = nil

  for i = 1, 55 do
    t50[i] = i
  end
  t50[51] = nil
end

local do_nothing = function()
end

bench = {}

local do_loop_ipairs = function(t)
  for i, v in ipairs(t) do
  end
end

local do_loop_numfor = function(t)
  for i = 1, #t do
    if t[i] == nil then
      break
    end
  end
end

local do_loop_while = function(t)
  local i = 1
  while t[i] ~= nil do
    i = i + 1
  end
end

bench.loop_ipairs_50 = function()
  do_loop_ipairs(t50)
  do_nothing(t50) -- Padding to get equivalent number of function calls.
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
end

bench.loop_numfor_50 = function()
  do_loop_numfor(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
end

bench.loop_while_50 = function()
  do_loop_while(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
  do_nothing(t50)
end

bench.loop_ipairs_25 = function()
  do_loop_ipairs(t25)
  do_loop_ipairs(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
end

bench.loop_numfor_25 = function()
  do_loop_numfor(t25)
  do_loop_numfor(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
end

bench.loop_while_25 = function()
  do_loop_while(t25)
  do_loop_while(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
  do_nothing(t25)
end

bench.loop_ipairs_5 = function()
  do_loop_ipairs(t5)
  do_loop_ipairs(t5)
  do_loop_ipairs(t5)
  do_loop_ipairs(t5)
  do_loop_ipairs(t5)
  do_loop_ipairs(t5)
  do_loop_ipairs(t5)
  do_loop_ipairs(t5)
  do_loop_ipairs(t5)
  do_loop_ipairs(t5)
end

bench.loop_numfor_5 = function()
  do_loop_numfor(t5)
  do_loop_numfor(t5)
  do_loop_numfor(t5)
  do_loop_numfor(t5)
  do_loop_numfor(t5)
  do_loop_numfor(t5)
  do_loop_numfor(t5)
  do_loop_numfor(t5)
  do_loop_numfor(t5)
  do_loop_numfor(t5)
end

bench.loop_while_5 = function()
  do_loop_while(t5)
  do_loop_while(t5)
  do_loop_while(t5)
  do_loop_while(t5)
  do_loop_while(t5)
  do_loop_while(t5)
  do_loop_while(t5)
  do_loop_while(t5)
  do_loop_while(t5)
  do_loop_while(t5)
end

return bench
