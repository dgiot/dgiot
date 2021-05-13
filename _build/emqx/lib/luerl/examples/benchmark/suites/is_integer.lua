local math_floor = math.floor

bench = {}

local integer, noninteger = 12345, 12345.67

local noop = function(a)
  return a
end

local isint_floor = function(a)
  if a == math_floor(a) then
    return true
  end
  return false
end

local isint_floor_direct = function(a)
  return (a == math_floor(a))
end

local isint_mod = function(a)
  if a % 1 == 0 then
    return true
  end
  return false
end

local isint_mod_direct = function(a)
  return (a % 1 == 0)
end

local isint_bits = function(a)
  if (a + 2^52) - 2^52 == a then
    return true
  end
  return false
end

local isint_bits_direct = function(a)
  return (a + 2^52) - 2^52 == a
end

bench.noop_int = function()
  noop(integer)
end

bench.noop_nonint = function()
  noop(noninteger)
end

bench.floor_int = function()
  isint_floor(integer)
end

bench.floor_nonint = function()
  isint_floor(noninteger)
end

bench.floor_int_direct = function()
  isint_floor_direct(integer)
end

bench.floor_nonint_direct = function()
  isint_floor_direct(noninteger)
end

bench.mod_int = function()
  isint_mod(integer)
end

bench.mod_nonint = function()
  isint_mod(noninteger)
end

bench.mod_int_direct = function()
  isint_mod_direct(integer)
end

bench.mod_nonint_direct = function()
  isint_mod_direct(noninteger)
end

bench.bits_int = function()
  isint_bits(integer)
end

bench.bits_nonint = function()
  isint_bits(noninteger)
end

bench.bits_int_direct = function()
  isint_bits_direct(integer)
end

bench.bits_nonint_direct = function()
  isint_bits_direct(noninteger)
end

return bench
