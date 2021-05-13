-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

-- NUMBER_TO_FACTOR = 600851475143
NUMBER_TO_FACTOR = 13195

limit = math.sqrt(NUMBER_TO_FACTOR)

primes = { 3 }

function divisible_by_any(val, array)
  for ii, num in ipairs(array) do
    if (val % num) == 0 then
      return true
    end
  end
  return false
end

for ii = 5, limit, 2 do
  if not divisible_by_any(ii, primes) then
    table.insert(primes, ii)
    if NUMBER_TO_FACTOR % ii == 0 then
      factor = ii
    end
  end
end
print(factor)
return factor


