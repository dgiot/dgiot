-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

-- Find the sum of all the primes below two million.

-- LIMIT = 2000000
LIMIT = 2000

function primes_below(limit)
  found_primes = { }

  local function divisible_by_known_primes(num)
    for _, v in ipairs(found_primes) do
      if num % v == 0 then
        return true
      end
    end
    return false
  end

  local function next_prime(found_primes, last_prime)
    if last_prime == nil then
      table.insert(found_primes, 2)
      return 2
    end

    val = found_primes[#found_primes]
    repeat
      val = (val == 2) and 3 or val + 2
    until(not divisible_by_known_primes(val))
    table.insert(found_primes, val)

    if val > limit then
      return nil
    else
      return val
    end
  end

  return next_prime, found_primes, nil
end


sum = 0
for ii in primes_below(LIMIT) do
  sum = sum + ii
end
print(sum)
return sum
