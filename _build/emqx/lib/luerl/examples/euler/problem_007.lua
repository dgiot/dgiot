-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

-- LIMIT = 10001
LIMIT = 113

function primes(count)
  found_primes = { 2 }

  local function divisible_by_known_primes(num)
    for _, v in ipairs(found_primes) do
      if num % v == 0 then
        return true
      end
    end
    return false
  end

  local function next_prime(found_primes, _)
    val = found_primes[#found_primes]
    repeat
      val = (val == 2) and 3 or val + 2
    until(not divisible_by_known_primes(val))
    table.insert(found_primes, val)

    if #found_primes > count then
      return nil
    else
      return val
    end
  end

  return next_prime, found_primes, 3
end

for ii in primes(LIMIT) do
  highest = ii
end
print(highest)
return highest
