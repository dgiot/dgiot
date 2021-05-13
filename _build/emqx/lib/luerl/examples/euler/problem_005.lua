-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

LIMIT = 20

function prime_factors(n)

  local function factor(_, val)
    if n > 1 then
      while n % val > 0 do
        val = val + ( val == 2 and 1 or 2)
        if val * val > n then val = n end
      end
      n = n / val
      return val
    end
  end
  return factor, nil, 2
end

function factorize(number)
  factors = {}
  for p in prime_factors(number) do
    factors[p] = factors[p] and factors[p] + 1 or 1
  end
  return factors
end

function collapse(dict1, dict2)
  dict = {}
  for key, val in pairs(dict1) do
    dict[key] = math.max(val, dict2[key] or 0)
  end
  for key, val in pairs(dict2) do
    dict[key] = math.max(val, dict1[key] or 0)
  end
  return dict
end

factors = {}
for ii = 2, LIMIT do
  factors = collapse(factors, factorize(ii))
end
product = 1
for key, val in pairs(factors) do
  product = product * (key ^ val)
end

print(product)
return product
