-- The sum of the squares of the first ten natural numbers is,

-- 12 + 22 + ... + 102 = 385

-- The square of the sum of the first ten natural numbers is,

-- (1 + 2 + ... + 10)2 = 552 = 3025

-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

LIMIT = 100
sum, sum_squares = 0, 0

for ii = 1, LIMIT do
  sum, sum_squares = sum + ii, sum_squares + (ii * ii)
end

answer = (sum*sum) - sum_squares
print(answer)
return answer
