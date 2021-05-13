-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

LIMIT = 1000

sum = 0
for ii = 1, (LIMIT-1) do
  if (ii%3 == 0) or (ii%5 == 0)  then
    sum = sum + ii
  end
end

print(sum)
return sum
