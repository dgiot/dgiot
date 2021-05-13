-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

function is_palindrome(number)
  local str = number .. ''
  for ii = 1, (#str / 2) do
    if string.byte(str,ii) ~= string.byte(str, -ii) then
      return false
    end
  end
  return true
end

LOW = 100
--HIGH = 999
HIGH = 199

highest = 0
for ii = LOW, HIGH do
  for jj = LOW, HIGH do
    num = ii * jj
    if is_palindrome(num) then
      highest = (num > highest) and num or highest
    end
  end
end
print(highest)
return highest

