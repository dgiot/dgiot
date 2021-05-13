local list = {}
for key, func in pairs(bench) do
  list[#list+1] = key 
end
return list
