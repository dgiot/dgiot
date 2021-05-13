bench = {}

local function chain()
  return chain
end

local function plain()
  -- No-op
end

bench.chain_upval = function()
  chain () () () () () () () () () () -- 10 calls
end

bench.plain_upval = function()
  plain ()
  plain ()
  plain ()
  plain ()
  plain ()
  plain ()
  plain ()
  plain ()
  plain ()
  plain () -- 10 calls
end

bench.plain_chain_upval = function()
  chain ()
  chain ()
  chain ()
  chain ()
  chain ()
  chain ()
  chain ()
  chain ()
  chain ()
  chain () -- 10 calls
end

bench.chain_local = function()
  local chain = chain
  chain () () () () () () () () () () -- 10 calls
end

bench.plain_local = function()
  local plain = plain
  plain ()
  plain ()
  plain ()
  plain ()
  plain ()
  plain ()
  plain ()
  plain ()
  plain ()
  plain () -- 10 calls
end

bench.plain_chain_local = function()
  local chain = chain
  chain ()
  chain ()
  chain ()
  chain ()
  chain ()
  chain ()
  chain ()
  chain ()
  chain ()
  chain () -- 10 calls
end

return bench
