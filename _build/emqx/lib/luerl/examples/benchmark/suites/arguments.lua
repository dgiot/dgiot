local select, tostring, assert, type, error
    = select, tostring, assert, type, error

--------------------------------------------------------------------------------

local run_plain_assert = function(a, b, c)
  assert(type(a) == "number")
  assert(type(b) == "boolean")
  assert(type(c) == "string")
end

--------------------------------------------------------------------------------

local run_assert_is
do
  local make_assert_is = function(typename)
    return function(v, msg)
      if type(v) == typename then
        return v
      end
      error(
          (msg or "assertion failed")
          .. ": expected `" .. typename .. "', got `"
          .. type(v) .. "'",
          3
        )
    end
  end

  local assert_is_number = make_assert_is("number")
  local assert_is_boolean = make_assert_is("boolean")
  local assert_is_string = make_assert_is("string")

  run_assert_is = function(a, b, c)
    assert_is_number(a)
    assert_is_boolean(b)
    assert_is_string(c)
  end
end

--------------------------------------------------------------------------------

local run_arguments_select_simple
do
  local arguments_select = function(...)
    local nargs = select("#", ...)
    for i = 1, nargs, 2 do
      local expected_type, value = select(i, ...)
      if type(value) ~= expected_type then
        error(
            "bad argument #" .. ((i + 1) / 2)
            .. " type: expected `" .. expected_type
            .. "', got `" .. type(value) .. "'",
            3
          )
      end
    end
  end

  run_arguments_select_simple = function(a, b, c)
    arguments_select(
        "number", a,
        "boolean", b,
        "string", c
      )
  end
end

--------------------------------------------------------------------------------

local run_arguments_recursive_simple
do
  -- Simplified lua-nucleo version, equivalent to the others.
  local function impl(arg_n, expected_type, value, ...)
    -- Points error on function, calling function which calls *arguments()
    if type(value) ~= expected_type then
      error(
          "argument #"..arg_n..": expected `"..tostring(expected_type)
       .. "', got `"..type(value).."'",
          3 + arg_n
        )
    end

    -- If have at least one more type, check it
    return ((...) ~= nil) and impl(arg_n + 1, ...) or true
  end

  local arguments_recursive = function(...)
    local nargs = select('#', ...)
    return (nargs > 0)
       and impl(1, ...)
        or true
  end

  run_arguments_recursive_simple = function(a, b, c)
    arguments_recursive(
        "number", a,
        "boolean", b,
        "string", c
      )
  end
end

--------------------------------------------------------------------------------

local run_arguments_recursive_lua_nucleo
do
  -- Taken directly from lua-nucleo
  local lua51_types =
  {
    ["nil"] = true;
    ["boolean"] = true;
    ["number"] = true;
    ["string"] = true;
    ["table"] = true;
    ["function"] = true;
    ["thread"] = true;
    ["userdata"] = true;
  }

  local function impl(is_optional, arg_n, expected_type, value, ...)
    -- Points error on function, calling function which calls *arguments()

    if type(value) ~= expected_type then
      if not lua51_types[expected_type] then
        error(
            "argument #"..arg_n..": bad expected type `"..tostring(expected_type).."'",
            3 + arg_n
          )
      end

      if not is_optional or value ~= nil then
        error(
            (is_optional and "optional" or "")
         .. "argument #"..arg_n..": expected `"..tostring(expected_type)
         .. "', got `"..type(value).."'",
            3 + arg_n
          )
      end
    end

    -- If have at least one more type, check it
    return ((...) ~= nil) and impl(is_optional, arg_n + 1, ...) or true
  end

  local arguments_recursive = function(...)
    local nargs = select('#', ...)
    return (nargs > 0)
       and (
         (nargs % 2 == 0)
           and impl(false, 1, ...) -- Not optional
            or error("arguments: bad call, dangling argument detected")
       )
       or true
  end

  run_arguments_recursive_lua_nucleo = function(a, b, c)
    arguments_recursive(
        "number", a,
        "boolean", b,
        "string", c
      )
  end
end

--------------------------------------------------------------------------------

-- TODO: Add a version with full-blown validation.
local run_arguments_unroll_simple
do
  -- TODO: Put a code-generation metatable over cache
  --       and pre-populate it for cases with (1-10) * 2 arguments.
  --       If __index sees odd number, it should crash
  --       with dangling argument error.
  local arguments_cache =
  {
    [6] = function(t1, v1, t2, v2, t3, v3)
      if type(v1) ~= t1 then
        error(
            "argument #1: expected `"..tostring(t1)
         .. "', got `"..type(v1).."'",
            4
          )
      end
      if type(v2) ~= t2 then
        error(
            "argument #2: expected `"..tostring(t2)
         .. "', got `"..type(v2).."'",
            4
          )
      end
      if type(v3) ~= t3 then
        error(
            "argument #3: expected `"..tostring(t3)
         .. "', got `"..type(v3).."'",
            4
          )
      end
    end;
  }

  local arguments = function(...)
    local n = select("#", ...)
    -- Assuming cache is pre-populated for all possible use-cases
    return assert(arguments_cache[n])(...)
  end

  run_arguments_unroll_simple = function(a, b, c)
    arguments(
        "number", a,
        "boolean", b,
        "string", c
      )
  end
end

--------------------------------------------------------------------------------

local run_arguments_hardcoded_simple
do
  -- Not much real-word meaning, just for comparison with
  -- run_arguments_unroll_simple.
  local hardcoded_arguments_6 = function(t1, v1, t2, v2, t3, v3)
    if type(v1) ~= t1 then
      error(
          "argument #1: expected `"..tostring(t1)
       .. "', got `"..type(v1).."'",
          2
        )
    end
    if type(v2) ~= t2 then
      error(
          "argument #2: expected `"..tostring(t2)
       .. "', got `"..type(v2).."'",
          2
        )
    end
    if type(v3) ~= t3 then
      error(
          "argument #3: expected `"..tostring(t3)
       .. "', got `"..type(v3).."'",
          2
        )
    end
  end

  run_arguments_hardcoded_simple = function(a, b, c)
    hardcoded_arguments_6(
        "number", a,
        "boolean", b,
        "string", c
      )
  end
end

--------------------------------------------------------------------------------

bench = { }

bench.plain_assert = function()
  run_plain_assert(42, true, "aaa")
end

bench.assert_is = function()
  run_assert_is(42, true, "aaa")
end

bench.assert_is_alloc = function()
  -- Imitating args table allocation.
  -- Needed to compensate plain Lua interpreter
  -- compatibility mode.
  local a, b, c = { }, { }, { }
  run_assert_is(42, true, "aaa")
end

bench.args_select_simple = function()
  run_arguments_select_simple(42, true, "aaa")
end

bench.args_recursive_simp = function()
  run_arguments_recursive_simple(42, true, "aaa")
end

bench.args_recursive_ln = function()
  run_arguments_recursive_lua_nucleo(42, true, "aaa")
end

bench.args_unroll_simple = function()
  run_arguments_unroll_simple(42, true, "aaa")
end

bench.args_hard_simple = function()
  run_arguments_hardcoded_simple(42, true, "aaa")
end

return bench
