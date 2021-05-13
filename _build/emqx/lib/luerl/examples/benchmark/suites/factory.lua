local pairs, setmetatable = pairs, setmetatable

local clone_table = function(t)
  local r = {}
  for k, v in pairs(t) do
    r[k] = v
  end
  return r
end

local inplace = function()
  local factory
  do
    factory = function()
      return
      {
        method1 = function(self) end;
        method2 = function(self) end;
        method3 = function(self) end;
        method4 = function(self) end;
        method5 = function(self) end;
        method6 = function(self) end;
        method7 = function(self) end;
        method8 = function(self) end;
        method9 = function(self) end;
        method10 = function(self) end;
      }
    end
  end
  return factory
end

local plain = function()
  local factory
  do
    local method1 = function(self) end
    local method2 = function(self) end
    local method3 = function(self) end
    local method4 = function(self) end
    local method5 = function(self) end
    local method6 = function(self) end
    local method7 = function(self) end
    local method8 = function(self) end
    local method9 = function(self) end
    local method10 = function(self) end
    factory = function()
      return
      {
        method1 = method1;
        method2 = method2;
        method3 = method3;
        method4 = method4;
        method5 = method5;
        method6 = method6;
        method7 = method7;
        method8 = method8;
        method9 = method9;
        method10 = method10;
      }
    end
  end
  return factory
end

local mt = function()
  local factory
  do
    local mt =
    {
      __index =
      {
        method1 = function(self) end;
        method2 = function(self) end;
        method3 = function(self) end;
        method4 = function(self) end;
        method5 = function(self) end;
        method6 = function(self) end;
        method7 = function(self) end;
        method8 = function(self) end;
        method9 = function(self) end;
        method10 = function(self) end;
      };
    }
    factory = function()
      return setmetatable({}, mt)
    end
  end
  return factory
end

local clone = function()
  local factory
  do
    local proto =
    {
      method1 = function(self) end;
      method2 = function(self) end;
      method3 = function(self) end;
      method4 = function(self) end;
      method5 = function(self) end;
      method6 = function(self) end;
      method7 = function(self) end;
      method8 = function(self) end;
      method9 = function(self) end;
      method10 = function(self) end;
    }
    factory = function()
      return clone_table(proto)
    end
  end
  return factory
end

local invoker = function(factory)
  local obj = factory()
  return function()
    obj:method1()
    obj:method2()
    obj:method3() 
    obj:method4()
    obj:method5()
    obj:method6()
    obj:method7()
    obj:method8()
    obj:method9()
    obj:method10()
  end
end

bench =
{
  inplace_init = inplace;
  plain_init = plain;
  metatable_init = mt;
  clone_init = clone;

  inplace_call = inplace();
  plain_call = plain();
  metatable_call = mt();
  clone_call = clone();

  inplace_method = invoker(inplace());
  plain_method = invoker(plain());
  metatable_method = invoker(mt());
  clone_method = invoker(clone());
}

return bench
