-- Dedukti LUA basic runtime.

-- Code 
 ccon, clam, cpi, ctype, ckind = 'ccon', 'clam', 'cpi', 'ctype', 'ckind';
-- { co = ccon ; id:string ; arity:int ; f:Code^arity -> Code option ; args:Code* }
-- { co = clam ; f:Code -> Code }
-- { co = cpi  ; ctype:Code ; f:Code -> Code }
-- { co = ctype }
-- { co = ckind }

-- Code0 = unit -> Code

-- Term 
tlam, tpi, tapp, ttype, tbox = 'tlam', 'tpi', 'tapp', 'ttype', 'tbox';
-- { te = tlam ; ttype:Term option; ctype:Code0 option; f:Term*Code -> Term }
-- { te = tpi  ; ttype:Term       ; ctype:Code0       ; f:Term*Code -> Term}
-- { te = tapp ; f:Term ; a:Term ; ca:Code0 }
-- { te = ttype }
-- { te = tbox ; ctype:Code }

-- int -> Code
function mk_var ( i )
  return { co = ccon, id=("var"..i) ; arity = 0 ; f = function() return nil end ; args = { }  };
end

-- Code --> Term
function mk_box ( ty )
  return { te = tbox ; ctype = ty };
end

function push (a,t)
  local res = { }
  res[#t+1] = a
  for i=1,#t do
    res[i] = t[i]
  end
  return res
end

function split (t,n)
  local t1 = { }
  local t2 = { }
  for i=1,n do
    t1[i] = t[i]
  end
  for i=n+1,#t do
    t2[i-n] = t[i]
  end
  return t1,t2
end 

-- Code -> Code
function app0 ( f )
  if f.co ~= ccon then return f end
  if f.arity ~= 0 then return f end
  local f0 = f.f()
  if f0 == nil    then return f end
  return f0
end

-- Code*Code -> Code
function app ( f , arg )
  --print("entering app...")
  --print(" f : " .. string_of_code(50,f))
  --print(" a : " .. string_of_code(50,arg))
  -- CCON
  if     f.co == ccon then
    local args = push(arg,f.args)
    if     f.arity == #args then
      local red = f.f(unpack(args))
      if red ~= nil then return red
      else
        return  { co = ccon ; id=f.id ; arity=f.arity ; f=f.f ; args=args }
      end
    else
      if #args < f.arity then
        return  { co = ccon ; id=f.id ; arity=f.arity ; f=f.f ; args=args }
      else
        local t1,t2 = split(args,f.arity)
        local red = f.f(unpack(t1))
        if red ~= nil then
          for i=1,#t2 do
            red = app ( red , t2[i] )
          end
          return red
        else
          return  { co = ccon ; id=f.id ; arity=f.arity ; f=f.f ; args=args }
        end 
      end
    end
  -- CLAM
  elseif f.co == clam then
    return f.f(arg)
  -- ERROR
  else
    --print("app error... " )
    --print("Fct: " .. string_of_code ( 50 , f ) )
    assert(false)
  end
end

function is_conv ( n , ty1 , ty2 )
  --print ( "entering is_conv ...")
  --print("Type: " .. string_of_code(n,ty1))
  --print("Type: " .. string_of_code(n,ty2))
  --print()
  if     ty1.co == ckind and ty2.co == ckind then return true                   -- Kind
  elseif ty1.co == ctype and ty2.co == ctype then return true                   -- Type
  elseif ty1.co == clam  and ty2.co == clam  then                               -- Lam
    local var = mk_var( n )
    return is_conv ( n+1 , ty1.f(var) , ty2.f(var) )
  elseif ty1.co == cpi   and ty2.co == cpi   then                               -- Pi 
    if is_conv ( n , ty1.ctype , ty2.ctype ) then 
      return is_conv ( n+1 , ty1.f(mk_var(n)) , ty2.f(mk_var(n)) ) 
    else return false
    end
  elseif ty1.co == ccon  and ty2.co == ccon  then                               -- Cons
    if     ty1.id    ~= ty2.id    then return false
    elseif #ty1.args ~= #ty2.args then return false
    else
      for i=1,#ty1.args do
        if not is_conv( n , ty1.args[i] , ty2.args[i] ) then return false end
      end
      return true
    end
  else
    return false
  end
end

-- int * Term * Code --> unit
function type_check ( n , te , ty )
  --print("entering type_check ...")
  --print("Term: " .. string_of_term( n , te ) )
  --print("Type: " .. string_of_code( n , ty ) )
  --print()

  -- LAMBDA
  if      te.te == tlam then
    if ty.co   ~= cpi then error("ERROR: Product Expected.") end
    -- Type Annotations BEGIN
    if te.ttype ~= nil then
      type_check ( n , te.ttype , { co = ctype } )
      if not is_conv ( n , te.ctype() , ty.ctype ) then error("ERROR: Conversion Error.") end
    end
    -- Type Annotations END
    local var = mk_var(n)
    local te2 = te.f ( mk_box (ty.ctype) , var )
    local ty2 = ty.f( mk_var(n) )
    type_check ( n+1 , te2 , ty2 )
  
  -- PI
  elseif te.te == tpi  then
    type_check ( n , te.ttype , { co = ctype } ) ;
    if     is_conv ( n , ty , { co = ctype } ) then 
      type_check ( n+1 , te.f( mk_box(te.ctype()) , mk_var(n) ) , { co = ctype } )
    elseif is_conv ( n , ty , { co = ckind } ) then 
      type_check ( n+1 , te.f( mk_box(te.ctype()) , mk_var(n) ) , { co = ckind } ) 
    else error("ERROR: Sort Error.")
    end

  -- OTHER
  else 
    local ty2 = type_synth ( n , te ) ;
    if not is_conv ( n , ty2 , ty ) then 
      print("ERROR: Cannot Convert: ")
      print("      " .. string_of_code( n , ty2 ))
      print(" with " .. string_of_code( n , ty  ))
      error("Type Checking Failed.")
    end
  end
  --print("leaving type_check ...")
end

-- int * Term --> Code
function type_synth ( n , te )
  --print("entering type_synth ...")
  --print ("Term: " .. string_of_term(n,te ))
  local res = nil

  if     te.te == ttype then res = { co = ckind }        -- Kind
  elseif te.te == tbox  then res = te.ctype              -- Type
  elseif te.te == tlam  then                             -- Lam 
    if te.ctype == nil  then error("Type Synthesis Failed (1).") end
    type_check( n , te.ttype , { co = ctype } )
    local tya = te.ctype()
    local box = mk_box(tya)
    local dummy = type_synth( n+1 , te.f ( box , mk_var(n) ) )
    res = { co = cpi ; ctype = tya ; f = function(x) return type_synth( n , te.f(box,x) ) end } -- FIXME
  elseif te.te == tapp  then                            -- App
    local tyf = type_synth ( n , te.f )
    if tyf.co ~= cpi then error("Type Synthesis Failed (2).") end
    type_check ( n , te.a , tyf.ctype )
    res = tyf.f(te.ca())
  else                                                  -- Default
    error("Type Synthesis Failed (3).")
  end

  --print("leaving type_synth...")
  --print ("Type: " .. string_of_code(n,res))
  --print()
  return res
end

-- Term -> unit
function chktype ( t )
  type_check ( 0 , t , { co = ctype } )
end

-- Term -> unit
function chkkind ( t )
  type_check ( 0 , t , { co = ckind } )
end

-- Term*Code -> unit
function chk ( t , c )
  type_check ( 0 , t , c ) 
end

--[[ Utility functions. ]]

local indent = 0;
local function shiftp(m)
  print(string.rep("  ", indent) .. m);
end

function chkbeg(x)
  shiftp("Checking " .. x .. ".");
  indent = indent + 1;
end

function chkmsg(x)
  shiftp(x);
end

function chkend(x)
  indent = indent - 1;
  shiftp("Done checking \027[32m" .. x .. "\027[m.");
end

-- Debug
--
--[[
function dump ( t )
  print(t)
  for i,v in ipairs(t) do print(i,v) end
end ]]

function string_of_code ( n , c )
  if     c.co == ctype	then return "Type"
  elseif c.co == ckind 	then return "Kind"
  elseif c.co == cpi  	then 
    return ("(v" .. n .. " : " .. string_of_code(n,c.ctype) .. " -> " .. string_of_code(n+1,c.f(mk_var(n))) .. ")")
  elseif c.co == clam 	then 
    return ("(v" .. n .. " => " .. string_of_code(n+1,c.f(mk_var(n))) .. ")" )
  elseif c.co == ccon 	then 
    local str = c.id
    for i=1,#c.args do
      str = str .. " " .. string_of_code(n,c.args[i])
    end
    if #c.args==0 then return str
    else return "(" .. str .. " )"
    end
  else
    return "Error"
  end
end
 
function mk_vart(n)
  return { te = tbox ; ctype = mk_var(n) }
end

function string_of_term ( n , t )
  if  t.te == tlam 	then
    -- Lam
    if t.ctype == nil then 
      return ("(v" .. n .. " => " .. string_of_term( n+1 , t.f(mk_vart(n),mk_var(n)) ) .. ")" )
    else 
      return ("(v" .. n .. " : " .. string_of_code(n,t.ctype()).. " => " .. 
                                string_of_term( n+1 , t.f(mk_vart(n),mk_var(n)) ) .. ")" )
    end
  elseif t.te == tpi  	then 
    -- Pi
    return ( "(v" .. n .. " : " .. string_of_term( n , t.ttype ) .. " -> " .. 
                                string_of_term( n+1 , t.f(mk_vart(n),mk_var(n)) ) .. ")" )
  elseif t.te == tapp 	then 
    -- App
    return "(" .. string_of_term( n , t.f ) .. " " .. string_of_term( n , t.a ) .. ")" 
  elseif t.te == ttype	then 
    -- Type
    return "Type"
  elseif t.te == tbox 	then 
    -- Box
    return "(Box " .. string_of_code(n,t.ctype) .. ")"
  else 
    -- Err
    return "Error"
  end
end
    
-- vi: expandtab: sw=2
