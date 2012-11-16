-- Dedukti LUA basic runtime.

-- Code 
 ccon, clam, capp, cpi, ctype, ckind, cvar = 'ccon', 'clam', 'capp', 'cpi', 'ctype', 'ckind', 'cvar';
-- { co = ccon ; id = string ; arity:int ; f:Code^arity -> Code option ; args:Code* }
-- { co = clam ; f:Code -> Code }
-- { co = capp ; f:Code ; arg = Code }
-- { co = cpi  ; ctype:Code ; f:Code -> Code }
-- { co = ctype }
-- { co = ckind }
-- { co = cvar ; n:int }

-- Term 
tlam, tpi, tapp, ttype, tbox = 'tlam', 'tpi', 'tapp', 'ttype', 'tbox';
-- { te = tlam ; ctype:Code option; f:Term*Code -> Term }
-- { te = tpi  ; ttype:Term ; ctype:Code ; f:Term*Code -> Term}
-- { te = tapp ; f:Term ; a:Term ; ca:Code }
-- { te = ttype }
-- { te = tbox ; ctype:Code }

-- int -> Code
function mk_var ( i )
  return { co = cvar, n=i };
end

-- Code --> Term
function mk_box ( ty )
  return { te = tbox ; ctype = ty };
end

function push (a,t) -- FIXME table.insert ?
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

function app ( c , args )
  local res = c
  for i=1,#args do
    res = { co = capp ; f = res ; arg = args[i] }
  end
  return res
end

function cons_norm ( args )
  local res = { }
  for i=1,#args do
    local w = weak_norm ( args[i] )
    if w.co == ccon then
      res[i] = { co = ccon ; id = w.id ; arity = w.arity ; f = w.f ; args = cons_norm(w.args) }
    else
      res[i] = w
    end
  end
  return res
end

-- Code -> Code 
-- This may not terminate if c is not well-typed !
function weak_norm ( c )
  --print ( "entering weak_norm..." )
  --print ("Type: " .. strc(c))
  -- Cons
  if     c.co == ccon then
    if     c.arity == #c.args then
      local red = c.f(unpack(cons_norm(c.args)))
      if red == nil then 
        return c
      else 
        local hnf = weak_norm(red)
        return hnf
      end
    elseif c.arity <  #c.args then
      local t1,t2 = split(c.args,c.arity)
      local red = c.f(unpack(cons_norm(t1)))
      if red == nil then
        return c
      else
        local hnf = weak_norm(app(red,t2))
        return hnf
      end
    else -- c.arity > #c.args
      return c
    end
    -- App
  elseif c.co == capp  then
    local f = weak_norm(c.f)
    if     f.co == clam then 
      local hnf = weak_norm(f.f(c.arg))
      return hnf
    elseif f.co == ccon then
      local hnf = weak_norm( { co = ccon ; id = f.id ; arity = f.arity ; f = f.f ; args = push(c.arg,f.args) } )
      return hnf
    else
      return c
      -- Other
    end
  else
    return c
  end
end
   
-- int*Code*Code --> bool
-- This may not terminate if tty1 or tty2 is not well-typed !
function is_conv ( n , tty1 , tty2 )
  local res = false
  --print ( "entering is_conv ...")
  --print ( "Type1 " .. strc(tty1) )
  --print ( "Type2 " .. strc(tty2) )

  local ty1 = weak_norm(tty1)
  local ty2 = weak_norm(tty2)
  
    -- Kind
  if     ty1.co == ckind and ty2.co == ckind then res = true
    -- Type
  elseif ty1.co == ctype and ty2.co == ctype then res = true
    -- Var
  elseif ty1.co == cvar  and ty2.co == cvar  then res = (ty1.n == ty2.n)
    -- Lam
  elseif ty1.co == clam  and ty2.co == clam  then res = is_conv ( n+1 , ty1.f(mk_var(n)) , ty2.f(mk_var(n)) )
    -- Pi
  elseif ty1.co == cpi   and ty2.co == cpi   then 
      res = ( is_conv ( n , ty1.ctype , ty2.ctype ) and is_conv ( n+1 , ty1.f(mk_var(n)) , ty2.f(mk_var(n)) ) )
    -- Cons
  elseif ty1.co == ccon  and ty2.co == ccon  then 
    res = true
    if (ty1.id ~= ty2.id or #ty1.args ~= #ty2.args) then res = false end
    for i=1,#ty1.args do
      if not is_conv( n , ty1.args[i] , ty2.args[i] ) then res = false end
    end
    -- App
  elseif ty1.co == capp  and ty2.co == capp  then 
    res = ( is_conv( n , ty1.f , ty2.f ) and is_conv( n , ty1.arg , ty2.arg ) )
    -- Default
  end
  --print("leaving is_conv ...")
  return res
end

-- int * Term * Code --> unit
function type_check ( n , t , ty )
  --print("entering type_check ...")
  --print("Term: " .. strt(t)  )
  --print("Type: " .. strc(ty) )

    -- LAMBDA
  if      t.te == tlam then
    local wty = weak_norm(ty)  
    if wty.co == cpi then
      if t.ctype ~= nil then
        if not is_conv ( n , t.ctype , wty.ctype ) then error ("Conv error") end
      end
      local b = wty.f(mk_var(n))
      local box = mk_box (wty.ctype)
      type_check ( n+1 , t.f(box,mk_var(n)) , b )
    else
      print("Product Expected.")
      print(strc(wty))
      error("Error in type_check.")
    end
  
  -- PI
  elseif t.te == tpi  then
    type_check ( n , t.ttype , { co = ctype } ) ;
    if     is_conv ( n , ty , { co = ctype } ) then 
      type_check ( n+1 , t.f( mk_box(t.ctype) , mk_var(n) ) , { co = ctype } )
    elseif is_conv ( n , ty , { co = ckind } ) then 
      type_check ( n+1 , t.f( mk_box(t.ctype) , mk_var(n) ) , { co = ckind } ) 
    else 
      error("Sort error")
    end

    -- OTHER
  else 
    local sy = type_synth ( n , t ) ;
    if not is_conv ( n , sy , ty ) then 
      print("Cannot Convert: ")
      print(" -> " .. strc(sy))
      print(" -> " .. strc(ty))
      error("Type Checking Failed.")
    end
  end
end

-- int * Term --> Code
function type_synth ( n , t )
  --print("entering type_synth ...")
  --print ("Term: " .. strt(t))
  if     t.te == ttype then 
    -- Kind
    --print("leaving type_synth (Kind)")
    return { co = ckind }
  elseif t.te == tbox  then 
    -- Type
    --print("leaving type_synth (Type)")
    return t.ctype
  elseif t.te == tlam  then 
    -- Lam 
    if t.ctype ~= nil then
      return { co = cpi ; ctype = t.ctype ; f = 
        function(x)
          return type_synth( n , t.f(mk_box(t.ctype),x) ) -- FIXME
        end }
    else
      error ("Type synthesis failed (3).")
    end
  elseif t.te == tapp  then
    -- App
    local sy = type_synth ( n , t.f );
    local c  = weak_norm( sy );
    if c.co == cpi then
      type_check ( n , t.a , c.ctype ) ;
      local res = c.f(t.ca)
      --print("leaving type_synth: ")
      --print(strc(res))
      return res
    else
      error("Type synthesis failed (1).")
    end
  else 
    -- Default
    error("Type synthesis failed (2).");
  end
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
-- { co = ccon ; id = string ; arity:int ; f:Code^arity -> Code option ; args:Code* }
-- { co = clam ; f:Code -> Code }
-- { co = capp ; f:Code ; arg = Code }
-- { co = cpi  ; ctype:Code ; f:Code -> Code }
-- { co = ctype }
-- { co = ckind }
-- { co = cvar ; n:int }

function strc0 ( n , c , b )
  if b>10 then return "..."
  elseif c.co == ctype	then return "Type"
  elseif c.co == ckind 	then return "Kind"
  elseif c.co == cvar 	then 
    return ("(Var " .. c.n .. ")")
  elseif c.co == cpi  	then 
    return ("(Pi " .. n .. " : " .. strc0( n , c.ctype , b+1 ) .. " -> " .. strc0( n+1 , c.f(mk_var(n)) , b+1 ) .. ")" )
  elseif c.co == clam 	then 
    return ("(Lam " .. n .. " -> " .. strc0( n+1 , c.f(mk_var(n)) , b+1 ) .. ")" )
  elseif c.co == capp 	then 
    return "(App " .. strc0(n,c.f,b+1) .. " " .. strc0(n,c.arg,b+1) .. " )"
  elseif c.co == ccon 	then 
    local str = "(" .. c.id .. " arity=" .. c.arity 
    for i=1,#c.args do
      str = str .. " " .. strc0(n,c.args[i],b+1)
    end
    return str .. " )"
  else
    return "Error"
  end
end
     
function strc ( c )
	return strc0 ( 0 , c , 0 )
end


-- { te = tlam ; ctype:Code option; f:Term*Code -> Term }
-- { te = tpi  ; ttype:Term ; ctype:Code ; f:Term*Code -> Term}
-- { te = tapp ; f:Term ; a:Term ; ca:Code }
-- { te = ttype }
-- { te = tbox ; ctype:Code }

function mk_vart(n)
  return { te = tbox ; ctype = mk_var(n) }
end

function strt0 ( n , t , b)
  if b>10 then return "..."
  elseif  t.te == tlam 	then
    -- Lam
    if t.ctype == nil then 
      return ("(Lam " .. n .. " -> " .. strt0( n+1 , t.f(mk_vart(n),mk_var(n)) , b+1 ) .. ")" )
    else 
      return ("(Lam " .. n .. " : " .. strc(t.ctype).. " -> " .. strt0( n+1 , t.f(mk_vart(n),mk_var(n)) , b+1 ) .. ")" )
    end
  elseif t.te == tpi  	then 
    -- Pi
    return ( "(Pi " .. n .. " : (" .. strt0( n , t.ttype , b+1 ) .. ", " .. strc(t.ctype) .. ") -> " .. strt0( n+1 , t.f(mk_vart(n),mk_var(n)) , b+1) .. ")" )
  elseif t.te == tapp 	then 
    -- App
    return "(App f=" .. strt0( n , t.f , b+1 ) .. " a=" .. strt0( n , t.a , b+1 ) .. " ca=" .. strc(t.ca) .. ")" 
  elseif t.te == ttype	then 
    -- Type
    return "Type"
  elseif t.te == tbox 	then 
    -- Box
    return "(Box " .. strc(t.ctype) .. ")"
  else 
    -- Err
    print("Error strt")
    error("Err2")
  end
end

function strt ( t )
  return strt0(0,t,0)
end
-- vi: expandtab: sw=2
