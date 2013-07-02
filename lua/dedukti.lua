-- Dedukti LUA basic runtime.

-- Code 
-- { cid:string  ; args:Code* [ ; arity:int ; f:Code^arity -> Code option ] }
-- { clam_f:Code -> Code }
-- { cpi_cty:Code ; cpi_f:Code -> Code }
-- { clazy=unit->Code }
-- { ctype=true }
-- { ckind=true }

-- Term 
-- { tlam_f:Term*Code -> Term [ ; tlam_tty:Term option ; tlam_cty:Code option ] }
-- { tpi_tty:Term ; tpi_cty:Code ; tpi_f:Term*Code -> Term }
-- { tapp_f:Term ; tapp_a:Term ; tapp_ca:Code }
-- { ttype=true }
-- { tbox_cty:Code }

function force ( c )
  if c.clazy then return force (c.clazy())
  else return c
  end
end

function force2 ( c )
  if c.clazy then return force (c.clazy())
  elseif c.id then 
    local args = {}
    for i=1,#c.args do args[i] = force2(c.args[i]) end
    return { cid=c.cid ; arity=c.arity ; args=args ; f=c.f }
  else return c
  end
end

function passert ( cond , msg , lvl )
  if cond then return end
  if lvl then error(msg,lvl)
  else error(msg,0) end
end

function is_code( c )
  if type(c) == 'table' then
    if c.cid 		then return ( c.args ~= nil ) 
    elseif c.cpi_cty	then return ( c.cpi_f ~= nil )
    elseif c.clam_f 	then return true
    elseif c.clazy 	then return true
    elseif c.ctype 	then return true
    elseif c.ckind 	then return true
    end
  end
  return false
end

function is_term( t )
  if type(t) == 'table' then
    if t.tpi_tty 	then return ( t.tpi_cty and t.tpi_f )
    elseif t.tapp_f 	then return ( t.tapp_ca and t.tapp_a ) 
    elseif t.tlam_f 	then return true
    elseif t.ttype 	then return true 
    elseif t.tbox_cty	then return true 
    end
  end
  return false
end

local fresh_var = 0

-- unit -> Code
function mk_var ( )
  i = fresh_var
  fresh_var = i+1
  return { cid = ("var"..i) ; args = { } } 
end

-- unit -> int*Code
function mk_var2 ( )
  i = fresh_var
  fresh_var = i+1
  return i,{ cid = ("var"..i) ; args = { } } 
end

-- Code --> Term
function mk_box ( ty )
  return { tbox_cty = ty }
end

function push (a,t)
  passert( type(t) == 'table' , "[ Lua ] Error in function push." )
  local res = { }
  res[#t+1] = a
  for i=1,#t do res[i] = t[i] end
  return res
end

function split (t,n)
  passert(type(t) == 'table' , "[ Lua ] Error in function split." )
  local t1 = { }
  local t2 = { }
  for i=1,n do t1[i] = t[i] end
  for i=n+1,#t do t2[i-n] = t[i] end
  return t1,t2
end 

function uapp0 ( c )
  if c.arity == 0 then 
    res = c.f()
    if res then return res end
  end
  return c
end

-- Code*Code -> Code
function uapp ( f , arg )
  --print(" -- entering app...")
  --print(" @f : " .. string_of_code(50,f))
  --print(" @a : " .. string_of_code(50,arg))
  passert(is_code(f) and is_code(arg) , "[ Lua ] Error in function uapp." )

  if f.clazy then -- LAZY 
    return uapp ( f.clazy() , arg )

  elseif f.cid then -- CONS
    local args = push(arg,f.args)
    if not f.arity then return { cid=f.cid ; args=args } end 
    
    if f.arity == #args then -- Rewrite
      local red = f.f(unpack(args))
      if red then 
	return red
      else 
	return { cid=f.cid ; arity=f.arity ; f=f.f ; args=args }
      end
      
    elseif #args < f.arity then -- Argument collecting
      return { cid=f.cid ; arity=f.arity ; f=f.f ; args=args }
    
    else -- Too much arguments
      local t1,t2 = split(args,f.arity)
      local red = f.f(unpack(t1))
      if red then
	for i=1,#t2 do red = uapp ( red , t2[i] ) end
        return red
      else
	return { cid=f.cid ; arity=f.arity ; f=f.f ; args=args }
      end 
    end
    
  elseif f.clam_f then -- LAMBDA
    return f.clam_f(arg)

  else -- OTHER
    error("Error in uapp.",0)
  end
end

function is_conv ( ty1 , ty2 )
  --print ( "entering is_conv ...")
  --print(" @ Type1: " .. string_of_code(ty1))
  --print(" @ Type2: " .. string_of_code(ty2))
  passert( is_code(ty1) and is_code(ty2) , "[ Lua ] Error in is_conv." )
  if ty1.clazy then return is_conv ( ty1.clazy() , ty2 ) end
  if ty2.clazy then return is_conv ( ty1 , ty2.clazy() ) end

  if     ty1.ckind and ty2.ckind then return true	-- Kind

  elseif ty1.ctype and ty2.ctype then return true       -- Type
  
  elseif ty1.clam_f  and ty2.clam_f  then                   -- Lam
    local var = mk_var( )
    return is_conv ( ty1.clam_f(var) , ty2.clam_f(var) )
  
  elseif ty1.cpi_cty and ty2.cpi_cty then       	-- Pi 
  --  assert(is_code(ty1.cpi_cty))
  --  assert(is_code(ty2.cpi_cty))
    if is_conv ( ty1.cpi_cty , ty2.cpi_cty ) then 
      local var = mk_var( )
      return is_conv ( ty1.cpi_f(var) , ty2.cpi_f(var) ) 
    else 
      return false
    end
  
  elseif ty1.cid and ty2.cid then                       -- Cons
    if     ty1.cid    ~= ty2.cid  then return false
    elseif #ty1.args ~= #ty2.args then return false
    else
      for i=1,#ty1.args do
        if not is_conv( ty1.args[i] , ty2.args[i] ) then 
	  return false 
	end
      end
      return true
    end
  
  else							-- Else
    return false
  end
end

-- Term * Code --> unit
function type_check ( te , ty )
  --print(" -- entering type_check ...")
  --print("@Term: " .. string_of_term( te ) )
  --print("@Type: " .. string_of_code( ty ) )
  passert( is_term(te) and is_code(ty) , "[ Lua ] Error in function type_check." )

  -- LAMBDA
  if te.tlam_f then
    if ty.clazy then type_check( te , ty.clazy() )
    elseif ty.cpi_cty then
      -- Type Annotations BEGIN
      if te.tlam_tty then
	type_check ( te.tlam_tty , { ctype=true } )
	--assert(is_code(te.tlam_cty))
	if not is_conv ( te.tlam_cty , ty.cpi_cty ) then 
	  error("Lambda Annotation Error.\nCannot Convert:" 
	  .. string_of_code(te.tlam_cty) .. "\nwith\n" 
	  .. string_of_code(ty.cpi_cty) , 0 ) 
	end
      end
      -- Type Annotations END
      local var = mk_var( )
      local te2 = te.tlam_f ( mk_box (ty.cpi_cty) , var )
      local ty2 = ty.cpi_f( var )
      --assert(is_term(te2))
      --assert(is_code(ty2))
      type_check ( te2 , ty2 )
    else 
      error("Product Expected:\n" .. string_of_code(ty),0) 
    end
  
  -- PI
  elseif te.tpi_tty  then
    type_check ( te.tpi_tty , { ctype=true } ) ;
    if  ty.ctype then 
      local te2 = te.tpi_f ( mk_box(te.tpi_cty) , mk_var() )
      --assert(te2)
      type_check ( te2 , { ctype=true } )
    elseif ty.ckind then 
      local te2 = te.tpi_f ( mk_box(te.tpi_cty) , mk_var() )
      --assert(te2)
      type_check ( te2 , { ckind=true } ) 
    elseif ty.clazy then 
      type_check( te , ty.clazy() )
    else 
      error("Sort Error:\n" .. string_of_code(ty) , 0 )
    end

  -- OTHER
  else 
    local ty2 = type_synth ( te ) 
    --assert(is_code(ty2))
    if not is_conv ( ty2 , ty ) then 
      error("Cannot convert:\n" .. string_of_code(ty2) .. "\nwith\n" .. string_of_code(ty) , 0 )
    end
  end
  --print(" -- leaving type_check ...")
end

-- int * Term --> Code
function type_gen ( te )
  -- Type
  if te.ttype 		then	return { ckind=true } 
  -- Box
  elseif te.tbox_cty 	then 	return te.tbox_cty 
  -- Lam
  elseif te.tlam_f  	then     
    --assert(te.tlam_cty)
    local box = mk_box(te.tlam_cty)
    return { cpi_cty = te.tlam_cty ; cpi_f = function(x) return type_gen( te.tlam_f(box,x) ) end }
  -- App
  elseif te.tapp_f  	then
    --assert (te.tapp_a)
    --assert (te.tapp_ca)
    local tyf = force ( type_gen ( te.tapp_f ) )
    return tyf.cpi_f( te.tapp_ca )
  -- PI
  elseif te.tpi_tty  then
    local te2 = te.tpi_f ( mk_box(te.tpi_cty) , mk_var() )
    return type_gen( te2 ) 
  -- Error
  else  
    assert(false)
  end
end

-- int * Term --> Code
function type_synth ( te )
  --print(" -- entering type_synth ...")
  --print ("@Term: " .. string_of_term(te ))
  passert( is_term(te) , "[ Lua ] Error in function type_synth." )

  -- Type
  if     te.ttype 	then	return { ckind=true }
  -- Box
  elseif te.tbox_cty  	then	return te.tbox_cty
  -- Lam
  elseif te.tlam_f  	then
    if te.tlam_tty 	then 
      type_check( te.tlam_tty , { ctype=true } )
      local box = mk_box( te.tlam_cty )
      local dummy = type_synth( te.tlam_f ( box , mk_var( ) ) )
      return { cpi_cty = te.tlam_cty ; cpi_f = function( x ) return type_gen( te.tlam_f(box,x) ) end }
    else
      error("Cannot find type of:\n" .. string_of_term( te ) , 0 )
    end
  -- App
  elseif te.tapp_f  	then
    --assert(te.tapp_a) -- 
    --assert(te.tapp_ca) -- 
    local tyf = force ( type_synth ( te.tapp_f ) )
    if tyf.cpi_cty then 
      type_check ( te.tapp_a , tyf.cpi_cty )
      --assert(tyf.cpi_f) --
      return tyf.cpi_f( te.tapp_ca )
    else
      error("Product expected:\n" .. string_of_code( tyf ) , 0 ) 
    end 
  -- PI
  elseif te.tpi_tty  then
    type_check ( te.tpi_tty , { ctype=true } ) ;
    local te2 = te.tpi_f ( mk_box(te.tpi_cty) , mk_var() )
    local ty = force( type_synth( te2 ) )
    if     ty.ctype then return { ctype=true }
    elseif ty.ckind then return { ckind=true }
    else 
      error("Sort Error:\n" .. string_of_code(ty) , 0 )
    end

    -- Default
  else                                                  
    error("Cannot find type of:\n" .. string_of_term(te) , 0 )
  end
end

function print_debug ( msg )
  if debug_infos then io.stderr:write(msg) end
end

function print_ok_ko ( status , msg )
  if status then 
    if debug_infos then io.stderr:write("\027[32m[OK]\027[m\n") end
  else 
    if debug_infos then io.stderr:write("\027[31m[KO]\027[m\n") end
    io.stderr:write(" ##############################\n")
    io.stderr:write(msg .. "\n")
    os.exit(1)
  end
end

function print_ok_ko2 ( status , msg )
  if not status then 
    if debug_infos then io.stderr:write("\027[31m[KO]\027[m\n") end
    io.stderr:write(" ##############################\n")
    io.stderr:write(msg .. "\n")
    os.exit(1)
  end
end

function check_ext ( ext , msg )
  if not ext then
    io.stderr:write("\n ##############################\n")
    io.stderr:write(msg .. "\n")
    os.exit(1)
  end
end    

-- Code*Code -> Code
function app ( f , a )
  status,res = pcall ( uapp , f , a )
  print_ok_ko2(status,res)
  return res
end

-- Code -> Code
function app0 ( a )
  status,res = pcall ( uapp0 , a )
  print_ok_ko2(status,res)
  return res
end

-- Term -> unit
function chktype ( t )
  status,msg = pcall ( type_check , t , { ctype=true } ) 
  print_ok_ko(status,msg)
end

-- Term -> unit
function chkkind ( t )
  status,msg = pcall ( type_check , t , { ckind=true } ) 
  print_ok_ko(status,msg)
end

-- Term*Code -> unit
function chk ( t , c )
  status,msg = pcall ( type_check , t , c ) 
  print_ok_ko(status,msg)
end

--[[ Utility functions. ]]

function string_of_code ( c ) 
  if not is_code(c) 	then return "@@@ ERROR @@@"
  elseif c.ctype	then return "Type"
  elseif c.ckind 	then return "Kind"
  elseif c.clazy 	then return ("(Lazy " .. string_of_code (c.clazy()) .. ")")
  elseif c.cpi_cty  	then 
    local n,var = mk_var2 ( )
    return ("(var" .. n .. " : " .. string_of_code(c.cpi_cty) .. " -> " .. string_of_code(c.cpi_f(var)) .. ")")
  elseif c.clam_f 	then
    local n,var = mk_var2 ( )
    return ("(var" .. n .. " => " .. string_of_code(c.clam_f(var)) .. ")" )
  elseif c.cid 		then
    local str = c.cid
    for i=1,#c.args do
      str = str .. " " .. string_of_code(c.args[i])
    end
    if #c.args==0 then return str else return "( " .. str .. " )" end
  else
    assert(false)
  end
end
 
function string_of_term ( t )
  if not is_term(t) 	then return "@@@ ERROR @@@"
  elseif t.ttype	then return "Type"
  elseif t.tbox_cty 	then return "(Box " .. string_of_code( t.tbox_cty ) .. ")"
  elseif  t.tlam_f 	then
    local n,var = mk_var2 ( )
    if t.tlam_cty then
      return ("(var" .. n .. " : " .. string_of_code(t.tlam_cty).. " => " .. string_of_term( t.tlam_f(mk_box(var),var) ) .. ")" )
    else 
      return ("(var" .. n .. " => " .. string_of_term( t.tlam_f(mk_box(var),var) ) .. ")" )
    end
  elseif t.tpi_f  	then 
    local n,var = mk_var2 ( )
    return ( "(var" .. n .. " : " .. string_of_term( t.tpi_tty ) .. " -> " .. string_of_term( t.tpi_f(mk_box(var),var) ) .. ")" )
  elseif t.tapp_f 	then 
    return "(App " .. string_of_term( t.tapp_f ) .. " " .. string_of_term( t.tapp_a ) .. ")" 
  else 
    assert(false)
  end
end
    
-- vi: expandtab: sw=2
