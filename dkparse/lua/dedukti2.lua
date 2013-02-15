-- Dedukti LUA basic runtime.

-- Code 
 ccon, clam, cpi, ctype, ckind = 'ccon', 'clam', 'cpi', 'ctype', 'ckind';
-- { co = ccon ; id:string ; arity:int ; f:Code^arity -> Code option ; args:Code* }
-- { co = clam ; f:Code -> Code }
-- { co = cpi  ; ctype:Code ; f:Code -> Code }
-- { co = ctype }
-- { co = ckind }

function passert ( cond , msg , lvl )
	if cond then return end
	if lvl then error(msg,lvl)
	else error(msg,0) end
end

function is_code( c )
	if type(c) == 'table' then
		if c.co ~= nil then return true end
	end
	return false
end
function is_term( t )
	if type(t) == 'table' then
		if t.te ~= nil then return true end
	end
	return false
end

-- int -> Code
function mk_var ( i )
  return { co = ccon, id=("var"..i) ; arity = 0 ; f = function() return nil end ; args = { }  };
end

function push (a,t)
  passert( type(t) == 'table' , "Lua Error (1)." )
  local res = { }
  res[#t+1] = a
  for i=1,#t do
    res[i] = t[i]
  end
  return res
end

function split (t,n)
  passert(type(t) == 'table' , "Lua Error (2)." )
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
function uapp0 ( f )
  --print(" -- entering app0...")
  --print(" @f : " .. string_of_code(50,f))
  passert(is_code(f) , "Undefined external symbol (1)." )
  local res = f
  if f.co == ccon     then 
	  if f.arity == 0 then
		  local f0 = f.f()
		  if f0 ~= nil then res = f0 end
	  end
  end
  --print(" -- leaving app0...")
  --print(" @App0 : " .. string_of_code(50,res))
  return res
end

-- Code*Code -> Code
function uapp ( f , arg )
  --io.stderr:write(" -- entering uapp...\n")
  --io.stderr:write(" @f : " .. string_of_code(50,f) .. "\n")
  --io.stderr:write(" @a : " .. string_of_code(50,arg) .. "\n")
  passert(is_code(f) and is_code(arg) , "Undefined external symbol (2)." )

  local res = nil

  -- CCON
  if     f.co == ccon then
    local args = push(arg,f.args)
    if     f.arity == #args then
      local red = f.f(unpack(args))
      if red ~= nil then 
	      res= red
      else
        res = { co = ccon ; id=f.id ; arity=f.arity ; f=f.f ; args=args }
      end
    else
      if #args < f.arity then
        res = { co = ccon ; id=f.id ; arity=f.arity ; f=f.f ; args=args }
      else
        local t1,t2 = split(args,f.arity)
        local red = f.f(unpack(t1))
        if red ~= nil then
          for i=1,#t2 do
            red = uapp ( red , t2[i] )
          end
          res = red
        else
          res = { co = ccon ; id=f.id ; arity=f.arity ; f=f.f ; args=args }
        end 
      end
    end
  -- CLAM
  elseif f.co == clam then
    res = f.f(arg)
  -- ERROR
  else
    error("Lua Error (3).",0)
  end
  --io.stderr:write ("@App : " .. string_of_code(10,res) .. "\n")
  --io.stderr:write(" -- leaving uapp...\n")
  return res
end

-- Code*Code -> Code
function pi_app0 ( f , arg )
	if f.co == cpi then return f.f(arg)
	else error("Lua Error (4).",0)
	end
end

function is_conv ( n , ty1 , ty2 )
  --print ( "entering is_conv ...")
  --print(" @ Type1: " .. string_of_code(n,ty1))
  --print(" @ Type2: " .. string_of_code(n,ty2))
  passert( is_code(ty1) and is_code(ty2) , "Undefined external symbol (3)." )

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

function is_convPi ( ty1 , ty2 )
	if ty1.co == cpi then
		return is_conv(0,ty1.ctype,ty2)
	else
		return false -- TODO
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

function print_ok()
	if debug_infos then io.stderr:write("\027[32m[OK]\027[m\n") end
end

function app0 ( f )
	status,res = pcall ( uapp0 , f )
	print_ok_ko2(status,res)
	return res
end

function app ( f , a )
	status,res = pcall ( uapp , f , a )
	print_ok_ko2(status,res)
	return res
end

function pi_app ( f , a )
	status,res = pcall ( pi_app0 , f , a )
	print_ok_ko2(status,res)
	return res
end

-- Code*Code -> unit
function conv ( t1 , t2 )
  status,res = pcall ( is_conv , 0 , t1 , t2 ) 
  if not status then 
	  if debug_infos then io.stderr:write("\027[31m[KO]\027[m\n") end
	  io.stderr:write(" ##############################\n")
	  io.stderr:write(res .. "\n")
	  os.exit(1)
  end
  if not res then
	  if debug_infos then io.stderr:write("\027[31m[KO]\027[m\n") end
	  io.stderr:write(" ##############################\n")
	  io.stderr:write("Not Convertible.\n")
	  os.exit(1)
  end
end

-- Code*Code -> unit
function convPi ( t1 , t2 )
  status,res = pcall ( is_convPi , t1 , t2 ) 
  if not status then 
	  if debug_infos then io.stderr:write("\027[31m[KO]\027[m\n") end
	  io.stderr:write(" ##############################\n")
	  io.stderr:write(res .. "\n")
	  os.exit(1)
  end
  if not res then
	  if debug_infos then io.stderr:write("\027[31m[KO]\027[m\n") end
	  io.stderr:write(" ##############################\n")
	  io.stderr:write("Not Pi-Convertible.\n")
	  os.exit(1)
  end
end


--[[ Utility functions. ]]

function string_of_code ( n , c )
--	 if type(c) ~= 'table' then return "@@@ ERROR ".. type(c) .." @@@" end 
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
    else return "( " .. str .. " )"
    end
  else
    --return "Error"
    error('string_of_code')
  end
end
   
-- vi: expandtab: sw=2
