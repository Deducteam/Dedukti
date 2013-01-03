require('dedukti')
--[[ Code for module coc ]]
coc = { }

coc.Utype_c = { co = ccon ; id = "coc.Utype" ; arity = 0 ; args = { } ; f = function() return nil end }
coc.Utype_t = { te = tbox ; ctype = function() return { co = ctype } end }

coc.Ukind_c = { co = ccon ; id = "coc.Ukind" ; arity = 0 ; args = { } ; f = function() return nil end }
coc.Ukind_t = { te = tbox ; ctype = function() return { co = ctype } end }

coc.etype_c = { co = ccon ; id = "coc.etype" ; arity = 0 ; args = { } ; f = function() return nil end }
coc.etype_t = { te = tbox ; ctype = function() return { co = cpi ; ctype = coc.Utype_c ; f = function (dummy) return { co = ctype } end } end }

coc.ekind_c = { co = ccon ; id = "coc.ekind" ; arity = 0 ; args = { } ; f = function() return nil end }
coc.ekind_t = { te = tbox ; ctype = function() return { co = cpi ; ctype = coc.Ukind_c ; f = function (dummy) return { co = ctype } end } end }

coc.dottype_c = { co = ccon ; id = "coc.dottype" ; arity = 0 ; args = { } ; f = function() return nil end }
coc.dottype_t = { te = tbox ; ctype = function() return coc.Ukind_c end }

coc.dotpi1_c = { co = ccon ; id = "coc.dotpi1" ; arity = 0 ; args = { } ; f = function() return nil end }
coc.dotpi1_t = { te = tbox ; ctype = function() return { co = cpi ; ctype = coc.Utype_c ; f = function (x_c) return { co = cpi ; ctype = { co = cpi ; ctype = app( coc.etype_c , x_c ) ; f = function (dummy) return coc.Utype_c end } ; f = function (y_c) return coc.Utype_c end } end } end }

coc.dotpi2_c = { co = ccon ; id = "coc.dotpi2" ; arity = 0 ; args = { } ; f = function() return nil end }
coc.dotpi2_t = { te = tbox ; ctype = function() return { co = cpi ; ctype = coc.Utype_c ; f = function (x_c) return { co = cpi ; ctype = { co = cpi ; ctype = app( coc.etype_c , x_c ) ; f = function (dummy) return coc.Ukind_c end } ; f = function (y_c) return coc.Ukind_c end } end } end }

coc.dotpi3_c = { co = ccon ; id = "coc.dotpi3" ; arity = 0 ; args = { } ; f = function() return nil end }
coc.dotpi3_t = { te = tbox ; ctype = function() return { co = cpi ; ctype = coc.Ukind_c ; f = function (x_c) return { co = cpi ; ctype = { co = cpi ; ctype = app( coc.ekind_c , x_c ) ; f = function (dummy) return coc.Utype_c end } ; f = function (y_c) return coc.Utype_c end } end } end }

coc.dotpi4_c = { co = ccon ; id = "coc.dotpi4" ; arity = 0 ; args = { } ; f = function() return nil end }
coc.dotpi4_t = { te = tbox ; ctype = function() return { co = cpi ; ctype = coc.Ukind_c ; f = function (x_c) return { co = cpi ; ctype = { co = cpi ; ctype = app( coc.ekind_c , x_c ) ; f = function (dummy) return coc.Ukind_c end } ; f = function (y_c) return coc.Ukind_c end } end } end }


 -- [[ Compiling rules of coc.etype. ]]
coc.etype_c = { co = ccon ; id="coc.etype" ; arity = 1 ; args = { } ; f =
function (y1)
if y1.co == ccon and y1.id == "coc.dotpi3" then
local x_c, y_c = y1.args[1], y1.args[2]
return { co = cpi ; ctype = app( coc.ekind_c , x_c ) ; f = function (w_c) return app( coc.etype_c , app( y_c , w_c ) ) end }
elseif y1.co == ccon and y1.id == "coc.dotpi1" then
local x_c, y_c = y1.args[1], y1.args[2]
return { co = cpi ; ctype = app( coc.etype_c , x_c ) ; f = function (w_c) return app( coc.etype_c , app( y_c , w_c ) ) end }
else
return nil
end
end }


 -- [[ Compiling rules of coc.ekind. ]]
coc.ekind_c = { co = ccon ; id="coc.ekind" ; arity = 1 ; args = { } ; f =
function (y1)
if y1.co == ccon and y1.id == "coc.dotpi4" then
local x_c, y_c = y1.args[1], y1.args[2]
return { co = cpi ; ctype = app( coc.ekind_c , x_c ) ; f = function (w_c) return app( coc.ekind_c , app( y_c , w_c ) ) end }
elseif y1.co == ccon and y1.id == "coc.dotpi2" then
local x_c, y_c = y1.args[1], y1.args[2]
return { co = cpi ; ctype = app( coc.etype_c , x_c ) ; f = function (w_c) return app( coc.ekind_c , app( y_c , w_c ) ) end }
elseif y1.co == ccon and y1.id == "coc.dottype" then
return coc.Utype_c
else
return nil
end
end }

coc.a_c = { co = ccon ; id = "coc.a" ; arity = 0 ; args = { } ; f = function() return nil end }
coc.a_t = { te = tbox ; ctype = function() return { co = cpi ; ctype = coc.Utype_c ; f = function (x_c) return { co = cpi ; ctype = app( coc.etype_c , x_c ) ; f = function (y_c) return app( coc.etype_c , x_c ) end } end } end }


 -- [[ Compiling rules of coc.a. ]]
coc.a_c = { co = ccon ; id="coc.a" ; arity = 0 ; args = { } ; f =
function ()
return { co = clam ; f = function (x_c) return { co = clam ; f = function (y_c) return y_c end } end }
end }

