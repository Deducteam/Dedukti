require('dedukti')
--[[ Code for module logic ]]
logic = { }

require("coc")
logic.False_c = { co = ccon ; id = "logic.False" ; arity = 0 ; args = { } ; f = function() return nil end }
logic.False_t = { te = tbox ; ctype = function() return app0(coc.Utype_c) end }

logic.True_c = { co = ccon ; id = "logic.True" ; arity = 0 ; args = { } ; f = function() return nil end }
logic.True_t = { te = tbox ; ctype = function() return app0(coc.Utype_c) end }

logic.I_c = { co = ccon ; id = "logic.I" ; arity = 0 ; args = { } ; f = function() return nil end }
logic.I_t = { te = tbox ; ctype = function() return app( app0(coc.etype_c) , logic.True_c ) end }

logic.eq_c = { co = ccon ; id = "logic.eq" ; arity = 0 ; args = { } ; f = function() return nil end }
logic.eq_t = { te = tbox ; ctype = function() return { co = cpi ; ctype = app0(coc.Utype_c) ; f = function (t_c) return { co = cpi ; ctype = app( app0(coc.etype_c) , t_c ) ; f = function (dummy) return { co = cpi ; ctype = app( app0(coc.etype_c) , t_c ) ; f = function (dummy) return { co = ctype } end } end } end } end }

logic.eq__c = { co = ccon ; id = "logic.eq_" ; arity = 0 ; args = { } ; f = function() return nil end }
logic.eq__t = { te = tbox ; ctype = function() return { co = cpi ; ctype = app0(coc.Utype_c) ; f = function (t_c) return { co = cpi ; ctype = app( app0(coc.etype_c) , t_c ) ; f = function (dummy) return { co = cpi ; ctype = app( app0(coc.etype_c) , t_c ) ; f = function (dummy) return app0(coc.Utype_c) end } end } end } end }


 -- [[ Compiling rules of logic.eq. ]]
logic.eq_c = { co = ccon ; id="logic.eq" ; arity = 3 ; args = { } ; f =
function (y1, y2, y3)
local t_c, x_c, y_c = y1, y2, y3
return app( app0(coc.etype_c) , app( app( app( logic.eq__c , t_c ) , x_c ) , y_c ) )
end }

logic.refl_equal_c = { co = ccon ; id = "logic.refl_equal" ; arity = 0 ; args = { } ; f = function() return nil end }
logic.refl_equal_t = { te = tbox ; ctype = function() return { co = cpi ; ctype = app0(coc.Utype_c) ; f = function (t_c) return { co = cpi ; ctype = app( app0(coc.etype_c) , t_c ) ; f = function (x_c) return app( app( app( logic.eq_c , t_c ) , x_c ) , x_c ) end } end } end }

logic.eq_rec_c = { co = ccon ; id = "logic.eq_rec" ; arity = 0 ; args = { } ; f = function() return nil end }
logic.eq_rec_t = { te = tbox ; ctype = function() return { co = cpi ; ctype = app0(coc.Utype_c) ; f = function (t_c) return { co = cpi ; ctype = app( app0(coc.etype_c) , t_c ) ; f = function (x_c) return { co = cpi ; ctype = { co = cpi ; ctype = app( app0(coc.etype_c) , t_c ) ; f = function (dummy) return app0(coc.Utype_c) end } ; f = function (p_c) return { co = cpi ; ctype = app( app0(coc.etype_c) , app( p_c , x_c ) ) ; f = function (g_c) return { co = cpi ; ctype = app( app0(coc.etype_c) , t_c ) ; f = function (y_c) return { co = cpi ; ctype = app( app( app( logic.eq_c , t_c ) , x_c ) , y_c ) ; f = function (h_c) return app( app0(coc.etype_c) , app( p_c , y_c ) ) end } end } end } end } end } end } end }

logic.f_equal_c = { co = ccon ; id = "logic.f_equal" ; arity = 0 ; args = { } ; f = function() return nil end }
logic.f_equal_t = { te = tbox ; ctype = function() return { co = cpi ; ctype = app0(coc.Utype_c) ; f = function (A_c) return { co = cpi ; ctype = app0(coc.Utype_c) ; f = function (B_c) return { co = cpi ; ctype = { co = cpi ; ctype = app( app0(coc.etype_c) , A_c ) ; f = function (dummy) return app( app0(coc.etype_c) , B_c ) end } ; f = function (f_c) return { co = cpi ; ctype = app( app0(coc.etype_c) , A_c ) ; f = function (x_c) return { co = cpi ; ctype = app( app0(coc.etype_c) , A_c ) ; f = function (y_c) return { co = cpi ; ctype = app( app( app( logic.eq_c , A_c ) , x_c ) , y_c ) ; f = function (H_c) return app( app( app( logic.eq_c , B_c ) , app( f_c , x_c ) ) , app( f_c , y_c ) ) end } end } end } end } end } end } end }


 -- [[ Compiling rules of logic.f_equal. ]]
logic.f_equal_c = { co = ccon ; id="logic.f_equal" ; arity = 0 ; args = { } ; f =
function ()
return { co = clam ; f = function (A_c) return { co = clam ; f = function (B_c) return { co = clam ; f = function (f_c) return { co = clam ; f = function (x_c) return { co = clam ; f = function (y_c) return { co = clam ; f = function (H_c) return app( app( app( app( app( app( logic.eq_rec_c , A_c ) , x_c ) , { co = clam ; f = function (z_c) return app( app( app( logic.eq__c , B_c ) , app( f_c , x_c ) ) , app( f_c , z_c ) ) end } ) , app( app( logic.refl_equal_c , B_c ) , app( f_c , x_c ) ) ) , y_c ) , H_c ) end } end } end } end } end } end }
end }

