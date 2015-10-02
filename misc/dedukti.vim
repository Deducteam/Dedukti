" Vim syntax file
" Language: Dedukti
" Maintainer: Ronan Saillard
" Latest Revision: 02/09/2015
"
if exists("b:current_syntax")
	finish
endif

syn keyword dkType Type def
syn keyword dkTodo contained TODO FIXME

syn match dkLongArrow 	'-->'
syn match dkArrow 	'->'
syn match dkFatArrow 	'=>'
syn match dkColon 	'\:'
syn match dkDef 	'\:='
syn match dkDummy 	'_'

syn match dkParL 	'('
syn match dkParR 	')'

syn match dkId		'\([a-zA-Z0-9_]\+\.\)\?[a-zA-Z0-9_][a-zA-Z0-9_!?\']*'
syn match dkDep		'#[A-Z]\+'

syn region dkComment	start="(;" 	end=";)" fold contains=dkTodo
syn region dkEnv 	start='\['	end='\]' contains=rien
syn region dkString 	start='"'	end='"' contains=rien

hi def link dkComment	Comment
hi def link dkString	String
hi def link dkTodo    	Todo
hi def link dkId	Identifier
hi def link dkDep	PreProc
hi def link dkMod	Special
hi def link dkType	Type

hi def link dkEnv	Statement
hi def link dkLongArrow	Special

hi def link dkFatArrow	Special
hi def link dkArrow	Special

hi def link dkColon	Constant
hi def link dkDef	Constant
hi def link dkDummy	Constant
