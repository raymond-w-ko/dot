" vim:fdm=marker:foldlevel=0
"  ____           _
" |  _ \ _ __ ___| |_ ___
" | |_) | '__/ _ \ __/ _ \
" |  __/| | |  __/ || (_) |
" |_|   |_|  \___|\__\___/
"
" A minimal dark theme for VIM

" Reset -------------------------------------------------------------------{{{1

set background=dark
highlight clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "preto"


" Palette ---------------------------------------------------------------- {{{1

let s:palette = {}

let s:palette.black  	 = [16 , "#000000"]
let s:palette.gray01 	 = [232, "#080808"]
let s:palette.gray02 	 = [233, "#121212"]
let s:palette.gray03 	 = [234, "#1c1c1c"]
let s:palette.gray04 	 = [235, "#262626"]
let s:palette.gray05 	 = [236, "#303030"]
let s:palette.gray06 	 = [237, "#3a3a3a"]
let s:palette.gray07 	 = [238, "#444444"]
let s:palette.gray08 	 = [239, "#4e4e4e"]
let s:palette.gray09 	 = [240, "#585858"]
let s:palette.gray10 	 = [241, "#606060"]
let s:palette.gray11 	 = [242, "#666666"]
let s:palette.gray12 	 = [243, "#767676"]
let s:palette.gray13 	 = [244, "#808080"]
let s:palette.gray14 	 = [245, "#8a8a8a"]
let s:palette.gray15 	 = [246, "#949494"]
let s:palette.gray16 	 = [247, "#9e9e9e"]
let s:palette.gray17 	 = [248, "#a8a8a8"]
let s:palette.gray18 	 = [249, "#b2b2b2"]
let s:palette.gray19 	 = [250, "#bcbcbc"]
let s:palette.gray20 	 = [251, "#c6c6c6"]
let s:palette.gray21 	 = [252, "#d0d0d0"]
let s:palette.gray22 	 = [253, "#dadada"]
let s:palette.gray23 	 = [254, "#e4e4e4"]
let s:palette.white  	 = [255, "#eeeeee"]

let s:palette.cyan        = [6  , "#008080"]
let s:palette.darkblue    = [18 , "#000087"]
let s:palette.darkgreen   = [22 , "#005f00"]
let s:palette.dampgreen   = [28 , "#008700"]
let s:palette.darkcyan    = [31 , "#0087af"]
let s:palette.dullgreen   = [34 , "#00af00"]
let s:palette.puregreen   = [82 , "#00ff00"]
let s:palette.blue        = [33 , "#0087ff"]
let s:palette.green       = [42 , "#00d787"]
let s:palette.darkred     = [52 , "#5f0000"]
let s:palette.darkpurple  = [53 , "#5f005f"]
let s:palette.darkyellow  = [58 , "#5f5f00"]
let s:palette.red         = [124, "#af0000"]
let s:palette.lightorange = [136, "#af8700"]
let s:palette.purple      = [139, "#af87af"]
let s:palette.brown       = [130, "#af5f00"]
let s:palette.orange      = [166, "#d75f00"]
let s:palette.purered     = [196, "#ff0000"]
let s:palette.pink        = [200, "#ff00d7"]
let s:palette.lightpurple = [219, "#ffafff"]
let s:palette.yellow      = [228, "#ffff87"]


" Utilities -------------------------------------------------------------- {{{1

function! s:HL(item, fgColor, bgColor, style, ...)
	let undesirable_runtimes = a:000
	for runtime in undesirable_runtimes
		if has(runtime)
			return	
		end
	endfor

	if has('gui_running')
		let target = 'gui'
		let pindex = 1
	else
		let target = 'cterm'
		let pindex = 0
	end

	let command  = 'hi ' . a:item
	if type(a:fgColor) == v:t_list
		let command .= ' ' . target . 'fg=' . a:fgColor[pindex]
	endif
	if type(a:bgColor) == v:t_list
		let command .= ' ' . target . 'bg=' . a:bgColor[pindex]
	elseif target == "gui"
		let command .= ' ' . target . 'bg=' . s:palette.black[pindex]
	endif
	let command .= ' ' . target . '=' . a:style

	execute command
endfunction


" Composition ------------------------------------------------------------ {{{1

let s:color_bg = s:palette.black
let s:color_alpha = s:palette.dullgreen
let s:color_beta = s:palette.darkcyan
let s:color_delta = s:palette.orange

" PRIMITIVES
call s:HL('Boolean',        s:color_alpha,       s:color_bg,          'none')
call s:HL('Character',      s:color_alpha,       s:color_bg,          'none')
call s:HL('Constant',       s:color_alpha,       s:color_bg,          'none')
call s:HL('Float',          s:color_alpha,       s:color_bg,          'none')
call s:HL('Number',         s:color_alpha,       s:color_bg,          'none')
call s:HL('String',         s:color_alpha,       s:color_bg,          'none')
call s:HL('SpecialChar',    s:palette.puregreen, s:color_bg,          'none')

" COMMENTS
call s:HL('Comment',        s:color_delta,       s:color_bg,          'none')
call s:HL('SpecialComment', s:color_delta,       s:color_bg,          'none')
call s:HL('Title',          s:color_alpha,       s:color_bg,          'none')
call s:HL('Todo',           s:palette.black,     s:palette.puregreen, 'none')

" LINES, COLUMNS
call s:HL('LineNr',         s:color_delta,       s:color_bg,          'none')
call s:HL('CursorLine',     s:palette.white,     s:palette.gray03,    'none')
call s:HL('CursorLineNr',   s:color_alpha,       s:color_bg,          'none')

call s:HL('ColorColumn',    s:palette.white,     s:palette.gray03,    'none')
call s:HL('CursorColumn',   s:palette.gray16,    s:palette.gray03,    'none')

" VISUAL MODE
call s:HL('Visual',         s:palette.green,     s:palette.gray03,    'none')
call s:HL('VisualNOS',      s:palette.green,     s:palette.gray03,    'none')

" SEARCH
call s:HL('Search',         s:palette.black,     s:palette.yellow,    'none')
call s:HL('IncSearch',      s:palette.yellow,    s:color_bg,          'none')

" SPELLING
call s:HL('SpellBad',       s:palette.black,     s:palette.yellow,    'none')
call s:HL('SpellCap',       s:palette.black,     s:palette.yellow,    'none')
call s:HL('SpellLocal',     s:palette.black,     s:palette.yellow,    'none')
call s:HL('SpellRare',      s:palette.black,     s:palette.yellow,    'none')

" ERROR
call s:HL('Error',          s:palette.white,     s:palette.purered,   'none')

" COMMAND MODE MESSAGES
call s:HL('ErrorMsg',       s:palette.yellow,    s:color_bg,          'none')
call s:HL('WarningMsg',     s:palette.brown,     s:color_bg,          'none')
call s:HL('ModeMsg',        s:palette.white,     s:color_bg,          'none')
call s:HL('MoreMsg',        s:palette.white,     s:color_bg,          'none')

" PREPROCESSOR DIRECTIVES
call s:HL('Include',        s:color_alpha,       s:color_bg,          'none')
call s:HL('Define',         s:color_alpha,       s:color_bg,          'none')
call s:HL('Macro',          s:color_alpha,       s:color_bg,          'none')
call s:HL('PreCondit',      s:color_alpha,       s:color_bg,          'none')
call s:HL('PreProc',        s:color_alpha,       s:color_bg,          'none')

" BINDINGS
call s:HL('Identifier',     s:color_beta,        s:color_bg,          'none')
call s:HL('Function',       s:color_beta,        s:color_bg,          'none')
call s:HL('Keyword',        s:color_beta,        s:color_bg,          'none')
call s:HL('Operator',       s:color_beta,        s:color_bg,          'none')

" TYPES
call s:HL('Type',           s:color_alpha,       s:color_bg,          'none')
call s:HL('Typedef',        s:color_alpha,       s:color_bg,          'none')
call s:HL('StorageClass',   s:color_alpha,       s:color_bg,          'none')
call s:HL('Structure',      s:color_alpha,       s:color_bg,          'none')

" FLOW CONTROL
call s:HL('Statement',      s:color_alpha,       s:color_bg,          'none')
call s:HL('Conditional',    s:color_alpha,       s:color_bg,          'none')
call s:HL('Repeat',         s:color_alpha,       s:color_bg,          'none')
call s:HL('Label',          s:color_alpha,       s:color_bg,          'none')
call s:HL('Exception',      s:color_alpha,       s:color_bg,          'none')

" MISC
call s:HL('Normal',         s:palette.gray13,    s:color_bg,          'none')
call s:HL('Cursor',         s:palette.black,     s:palette.puregreen, 'none')
call s:HL('Underlined',     s:palette.gray13,    s:color_bg,          'underline')
call s:HL('SpecialKey',     s:color_alpha,       s:color_bg,          'none')
call s:HL('NonText',        s:color_alpha,       s:color_bg,          'none')
call s:HL('Directory',      s:color_beta,        s:color_bg,          'none')

" FOLD
call s:HL('FoldColumn',     s:color_delta,       s:palette.black,     'none')
call s:HL('Folded',         s:color_delta,       s:palette.black,     'none')

" PARENTHESIS
call s:HL('MatchParen',     s:palette.white,     s:palette.red,      'none')

" POPUP
call s:HL('Pmenu',          s:palette.white,     s:palette.gray10,    'none')
call s:HL('PmenuSbar',      s:palette.black,     s:palette.dampgreen, 'none')
call s:HL('PmenuSel',       s:palette.black,     s:palette.dampgreen, 'none')
call s:HL('PmenuThumb',     s:palette.gray01,    s:palette.gray10,    'none')

" SPLITS
call s:HL('VertSplit',      s:palette.gray03,    s:palette.gray03,    'none')

" OTHERS
call s:HL('Debug',          s:color_alpha,       s:color_bg,          'none')
call s:HL('Delimiter',      s:color_alpha,       s:color_bg,          'none')
call s:HL('Question',       s:color_alpha,       s:color_bg,          'none')
call s:HL('Special',        s:color_alpha,       s:color_bg,          'none')
call s:HL('StatusLine',     s:color_alpha,       s:color_bg,          'none', 'gui_macvim')
call s:HL('StatusLineNC',   s:color_alpha,       s:color_bg,          'none', 'gui_macvim')
call s:HL('Tag',            s:color_alpha,       s:color_bg,          'none')
call s:HL('WildMenu',       s:color_alpha,       s:color_bg,          'none')

" DIFF
call s:HL('DiffAdd',        s:palette.green,     s:color_bg,          'none')
call s:HL('DiffChange',     s:palette.cyan,      s:color_bg,          'none')
call s:HL('DiffDelete',     s:palette.yellow,    s:color_bg,          'none')
call s:HL('DiffText',       s:palette.black,     s:palette.cyan,      'none')

call s:HL('diffRemoved',    s:palette.yellow,    s:color_bg,          'none')
call s:HL('diffAdded',      s:palette.green,     s:color_bg,          'none')


" Links ------------------------------------------------------------------ {{{1

" TODO


" Filetype Specific ------------------------------------------------------ {{{1

" TODO


" Plugin Specific -------------------------------------------------------- {{{1

" RAINBOW PARENTHESIS

let g:rbpt_colorpairs = [
			\ s:palette.brown,
			\ s:palette.gray11,
			\ s:palette.purple,
			\ s:palette.green,
			\ s:palette.white,
			\ s:palette.blue,
			\ s:palette.yellow,
			\ s:palette.red,
			\ s:palette.brown,
			\ s:palette.gray13,
			\ s:palette.cyan,
			\ s:palette.darkpurple,
			\ s:palette.dampgreen,
			\ s:palette.darkblue,
			\ s:palette.darkyellow,
			\ s:palette.darkred
			\ ]
