" Define the theme
let g:colors_name = "zenbones"

" Define the colors
if &background == "light"
    let g:zenbones_palette = {
        \ "bg": "#fbf1c7",
        \ "fg": "#3c3836",
        \ "rose": "#9d0006",
        \ "leaf": "#79740e",
        \ "wood": "#b57614",
        \ "water": "#076678",
        \ "blossom": "#8f3f71",
        \ "sky": "#427b58",
        \ }
else
    let g:zenbones_palette = {
        \ "bg": "#282828",
        \ "fg": "#ebdbb2",
        \ "rose": "#fb4934",
        \ "leaf": "#b8bb26",
        \ "wood": "#fabd2f",
        \ "water": "#83a598",
        \ "blossom": "#d3869b",
        \ "sky": "#83c07c",
        \ }
endif

" Set the colors
highlight Normal guibg=g:zenbones_palette.bg guifg=g:zenbones_palette.fg
highlight Comment guifg=g:zenbones_palette.leaf
highlight Constant guifg=g:zenbones_palette.wood
highlight String guifg=g:zenbones_palette.water
highlight Character guifg=g:zenbones_palette.water
highlight Number guifg=g:zenbones_palette.rose
highlight Boolean guifg=g:zenbones_palette.rose
highlight Float guifg=g:zenbones_palette.rose
highlight Identifier guifg=g:zenbones_palette.fg
highlight Function guifg=g:zenbones_palette.rose
highlight Variable guifg=g:zenbones_palette.fg
highlight Class guifg=g:zenbones_palette.rose
highlight Operator guifg=g:zenbones_palette.wood
highlight Keyword guifg=g:zenbones_palette.rose
highlight Exception guifg=g:zenbones_palette.rose
highlight Type guifg=g:zenbones_palette.wood
highlight Special guifg=g:zenbones_palette.water
highlight SpecialChar guifg=g:zenbones_palette.water
highlight Tag guifg=g:zenbones_palette.rose
highlight Underlined guifg=g:zenbones_palette.rose guioptions+=underline
highlight Bold guifg=g:zenbones_palette.rose gui=bold
highlight Italic guifg=g:zenbones_palette.rose gui=italic
highlight Error guifg=g:zenbones_palette.blossom guibg=g:zenbones_palette.bg
highlight Warning guifg=g:zenbones_palette.sky guibg=g:zenbones_palette.bg
highlight Info guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight MoreInfo guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight Hint guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight Debug guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight ErrorMsg guifg=g:zenbones_palette.blossom guibg=g:zenbones_palette.bg
highlight WarningMsg guifg=g:zenbones_palette.sky guibg=g:zenbones_palette.bg
highlight VertSplit guifg=g:zenbones_palette.sky guibg=g:zenbones_palette.bg
highlight Folded guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight Fold guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight SignColumn guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight LineNr guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight Cursor guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight CursorLine guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight CursorColumn guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight NormalFloat guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight FloatBorder guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight Pmenu guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight PmenuSel guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight PmenuSbar guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight PmenuThumb guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight TabLine guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight TabLineSel guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight TabLineFill guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight StatusLine guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight StatusLineNC guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight WinBar guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight WinBarNC guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight DiffAdd guifg=g:zenbones_palette.rose guibg=g:zenbones_palette.bg
highlight DiffChange guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight DiffDelete guifg=g:zenbones_palette.blossom guibg=g:zenbones_palette.bg
highlight DiffText guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight SpellBad guifg=g:zenbones_palette.blossom guibg=g:zenbones_palette.bg
highlight SpellCap guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight SpellLocal guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight SpellRare guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight IncSearch guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight Search guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight QuickFixLine guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight Directory guifg=g:zenbones_palette.rose guibg=g:zenbones_palette.bg
highlight Link guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight Title guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight Visual guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight VisualNOS guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight SpecialKey guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight NonText guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight Special guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight Statement guifg=g:zenbones_palette.rose guibg=g:zenbones_palette.bg
highlight Comment guifg=g:zenbones_palette.leaf guibg=g:zenbones_palette.bg
highlight Constant guifg=g:zenbones_palette.wood guibg=g:zenbones_palette.bg
highlight String guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight Character guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight Number guifg=g:zenbones_palette.rose guibg=g:zenbones_palette.bg
highlight Boolean guifg=g:zenbones_palette.rose guibg=g:zenbones_palette.bg
highlight Float guifg=g:zenbones_palette.rose guibg=g:zenbones_palette.bg
highlight Identifier guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight Function guifg=g:zenbones_palette.rose guibg=g:zenbones_palette.bg
highlight Variable guifg=g:zenbones_palette.fg guibg=g:zenbones_palette.bg
highlight Class guifg=g:zenbones_palette.rose guibg=g:zenbones_palette.bg
highlight Operator guifg=g:zenbones_palette.wood guibg=g:zenbones_palette.bg
highlight Keyword guifg=g:zenbones_palette.rose guibg=g:zenbones_palette.bg
highlight Exception guifg=g:zenbones_palette.rose guibg=g:zenbones_palette.bg
highlight Type guifg=g:zenbones_palette.wood guibg=g:zenbones_palette.bg
highlight Special guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight SpecialChar guifg=g:zenbones_palette.water guibg=g:zenbones_palette.bg
highlight Tag guifg=g:zenbones_palette.rose guibg=g:zenbones_palette.bg
highlight Underlined guifg=g:zenbones_palette.rose guioptions+=underline
highlight Bold guifg=g:zenbones_palette.rose gui=bold
highlight Italic guifg=g:zenbones_palette.rose gui=italic
