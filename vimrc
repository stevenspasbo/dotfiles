"""""""""""""""""""""""""""""""""""""""""""
"           Author: Steven Spasbo         "
"""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""
" Misc vim settings
"""""""""""""""""""""""""""""""""""""""""""
set nocompatible " Uses vim settings instead of vi settings
filetype on
filetype plugin on
set number " Enables line numbers
set mouse=a " Enable mouse
set ignorecase " Ignores case while searching
set smartcase " Unless search contains uppercase
set background=dark " For dark terminals
set ruler " Show position
set showmode

execute pathogen#infect()

"""""""""""""""""""""""""""""""""""""""""""
" Text, tabs, indentds
"""""""""""""""""""""""""""""""""""""""""""
filetype indent on " Smart indents
set wrap
set autoindent
set smarttab
set tabstop=2
set shiftwidth=2
set expandtab

"""""""""""""""""""""""""""""""""""""""""""
" Files, undos
"""""""""""""""""""""""""""""""""""""""""""
set noswapfile " Disables swap files
set undolevels=1000 " Updo 1000 changes
set updatecount=100 " Update swapfile after 100 character changes

"""""""""""""""""""""""""""""""""""""""""""
" Color settings
"""""""""""""""""""""""""""""""""""""""""""
syntax enable " Syntax highlighting
colorscheme molokai
let g:airline_powerline_fonts = 1
let g:airline_theme='light'

"""""""""""""""""""""""""""""""""""""""""""
" Status line
"""""""""""""""""""""""""""""""""""""""""""
set laststatus=2
"set statusline=%F " Full path to file
"set statusline+=%m " Flag if modified
"set statusline+=%h " Help file flag
"set statusline+=%r " Flag [RO] if only read-only
"set statusline+=%y " File type
"set statusline+=%= " Separator
"set statusline+=%c,
"set statusline+=%l/%L " Cursor location
"set statusline+=\ %P " Percent through file

"""""""""""""""""""""""""""""""""""""""""""
" Mappings
"""""""""""""""""""""""""""""""""""""""""""

map <c-x> VdO
