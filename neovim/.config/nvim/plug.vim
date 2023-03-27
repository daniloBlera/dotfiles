" Specify plugins installation directory
call plug#begin('~/.local/share/nvim/plugged')

" List of plugins to be used -- must use single-quotes
Plug 'tpope/vim-surround'

" Nord theme
" Plug 'arcticicestudio/nord-vim'

" Tree navigation
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}

" Syntax
Plug 'tpope/vim-git', {'for': 'git'}

" Bracket shortcuts mapping -- try 'help: unimpaired' for the maps
Plug 'tpope/vim-unimpaired'

" A set of default settings
Plug 'tpope/vim-sensible'

" Block commenting
Plug 'scrooloose/nerdcommenter'

" Enhanced C/Bison/Flex syntax highlight
Plug 'justinmk/vim-syntax-extra'

" Conceal some statements
Plug 'ehamberg/vim-cute-python'

" Python development
" Plug 'python-mode/python-mode', { 'for': 'python', 'branch': 'develop' }

" CoC
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Better python syntax highlight
Plug 'numirias/semshi', { 'do': ':UpdateRemotePlugins' }

" Tab and status lines
Plug 'vim-airline/vim-airline'

" Minimalist motion
" Plug 'justinmk/vim-sneak'

" More complete normal mode motion
" Plug 'easymotion/vim-easymotion'

" Plug 'luukvbaal/nnn.nvim'
" call plug#end()

" lua << EOF
" require("nnn").setup()
" EOF

filetype plugin indent on
call plug#end()

" Plugins configuration
" let g:vim_markdown_folding_disabled = 1     " Disable folding in markdown
let g:NERDSpaceDelims = 1                   " Enable spaces after comment delimiters
let g:NERDCommentEmptyLines = 1             " Enable commenting of empty lines
" let g:deoplete#enable_at_startup = 1        " Enable completion automatically
" let g:pymode_indent = 1
" let g:pymode = 0                            " Disable pymode
" let g:pymode_doc = 0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline_section_z = airline#section#create_right(['%l:%c'])
