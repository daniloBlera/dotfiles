" Specify plugins installation directory
call plug#begin('~/.local/share/nvim/plugged')

" List of plugins to be used -- must use single-quotes
Plug 'tpope/vim-surround'

" Completion framework
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Python autocomplete
Plug 'deoplete-plugins/deoplete-jedi'

" Tmux adjacent pane words
Plug 'wellle/tmux-complete.vim'

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

" Python development
Plug 'python-mode/python-mode', { 'for': 'python', 'branch': 'develop' }

filetype plugin indent on
call plug#end()

" Plugins configuration
" let g:vim_markdown_folding_disabled = 1     " Disable folding in markdown
let g:NERDSpaceDelims = 1                   " Enable spaces after comment delimiters
let g:NERDCommentEmptyLines = 1             " Enable commenting of empty lines
let g:deoplete#enable_at_startup = 1        " Enable completion automatically
let g:pymode_indent = 1
" let g:pymode = 0                            " Disable pymode
let g:pymode_doc = 0
