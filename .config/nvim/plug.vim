" Specify plugins installation directory
call plug#begin('~/.local/share/nvim/plugged')

" List of plugins to be used -- must use single-quotes
" Auto completion
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-tmux'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-jedi'

set completeopt=noinsert,menuone,noselect

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

filetype plugin indent on
call plug#end()

" Plugins configuration
let g:vim_markdown_folding_disabled = 1 " Disable folding in markdown
let g:NERDSpaceDelims = 1               " Enable spaces after comment delimiters
let g:NERDCommentEmptyLines = 1         " Enable commenting of empty lines

