" Specify plugins installation directory
call plug#begin('~/.local/share/nvim/plugged')

" List of plugins to be used -- must use single-quotes
" Operate on surroundings
Plug 'tpope/vim-surround'

" A tree explorer plugin
Plug 'preservim/nerdtree'

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

" VSCode-like extensions and language servers
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Shorten some statements with unicode symbols
Plug 'ehamberg/vim-cute-python'

" Better python syntax highlight
Plug 'numirias/semshi', { 'do': ':UpdateRemotePlugins' }

" Auto adjusting parens
Plug 'gpanders/nvim-parinfer'

" Colorize parenthesis for better visibility of nested structures
Plug 'luochen1990/rainbow'

" Tab and status lines
Plug 'vim-airline/vim-airline'

" Better normal mode motion
Plug 'easymotion/vim-easymotion'

" Show keybindings in pop-up
Plug 'liuchengxu/vim-which-key'

" Automatically insert matching pairs (quotes, parens, etc)
Plug 'jiangmiao/auto-pairs'

" lua << EOF
" require("nnn").setup()
" EOF

filetype plugin indent on
call plug#end()

" Plugins configuration
" let g:vim_markdown_folding_disabled = 1     " Disable folding in markdown
let g:NERDSpaceDelims = 1                   " Enable spaces after comment delimiters
let g:NERDCommentEmptyLines = 1             " Enable commenting of empty lines
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline_section_z = airline#section#create_right(['%l:%c'])
let g:rainbow_active = 1
