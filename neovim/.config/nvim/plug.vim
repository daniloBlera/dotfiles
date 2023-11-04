" Specify plugins installation directory
call plug#begin('~/.local/share/nvim/plugged')

" List of plugins to be used -- must use single-quotes
" Operate on surroundings
Plug 'tpope/vim-surround'

" Syntax, indent, and filetype for git stuff
Plug 'tpope/vim-git', {'for': 'git'}

" Bracket shortcuts mapping -- try 'help: unimpaired' for the maps
Plug 'tpope/vim-unimpaired'

" A set of default settings
Plug 'tpope/vim-sensible'

" Readline keybindings for movement in insert mode
Plug 'tpope/vim-rsi'

" A tree explorer plugin
Plug 'preservim/nerdtree'

" Block commenting
Plug 'scrooloose/nerdcommenter'

" Enhanced C/Bison/Flex syntax highlight
Plug 'justinmk/vim-syntax-extra'

" VSCode-like extensions and language servers
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Syntax parsing for many languages
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

" Colorize parenthesis for better visibility of nested structures
Plug 'luochen1990/rainbow'

" Fancy indent guides
Plug 'lukas-reineke/indent-blankline.nvim'

" Tab and status lines
Plug 'itchyny/lightline.vim'

" Extra normal mode motion
Plug 'easymotion/vim-easymotion'

" Normal mode motions for erlang, in the spirit of lispy for emacs
Plug 'edkolev/erlang-motions.vim'

" Show keybindings in pop-up
Plug 'liuchengxu/vim-which-key'

" Zen mode
Plug 'folke/zen-mode.nvim'

" lua << EOF
" require("nnn").setup()
" EOF

filetype plugin indent on
call plug#end()

" Plugins configuration
let g:NERDSpaceDelims = 1                   " Enable spaces after comment delimiters
let g:NERDCommentEmptyLines = 1             " Enable commenting of empty lines
let g:rainbow_active = 1
