" Specify python's virtualenv directory
source $HOME/.config/nvim/venv.vim

" Plugins
source $HOME/.config/nvim/plug.vim  " Plugins configuration file

" Visuals
set number                  " Turn on line numbering
" colorscheme koehler         " Set theme
set scrolloff=9             " Minimum lines above and below the cursor

" Indent and search
set tabstop=4               " Maximum width of a TAB in columns
set shiftwidth=4            " The size of an indent in spaces
" set softtabstop=4           " Number of spaces a TAB counts on edit operations
set expandtab               " Insert spaces when pressing the TAB key
set shiftround              " Round indent to multiple of 'shiftwidth'
set autoindent
set linebreak
set ignorecase              " Does case insensitive when all lowercase, sensitive otherwise
filetype plugin indent on   " Rely on file plugins to handle indenting

autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect

" Map ',' and '.' keys for 'previous' and 'next' tab navigation
:map . gt
:map , gT

" Toggle directory tree
:map <F2> :NERDTreeToggle<CR>

" Disable cursor style override
:set guicursor=
autocmd InsertEnter * set cursorline    " Enable cursor line on Insert mode entry
autocmd InsertLeave * set nocursorline  " Disable cursor line on Insert mode exit

