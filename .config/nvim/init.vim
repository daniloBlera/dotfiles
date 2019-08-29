" --Python's virtualenv configuration--
source $HOME/.config/nvim/venv.vim

" --Plugins--
source $HOME/.config/nvim/plug.vim  " Plugins configuration file

" --Visuals--
set number                  " Set line numbering on
" colorscheme koehler
set scrolloff=9             " Number of lines to keep above and below the cursor

" --Indent and search--
set tabstop=4               " Maximum width of a TAB in columns
set shiftwidth=4            " The size of an indent in spaces
" set softtabstop=4           " Number of spaces a TAB counts on edit operations
set expandtab               " Insert whitespaces when pressing TAB
set shiftround              " Round indent to a multiple of 'shiftwidth'
set autoindent
set linebreak
set ignorecase              " Case insensitive search on all lowercase regex, case sensitive otherwise
filetype plugin indent on   " Rely on file plugins to handle indenting

" autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect

" --Normal mode maps--
" Map 'J' and 'K' to 'Next' and 'Previous' tab (similar to qutebrowser's)
nmap J gt
nmap K gT

" Toggle directory tree
nmap <F2> :NERDTreeToggle<CR>

" Toggle line numbering
nnoremap <F3> :set nonumber!<CR>

