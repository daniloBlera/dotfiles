" --PYTHON'S VIRTUALENV--
source $XDG_CONFIG_HOME/nvim/venv.vim

" --PLUGINS--
source $XDG_CONFIG_HOME/nvim/plug.vim

" --VISUAL--
set number                  " Set line numbering on
colorscheme koehler
set scrolloff=10            " Number of lines to keep above and below the cursor

" --INDENT AND SEARCH--
set tabstop=4               " Number of spaces a TAB counts for
set shiftwidth=4            " Number of spaces to use for each step of (auto)indent.
set softtabstop=4           " Number of spaces a TAB counts on edit operations
set expandtab               " Insert whitespaces when pressing TAB
set shiftround              " Round indent to a multiple of 'shiftwidth'
set autoindent              " Copy current indent level into the next line
set linebreak               " Wrap long lines
set ignorecase              " Ignore case on search patterns
filetype plugin indent on   " Rely on file plugins to handle indenting

" Insert mode completion
set completeopt=noinsert,menuone,noselect

" --KEYMAPS--
" Map (capital) 'J' and 'K' to 'Next' and 'Previous' tab
nnoremap J gt
nnoremap K gT

" Toggle directory tree
nmap <F2> :NERDTreeToggle<CR>

" Toggle line numbering
nnoremap <F3> :set nonumber!<CR>

" Text selection
" Map '\' and 'p' to paste X11's clipboard into selection
" Map '\' and 'y' to yank the character selection into X11's clipboard
noremap <Leader>p "+p
noremap <Leader>y "+y

" Map (capital) 'y' to copy the current line into selection then jump to the
" next. This is useful to send a sequence of lines to clipboard managers
noremap Y V"+yj

" Map '\' and 'n' to open 'new tab' prompt
noremap <Leader>n :tabnew 

" Map '\' and '/' or '-' for (saner) vertical and horizontal pane splitting.
noremap <Leader>/ :vsplit<CR>
noremap <Leader>- :split<CR>

" Map 'j' and 'k' to move to 'Next' and 'Previous' wrapped lines
nnoremap j gj
nnoremap k gk

" Line highlight
hi CursorLine cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white
nnoremap <F4> :set cursorline!<CR>

" Disable last search results hightlight
nnoremap <M-/> :noh<CR>

" Map 'Alt' and 'h|j|k|l' to navigate pane splits
nnoremap <M-h> <C-W><C-H>
nnoremap <M-j> <C-W><C-J>
nnoremap <M-k> <C-W><C-K>
nnoremap <M-l> <C-W><C-L>

" Displaying special characters
set listchars=eol:$,tab:<-,trail:~,extends:>,precedes:<
set list
nnoremap <F5> :set list!<CR>
