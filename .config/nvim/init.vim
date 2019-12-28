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
" Map (capital) 'J' and 'K' to 'Next' and 'Previous' tab, similar to qutebrowser's tab selection
nnoremap J gt
nnoremap K gT

" Toggle directory tree
nmap <F2> :NERDTreeToggle<CR>

" Toggle line numbering
nnoremap <F3> :set nonumber!<CR>

" Text selection
" Map '\' and 'p' to paste X11's clipboard into selection
" Map '\' and 'y' to yank selection into X11's clipboard
noremap <Leader>p "+p
noremap <Leader>y "+y

" Map '\' and 'n' to open 'new tab' prompt
noremap <Leader>n :tabnew 

" Map 'j' and 'k' to move to 'Next' and 'Previous' wrapped lines
nnoremap j gj
nnoremap k gk

" Map '\' and 'P' to insert python hashbang and file encoding
nnoremap <Leader>P ggI#!/usr/bin/env python3<CR># -*- coding: utf-8 -*-<CR><ESC>

" Line highlight
hi CursorLine cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white
set cursorline
nnoremap <F4> :set cursorline!<CR>

" Pane navigation
nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
