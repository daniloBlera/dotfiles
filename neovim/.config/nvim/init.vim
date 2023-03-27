" Neovim configuration file
" version: source build from v0.8.0-dev+568-g664efa497

" PYTHON'S ENV
" source $XDG_CONFIG_HOME/nvim/venv.vim

" PLUGINS
source $XDG_CONFIG_HOME/nvim/plug.vim

" COLOURSCHEME
source $XDG_CONFIG_HOME/nvim/colourschemes/ruiner.vim

" GENERAL OPTIONS
set shada="NONE"            " Disable the creation of ShaDa files
set mouse=a                 " Enable mouse support on all modes
set virtualedit=onemore     " Enable moving the cursor past the last char
set clipboard=unnamedplus   " Use system's clipboard

" VISUAL OPTIONS
set number                  " Set line numbering on
set scrolloff=10            " Min lines above/below the cursor
set sidescrolloff=5         " Min columns to the left/right of the cursor
set showcmd                 " Show partial commands on the status line
" set cursorline              " Hightlight the cursor's line
" set cursorcolumn            " Hightlight the cursor's column
set list                    " Display special chars

"" Enable TrueColor if the terminal supports it. For more
"" information, see the help for 'term-dependent-settings'
if $TERM =~ '^\(linux\|screen\)$'
    " If using the linux tty, enable ASCII listchars
    "" ASCII symbols
    set listchars=eol:$,tab:-->,multispace:---+,trail:-,extends:>,precedes:<,nbsp:§
elseif $TERM =~ '^\(st\|screen\|tmux\)-256color$'
    set termguicolors
    " List mode characters - use special chars to display tabs, nbsp's, trailing
    " whitespaces and more.
    set listchars=eol:¬,tab:+-,space:·,multispace:···+,leadmultispace:\│···,trail:•,extends:⟩,precedes:⟨,nbsp:◊
endif

"" Filetype specific settings
" autocmd Filetype python syntax keyword Statement lambda conceal cchar=λ

autocmd Filetype haskell syntax match Statement '\vlambda +' conceal cchar=λ
autocmd Filetype haskell syntax match Statement '\\ *' conceal cchar=λ
autocmd Filetype haskell syntax match Statement ' \. ' conceal cchar=∘

autocmd Filetype lisp syntax keyword lispFunc lambda conceal cchar=λ
autocmd Filetype lisp highlight Conceal cterm=NONE ctermbg=NONE ctermfg=yellow

autocmd Filetype * highlight! link Conceal Statement
autocmd Filetype * set conceallevel=2

autocmd Filetype markdown set textwidth=60
autocmd Filetype lisp set formatoptions=qjr textwidth=60
autocmd Filetype lisp setlocal tabstop=2 shiftwidth=2 softtabstop=2

"" Disable automatic text wrapping
autocmd Filetype * set formatoptions-=t
autocmd Filetype markdown set formatoptions+=t

set wildcharm=<C-Z>
cnoremap <expr> <up> wildmenumode() ? "\<left>" : "\<up>"
cnoremap <expr> <down> wildmenumode() ? "\<right>" : "\<down>"
cnoremap <expr> <left> wildmenumode() ? "\<up>" : "\<left>"
cnoremap <expr> <right> wildmenumode() ? " \<bs>\<C-Z>" : "\<right>"

" --INDENT AND SEARCH--
set tabstop=4               " Number of spaces a TAB counts for
set shiftwidth=4            " Number of spaces to use for each step of (auto)indent.
set softtabstop=4           " Number of spaces a TAB counts on edit operations
set expandtab               " Insert whitespaces when pressing TAB
set shiftround              " Round indent to a multiple of 'shiftwidth'
set autoindent              " Copy current indent level into the next line
set linebreak               " Wrap long lines
set ignorecase              " Ignore case on search patterns
set splitright              " Put new split to the right
set splitbelow              " Put new split below
filetype plugin indent on   " Rely on file plugins to handle indenting


" KEYMAPS -- global
"" Map the spacebar as the Leader key
noremap <Space> <Nop>
let mapleader=" "

"" Unbind F keys
nnoremap <F1> <nop>
inoremap <F1> <nop>
inoremap <F2> <nop>
inoremap <F3> <nop>
inoremap <F4> <nop>
inoremap <F5> <nop>
inoremap <F6> <nop>
inoremap <F7> <nop>
inoremap <F8> <nop>
inoremap <F9> <nop>
inoremap <F10> <nop>
inoremap <F11> <nop>
inoremap <F12> <nop>

" Disable cursor moving one char left after exiting INSERT mode.
" Source
"   https://stackoverflow.com/a/17975729
"   https://vim.fandom.com/wiki/Prevent_escape_from_moving_the_cursor_one_character_to_the_left
inoremap <silent> <Esc> <C-O>:stopinsert<CR>

" Disable 'Ex Mode'
nnoremap Q <Nop>
nnoremap gQ <Nop>

" Mouse support
nnoremap <Leader>m :set mouse=a<CR>
nnoremap <Leader>M :set mouse=""<CR>

"" Paste system's clipboard
vnoremap <Leader>p "+p

"" Yank selected text into the system clipboard
vnoremap <Leader>y "+y

"" Select current line's visible chars (non trailing whitespaces or newline)
nnoremap <Leader>v ^vg_

"" Copy the current line into the system clipboard.
nnoremap L ^vg_"+y

"" Copy the current line into the system clipboard and jump to the next line
nnoremap Y ^vg_"+yj

" KEYMAPS -- Window, buffer, and tab-related.
"" Go to next and previous tabs
nnoremap <Leader>tn :tabnext<CR>
nnoremap <Leader>tp :tabprevious<CR>

"" Next, previous and list buffers
nnoremap <Leader>bn :bnext<CR>
nnoremap <Leader>bp :bprev<CR>
nnoremap <Leader>bi :buffers<CR>:buffer<Space>

"" Close current buffer, tab and window
nnoremap <Leader>bd :bdelete<CR>
nnoremap <Leader>td :tabclose<CR>
nnoremap <Leader>wd :close<CR>

"" Toggle directory tree
nnoremap <Leader>op :NERDTreeToggle<CR>

"" Map vertical and horizontal pane splitting.
nnoremap <Leader>w/ :vsplit<Space>
nnoremap <Leader>w- :split<Space>

"" Window navigation
nnoremap <Leader>wh <C-W><C-H>
nnoremap <Leader>wj <C-W><C-J>
nnoremap <Leader>wk <C-W><C-K>
nnoremap <Leader>wl <C-W><C-L>

"" Center screen after commands
nnoremap G Gzz
nnoremap n nzz
nnoremap N Nzz

"" Center window with keypad arrow navigation
nnoremap <kLeft> <Left>zz
nnoremap <kDown> <Down>zz
nnoremap <kRight> <Right>zz
nnoremap <kUp> <Up>zz

inoremap <kLeft> <Left><C-o>zz
inoremap <kDown> <Down><C-o>zz
inoremap <kRight> <Right><C-o>zz
inoremap <kUp> <Up><C-o>zz

" KEYMAPS -- Visual stuff
"" Toggle list mode
nnoremap <Leader>vl :set list!<CR>

"" Toggle line wrapping
nnoremap <Leader>vw :set wrap!<CR>

"" Toggle line numbering
nnoremap <Leader>vn :set number!<CR>

"" Toggle relative line numbering
nnoremap <Leader>vr :set relativenumber!<CR>

"" Toggle line and column highlight
nnoremap <Leader>vh :set cursorline!<CR>:set cursorcolumn!<CR>

" Vim-Sneak related
" 2-character Sneak (default)
nmap <Leader>s <Plug>Sneak_s
nmap <Leader><S-s> <Plug>Sneak_S
" visual-mode
xmap <Leader>s <Plug>Sneak_s
xmap <Leader><S-s> <Plug>Sneak_S
" operator-pending-mode
omap <Leader>s <Plug>Sneak_s
omap <Leader><S-s> <Plug>Sneak_S

" repeat motion
" map ? <Plug>Sneak_;
" map ? <Plug>Sneak_,

" 1-character enhanced 'f'
" nmap ? <Plug>Sneak_f
" nmap ? <Plug>Sneak_F
" visual-mode
" xmap ? <Plug>Sneak_f
" xmap ? <Plug>Sneak_F
" operator-pending-mode
" omap ? <Plug>Sneak_f
" omap ? <Plug>Sneak_F

" 1-character enhanced 't'
" nmap ? <Plug>Sneak_t
" nmap ? <Plug>Sneak_T
" visual-mode
" xmap ? <Plug>Sneak_t
" xmap ? <Plug>Sneak_T
" operator-pending-mode
" omap ? <Plug>Sneak_t
" omap ? <Plug>Sneak_T

" label-mode
" nmap ? <Plug>SneakLabel_s
" nmap ? <Plug>SneakLabel_S

" CoC
" Use tab for trigger completion with characters ahead and navigate.
" NOTE: There's always complete item selected by default, you may want to enable
" no select by `"suggest.noselect": true` in your configuration file.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
" inoremap <silent><expr> <TAB>
"       \ coc#pum#visible() ? coc#pum#next(1) :
"       \ CheckBackspace() ? "\<Tab>" :
"       \ coc#refresh()
" inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice.
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" KEYMAPS -- Misc
"" Insert shebangs
" nnoremap sbp ggI#!/usr/bin/env python3<CR># -*- coding: utf-8 -*-<CR><Esc>
" nnoremap sbd ggI#!/bin/sh<CR><Esc>
" nnoremap sbb ggI#!/bin/bash<CR><Esc>
" nnoremap sbz ggI#!/bin/zsh<CR><Esc>
