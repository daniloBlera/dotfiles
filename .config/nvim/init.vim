" --PYTHON'S VIRTUALENV--
source $XDG_CONFIG_HOME/nvim/venv.vim

" --PLUGINS--
source $XDG_CONFIG_HOME/nvim/plug.vim

" --COLOURSCHEME--
source $XDG_CONFIG_HOME/nvim/colourschemes/ruiner.vim

" --VISUAL--
set number                  " Set line numbering on
set relativenumber          " Set line numbering relative to the cursor
" colorscheme koehler
set scrolloff=10            " Number of lines to keep above and below the cursor
set showcmd                 " Show partial (<Leader> char) commands on the status line

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

" Insert mode completion
set completeopt=noinsert,menuone,noselect

" Disable the creation of ShaDa files
set shada="NONE"

" --KEYMAPS--
" Disable the cursor movement and map the spacebar as the Leader key
noremap <Space> <Nop>
let mapleader=" "

" Map (capital) 'J' and 'K' to 'Next' and 'Previous' tab
nnoremap J gt
nnoremap K gT

" Open the help menu in a separated tab
nnoremap <F1> :tab help<CR>

" Toggle directory tree
nnoremap <F2> :NERDTreeToggle<CR>

" Toggle line numbering
nnoremap <F3> :set nonumber!<CR>:set norelativenumber<CR>

" Text selection
" Map 'Leader' and 'p' to paste X11's clipboard
" Map 'Leader' and 'y' to yank the selected text into the clipboard
vnoremap <Leader>p "+p
vnoremap <Leader>y "+y

" Select current line's visible chars
nnoremap <Leader>v ^vg_

" Map (capital) 'y' to copy the current line into selection then jump to the
" next. This is useful to send a sequence of lines to clipboard managers
" nnoremap Y ^vg_ "+yj
nmap Y <Leader>v"+yj

" Map (capital) 'l' to copy the current line into selection.
nmap L <Leader>v"+y

" Map 'Leader' and 'n' to open 'new tab' prompt
nnoremap <Leader>n :tabnew 

" Map 'Leader' and '/' or '-' for vertical or horizontal pane splitting.
nnoremap <Leader>/ :vsplit<CR>
nnoremap <Leader>- :split<CR>

" Map 'j' and 'k' to move to 'Next' and 'Previous' wrapped lines
" nnoremap j gj
" nnoremap k gk

" Toggle line and column highlight
nnoremap <F4> :set cursorline!<CR>:set cursorcolumn!<CR>
set cursorline
set cursorcolumn

" Map '<Leader>' then 'h|j|k|l' to navigate pane splits
nnoremap <Leader>h <C-W><C-H>
nnoremap <Leader>j <C-W><C-J>
nnoremap <Leader>k <C-W><C-K>
nnoremap <Leader>l <C-W><C-L>

" Configuring list mode characters
" To use a basic set of characters, uncomment the line below
" set listchars=tab:<-,extends:>,precedes:<,nbsp:+,trail:~,eol:$

" To use more visual unicode characters, uncomment the next listchars command
" * tab:        U+00AB[U+2014]
" * extends:    U+203A
" * precedes:   U+2039
" * nbsp:       U+25CA
" * trail:      U+00B7
" * eol:        U+E33F  (UbuntuMono Nerd Font Mono)
set listchars=tab:«—,extends:›,precedes:‹,nbsp:◊,trail:·,eol:
set list

" Toggle list mode
nnoremap <F5> :set list!<CR>

" Toggle line wrap
nnoremap <F6> :set wrap!<CR>

" Toggle relative line numbering
nnoremap <F7> :set relativenumber!<CR>

" Colour configuration
" Enable TrueColor if the terminal supports it -- see:
"   :help term-dependent-settings
if $TERM =~ '^\(st\|screen\|tmux\)\(-.*\)\?$'
    set termguicolors
elseif $TERM == 'linux'
    " If we're using the linux tty, enable ascii listchars
    set listchars=tab:<-,extends:>,precedes:<,nbsp:+,trail:~,eol:$
endif

" Concealing Lambda expressions
autocmd VimEnter *.py syntax keyword Statement lambda conceal cchar=λ
autocmd VimEnter *.hs syntax match Statement '\vlambda +' conceal cchar=λ
autocmd VimEnter *.hs syntax match Statement '\\ *' conceal cchar=λ
autocmd VimEnter *.hs syntax match Statement ' \. ' conceal cchar=∘
autocmd VimEnter * hi! link Conceal Statement
autocmd VimEnter * set conceallevel=2

" Disable 'Ex Mode'
nnoremap Q <Nop>
nnoremap gQ <Nop>

" Movement keys
" Scroll one line up|down
" Ctrl + E|Y
"
" Scroll half a screen up|down
" Ctrl + U|D
"
" Center screen on cursor line
" zz
"
" Break long line into multiple shorter lines
" gq
"
" Move inside wrapped lines
" g j|k
"
" Open file under cursor line
" gf
"
" Join multiple (selected) lines
" J
" gJ
