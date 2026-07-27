-- Neovim's configuration file
-- written in lua

-- GENERAL OPTIONS
-- state and history
vim.o.undofile = false            -- do not create undo files
vim.o.shadafile = 'NONE'          -- disable reading or writing shada files

-- tabs and indentation
vim.o.tabstop = 8                 -- number of spaces a TAB counts for
vim.o.shiftwidth = 4              -- number of spaces to use for each step of (auto)indent.
vim.o.softtabstop = 4             -- number of spaces a TAB counts on edit operations
vim.o.expandtab = true            -- pressing tab inserts spaces
vim.o.shiftround = true           -- round indent to a multiple of 'shiftwidth'
vim.o.autoindent = true           -- copy the indent of the current line

-- line breaks and wrapping
vim.o.linebreak = true            -- wrap lines at breakat instead of exactly max column
vim.o.wrap = true                 -- enable visual line wrapping
vim.o.textwidth = 90              -- for "ninety-ish" columns hard wrapping
vim.o.colorcolumn = '90'          -- set an indicator at the wrapping column
vim.opt.formatoptions = {         -- disable automatic line breaking on
  t = false,                      -- text
  c = false,                      -- comments
}

-- search
vim.o.ignorecase = true           -- enable case-insensitive search
vim.o.smartcase = true            -- ... unless the pattern includes uppercase characters

-- visual and behaviour
vim.o.virtualedit = 'onemore'     -- allow the cursor to move past the end of the line
vim.o.clipboard = 'unnamedplus'   -- use system's clipboard for all selection operations
vim.o.splitbelow = true           -- new window splits will be placed below
vim.o.number = true               -- enable line numbering
vim.o.scrolloff = 10              -- minimum lines above and below the cursor
vim.o.sidescrolloff = 5           -- minimum columns to the left and right of the cursor
vim.o.showcmd = true              -- show partial commands on the status line
vim.o.list = true                 -- display special chars (spaces, tabs, newlines, etc)
vim.o.wrapscan = false            -- disable wrap around search
vim.o.cursorline = true           -- highlight the cursor's line
vim.o.cursorcolumn = false        -- highlight the cursor's column
vim.o.foldenable = false          -- folds start open
vim.o.foldmethod = 'marker'       -- enable folding regions between {{{ and }}} markers
vim.o.winborder = 'single'        -- single line border around floating windows 
vim.opt.whichwrap = 'b,s,<,>,[,]' -- movements allowed to jump to next/previous line

-- simple check for "fancy" (truecolor) terminals
local function usingFancyTerm()
  local term = os.getenv('TERM')
  return term and (
    term:match('256color')
    or term:match('st') 
    or term:match('alacritty')
    or term:match('foot')
    or term:match('truecolor') 
    or term:match('xterm') 
  )
end

-- setting colors and list mode chars based on truecolor and unicode support
if usingFancyTerm() then
  -- fancier chars and colors
  vim.o.termguicolors = true
  vim.cmd('set t_Co=256')
  vim.o.showbreak = '↳ '

  vim.opt.listchars = {
    eol = '¬',
    tab = '‹-',
    space = '•',
    multispace = '•••+',
    leadmultispace = '│•••',
    trail = '•',
    extends = '⟩',
    precedes = '⟨',
    nbsp = '^',
  }
else
  -- simpler chars and colours
  vim.o.termguicolors = false
  vim.cmd('set t_Co=8')
  vim.o.showbreak ='> '

  vim.opt.listchars = {
    eol = '$',
    tab = '-->',
    space = '.',
    multispace = '---+',
    leadmultispace = '|---',
    trail = '-',
    extends = '>',
    precedes = '<',
    nbsp = '^',
  }
end

-- KEYBINDINGS
-- use `<space>` as leader
vim.g.mapleader = ' '

-- disable moving to the next char when pressing `space`
vim.keymap.set('n', '<space>', '<nop>')

-- unbinding function keys
vim.keymap.set('n', '<F1>', '<nop>')
vim.keymap.set('i', '<F1>', '<nop>')
vim.keymap.set('i', '<F2>', '<nop>')
vim.keymap.set('i', '<F3>', '<nop>')
vim.keymap.set('i', '<F4>', '<nop>')
vim.keymap.set('i', '<F5>', '<nop>')
vim.keymap.set('i', '<F6>', '<nop>')
vim.keymap.set('i', '<F7>', '<nop>')
vim.keymap.set('i', '<F8>', '<nop>')
vim.keymap.set('i', '<F9>', '<nop>')
vim.keymap.set('i', '<F10>', '<nop>')
vim.keymap.set('i', '<F11>', '<nop>')
vim.keymap.set('i', '<F12>', '<nop>')

-- stop the cursor moving back one character when exiting insert mode
-- vim.keymap.set('i', '<esc>', '<C-O>:stopinsert<cr>')

vim.keymap.set('n', '<esc>', '<cmd>nohlsearch<cr>', { desc = 'Clear search highlight' })
vim.keymap.set('n', '<leader>l', 'vg_', { desc = 'Select from cursor to the end of the line' }) 
vim.keymap.set('n', '<leader>L', '^vg_', { desc = 'Select whole visible line' })
vim.keymap.set('n', 'yY', '^vg_y', { desc = 'Copy visible line' })

-- tab commands
vim.keymap.set('n', '<leader>tn', '<cmd>tabnext<cr>', { desc = 'Next tab' })
vim.keymap.set('n', '<leader>tp', '<cmd>tabprevious<cr>', { desc = 'Previous tab' })
vim.keymap.set('n', '<leader>tc', '<cmd>tabnew<cr>', { desc = 'Create new tab' })
vim.keymap.set('n', '<leader>td', '<cmd>tabclose<cr>', { desc = 'Close current tab' })

-- buffer commands
vim.keymap.set('n', '<leader>bn', '<cmd>bnext<cr>', { desc = 'Open next buffer' })
vim.keymap.set('n', '<leader>bp', '<cmd>bprev<cr>', { desc = 'Open previous buffer' })
vim.keymap.set('n', '<leader>bi', '<cmd>buffers<cr>:buffer<space>', { desc = 'Select buffer' })
vim.keymap.set('n', '<leader>bd', '<cmd>bdelete<cr>', { desc = 'Close current buffer' })
vim.keymap.set('n', '<leader>bD', '<cmd>bdelete!<cr>', { desc = 'Close current buffer (force)' })
vim.keymap.set('n', '<leader>bs', '<cmd>w<cr>', { desc = 'Save buffer contents' })
vim.keymap.set('n', '<leader>bx', '<cmd>x<cr>', { desc = 'Save buffer contents and close it' })

-- window/split commands
vim.keymap.set('n', '<c-left>', '<cmd>wincmd h<cr>')
vim.keymap.set('n', '<c-down>', '<cmd>wincmd j<cr>')
vim.keymap.set('n', '<c-up>', '<cmd>wincmd k<cr>')
vim.keymap.set('n', '<c-right>', '<cmd>wincmd l<cr>')
vim.keymap.set('n', '<leader>w<left>', '<cmd>wincmd h<cr>', { desc = 'Move to left split' })
vim.keymap.set('n', '<leader>w<down>', '<cmd>wincmd j<cr>', { desc = 'Move to split below' })
vim.keymap.set('n', '<leader>w<up>', '<cmd>wincmd k<cr>', { desc = 'Move to split above' })
vim.keymap.set('n', '<leader>w<right>', '<cmd>wincmd l<cr>', { desc = 'Move to right split' })
vim.keymap.set('n', '<leader>w/', ':vsplit<space>', { desc = 'New split to the right' })
vim.keymap.set('n', '<leader>w-', ':split<space>', { desc = 'New split below' })
vim.keymap.set('n', '<leader>wd', '<cmd>close<cr>', { desc = 'Close window' })
vim.keymap.set('n', '<leader>wD', '<cmd>close!<cr>', { desc = 'Close window (force)' })
vim.keymap.set('n', '<leader>wq', '<cmd>qa<cr>', { desc = 'Quit' })
vim.keymap.set('n', '<leader>wQ', '<cmd>qa!<cr>', { desc = 'Quit (force)' })

-- center the screen on the cursor after a jump
vim.keymap.set('n', 'G', 'Gzz')
vim.keymap.set('n', 'n', 'nzz')
vim.keymap.set('n', 'N', 'Nzz')

-- cursor moves through soft-wrapped lines
vim.keymap.set('n', 'j', 'gj')
vim.keymap.set('n', 'k', 'gk')
vim.keymap.set('n', '<down>', 'g<down>')
vim.keymap.set('n', '<up>', 'g<up>')

-- toggle visuals
vim.keymap.set('n', '<leader>vl', '<cmd>set list!<cr>', { desc = 'Toggle list mode' })
vim.keymap.set('n', '<leader>vw', '<cmd>set wrap!<cr>', { desc = 'Toggle soft line wrap' })
vim.keymap.set('n', '<leader>vh', '<cmd>set cursorline!<cr>', { desc = 'Toggle cursor line highlight' })

-- FILETYPE AND LANGUAGE SETTINGS
-- configure tab width in spaces
vim.api.nvim_create_autocmd('FileType', {
  pattern = { 'lua', 'markdown', 'org', 'haskell', 'xml', 'yaml' },
  callback = function()
    vim.o.tabstop = 2
    vim.o.softtabstop = 2
    vim.o.shiftwidth = 2
  end,
})

-- this should prevent the ~2sec delay when opening a python source file
if vim.fn.executable('python3') > 0 then
  vim.g.python3_host_prog = vim.fn.system('which python')
end
