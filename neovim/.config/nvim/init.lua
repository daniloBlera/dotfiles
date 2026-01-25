-- Neovim's configuration file
-- written in lua!!1!one! :O

-- PLUGIN MANAGER
require("config.lazy")

-- GENERAL OPTIONS
vim.opt.shadafile = "NONE"          -- disable reading or writing shada files
vim.opt.virtualedit = 'onemore'     -- allow the cursor to move past the end of the line
vim.opt.clipboard = 'unnamedplus'   -- use system's clipboard for all selection operations
vim.opt.tabstop = 8                 -- number of spaces a TAB counts for
vim.opt.shiftwidth = 4              -- number of spaces to use for each step of (auto)indent.
vim.opt.softtabstop = 4             -- number of spaces a TAB counts on edit operations
vim.opt.expandtab = true            -- pressing tab inserts spaces
vim.opt.shiftround = true           -- round indent to a multiple of 'shiftwidth'
vim.opt.autoindent = true           -- copy the indent of the current line
vim.opt.linebreak = true            -- enable visual line wrapping
vim.opt.textwidth = 90              -- for "ninety-ish" hard wrapping
vim.opt.ignorecase = true           -- case-insensitive search...
vim.opt.smartcase = true            -- ... unless the search pattern has uppercase characters
vim.opt.splitbelow = true           -- new window splits will be placed below
vim.opt.number = true               -- enable line numbering
vim.opt.scrolloff = 10              -- minimum lines above and below the cursor
vim.opt.sidescrolloff = 5           -- minimum columns to the left and right of the cursor
vim.opt.showcmd = true              -- show partial commands on the status line
vim.opt.list = true                 -- display special chars (spaces, tabs, newlines, etc)
vim.opt.wrapscan = false            -- disable wrap around search
vim.opt.cursorline = true           -- highlight the cursor's line
vim.opt.cursorcolumn = true         -- highlight the cursor's column

-- disable auto-wrapping of text and comments
vim.opt.formatoptions = { t = false, c = false, }

-- our check for terminal support for 256 colors and unicode glyps
local function supports_truecolor()
  local term = os.getenv("TERM")
  return term and (
    term:match("256color")
    or term:match("st") 
    or term:match("alacritty")
    or term:match("foot")
    or term:match("truecolor") 
    or term:match("xterm") 
  )
end

-- setting colors and list mode chars based on truecolor and unicode support
if supports_truecolor() then
  -- enable truecolor
  vim.opt.termguicolors = true
  vim.cmd("set t_Co=256")

  -- indicator that the line is visually wrapped
  vim.opt.showbreak = '↳ '

  -- prettier listchars
  vim.opt.listchars = {
    eol = '¬',
    tab = '-->',
    space = '•',
    multispace = '•••+',
    leadmultispace = '│•••',
    trail = '•',
    extends = '⟩',
    precedes = '⟨',
    nbsp = '◊',
  }
else
  -- use a simpler colourscheme
  vim.opt.termguicolors = false
  vim.cmd("set t_Co=8")
  vim.cmd("colorscheme koehler")

  -- indicator that the line is visually wrapped
  vim.opt.showbreak ='> '

  -- ASCII listchars
  vim.opt.listchars = {
    eol = '$',
    tab = '-->',
    space = '.',
    multispace = '---+',
    leadmultispace = '|---',
    trail = '-',
    extends = '>',
    precedes = '<',
    nbsp = '¤',
  }
end

-- KEYBINDINGS
-- use `<space>` as leader
vim.g.mapleader = ' '

-- disable moving to the next char when pressing `space`
vim.keymap.set('n', '<space>', '<nop>')

-- unbinding F-keys
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

-- stop the cursor moving one character left when exiting insert mode
vim.keymap.set('i', '<esc>', '<C-O>:stopinsert<cr>')

-- clear search highlight on Esc
vim.keymap.set('n', '<esc>', '<cmd>nohlsearch<cr>')

-- select the current line
vim.keymap.set('n', '<leader>l', 'vg_')
vim.keymap.set('n', '<leader>L', '^vg_')

-- copy the current line (minus newline)
vim.keymap.set('n', 'yY', '^vg_y')

-- navigate tabs
vim.keymap.set('n', '<leader>tn', '<cmd>tabnext<cr>')
vim.keymap.set('n', '<leader>tp', '<cmd>tabprevious<cr>')

-- navigate buffers
vim.keymap.set('n', '<leader>bn', '<cmd>bnext<cr>')
vim.keymap.set('n', '<leader>bp', '<cmd>bprev<cr>')
vim.keymap.set('n', '<leader>bi', '<cmd>buffers<cr>:buffer<space>')

-- navigate windows
vim.keymap.set('n', '<c-left>', '<cmd>wincmd h<cr>')
vim.keymap.set('n', '<c-down>', '<cmd>wincmd j<cr>')
vim.keymap.set('n', '<c-up>', '<cmd>wincmd k<cr>')
vim.keymap.set('n', '<c-right>', '<cmd>wincmd l<cr>')
vim.keymap.set('n', '<leader>w<left>', '<cmd>wincmd h<cr>')
vim.keymap.set('n', '<leader>w<down>', '<cmd>wincmd j<cr>')
vim.keymap.set('n', '<leader>w<up>', '<cmd>wincmd k<cr>')
vim.keymap.set('n', '<leader>w<right>', '<cmd>wincmd l<cr>')

-- split current window (open new ones to the right and below, respectively)
vim.keymap.set('n', '<leader>w/', ':vsplit<space>')
vim.keymap.set('n', '<leader>w-', ':split<space>')

-- close current buffer, tab, and window
vim.keymap.set('n', '<leader>bd', '<cmd>bdelete<cr>')
vim.keymap.set('n', '<leader>bD', '<cmd>bdelete!<cr>')
vim.keymap.set('n', '<leader>td', '<cmd>tabclose<cr>')
vim.keymap.set('n', '<leader>wd', '<cmd>close<cr>')
vim.keymap.set('n', '<leader>wD', '<cmd>close!<cr>')
vim.keymap.set('n', '<leader>wq', '<cmd>qa<cr>')
vim.keymap.set('n', '<leader>wQ', '<cmd>qa!<cr>')
vim.keymap.set('n', '<leader>bs', '<cmd>w<cr>')
vim.keymap.set('n', '<leader>bx', '<cmd>x<cr>')

-- center the screen on the cursor's line after a jump
vim.keymap.set('n', 'G', 'Gzz')
vim.keymap.set('n', 'n', 'nzz')
vim.keymap.set('n', 'N', 'Nzz')

-- enable moving cursor through soft-wrapped lines
vim.keymap.set('n', 'j', 'gj')
vim.keymap.set('n', 'k', 'gk')
vim.keymap.set('n', '<down>', 'g<down>')
vim.keymap.set('n', '<up>', 'g<up>')

-- toggle list mode and soft line wrapping
vim.keymap.set('n', '<leader>vl', '<cmd>set list!<cr>')
vim.keymap.set('n', '<leader>vw', '<cmd>set wrap!<cr>')

-- line highlight
vim.keymap.set('n', '<leader>vh', '<cmd>set cursorline!<cr>')

-- FILETYPE-SPECIFIC SETTINGS
-- indentation
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "lua", "markdown", "haskell", "xml" },
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
  end,
})

-- PLUGIN CONFIGURATION
-- toggles limelight when entering or exiting goyo
vim.api.nvim_create_autocmd("User", {
  pattern = "GoyoEnter",
  callback = function()
    vim.cmd("Limelight")
  end,
})

vim.api.nvim_create_autocmd("User", {
  pattern = "GoyoLeave",
  callback = function()
    vim.cmd("Limelight!")
  end,
})
