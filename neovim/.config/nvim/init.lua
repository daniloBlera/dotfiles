-- Neovim's configuration file
-- written in lua!!1!one! :O

-- PLUGIN MANAGER
require("config.lazy")

-- GENERAL OPTIONS
vim.opt.shadafile = "NONE"          -- disable reading or writing shada files
vim.opt.virtualedit = 'onemore'     -- allow the cursor to move past the end of the line
vim.opt.clipboard = 'unnamedplus'   -- use system's clipboard for all selection operations
vim.opt.tabstop = 4                 -- number of spaces a TAB counts for
vim.opt.shiftwidth = 4              -- number of spaces to use for each step of (auto)indent.
vim.opt.softtabstop = 4             -- number of spaces a TAB counts on edit operations
vim.opt.expandtab = true            -- tabs insert spaces
vim.opt.shiftround = true           -- round indent to a multiple of 'shiftwidth'
vim.opt.autoindent = true
vim.opt.linebreak = true            -- enable visual line wrapping
vim.opt.textwidth = 90
vim.opt.ignorecase = true           -- ignore case when searching
vim.opt.smartcase = true            -- unless the search pattern has uppercase chars
vim.opt.splitbelow = true           -- new window splits will be placed below
vim.opt.number = true               -- enable line numbering on
vim.opt.relativenumber = true       -- set numbering relative to the current line
vim.opt.scrolloff = 10              -- min lines above and below the cursor
vim.opt.sidescrolloff = 5           -- min columns to the left and right of the cursor
vim.opt.showcmd = true              -- show partial commands on the status line
vim.opt.list = true                 -- display special chars (spaces, tabs, newlines, etc)
vim.opt.wrapscan = false            -- disable wrap around the ends when searching
vim.opt.cursorline = true           -- enable higlighting the cusor's line
vim.opt.cursorcolumn = false        -- disable highlighting the cursor's column

-- disable auto-wrapping of text and comments
vim.opt.formatoptions = { t = false, c = false, }

-- our check for terminal support for pretty colours and unicode glyps
local function supports_truecolor()
  local term = os.getenv("TERM")
  return term and (
    term:match("256color") 
    or term:match("truecolor") 
    or term:match("xterm") 
    or term:match("st") 
    or term:match("alacritty")
  )
end

-- setting colors and list mode chars based on truecolor and unicode support
if supports_truecolor() then
  -- enable truecolor
  vim.opt.termguicolors = true
  vim.cmd("set t_Co=256")

  -- indicator that the line is visually wrapped
  vim.opt.showbreak = '↳ '

  -- unicode listchars
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
  -- probably using something like the linux TTY
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
    nbsp = '§',
  }
end

-- Keybindings
-- use `<Space>` as leader
vim.g.mapleader = ' '

-- disable moving to the next char when pressing `space`
vim.keymap.set('n', '<Space>', '<Nop>')

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
-- vim.keymap.set('i', '<Esc>', '<C-O>:stopinsert<CR>')

-- clear search highlight on Esc
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- select the current line
vim.keymap.set('n', '<leader>l', 'vg_')
vim.keymap.set('n', '<leader>L', '^vg_')

-- copy the current line (minus newline)
vim.keymap.set('n', 'yY', '^vg_y')

-- navigate tabs
vim.keymap.set('n', '<leader>tn', '<cmd>tabnext<CR>')
vim.keymap.set('n', '<leader>tp', '<cmd>tabprevious<CR>')

-- navigate buffers
vim.keymap.set('n', '<leader>bn', '<cmd>bnext<Cr>')
vim.keymap.set('n', '<leader>bp', '<cmd>bprev<Cr>')
vim.keymap.set('n', '<leader>bi', '<cmd>buffers<CR>:buffer<Space>')

-- navigate windows
vim.keymap.set('n', '<c-Left>', '<cmd>wincmd h<CR>')
vim.keymap.set('n', '<c-Down>', '<cmd>wincmd j<CR>')
vim.keymap.set('n', '<c-Up>', '<cmd>wincmd k<CR>')
vim.keymap.set('n', '<c-Right>', '<cmd>wincmd l<CR>')
vim.keymap.set('n', '<leader>w<Left>', '<cmd>wincmd h<CR>')
vim.keymap.set('n', '<leader>w<Down>', '<cmd>wincmd j<CR>')
vim.keymap.set('n', '<leader>w<Up>', '<cmd>wincmd k<CR>')
vim.keymap.set('n', '<leader>w<Right>', '<cmd>wincmd l<CR>')

-- split current window (open new ones to the right and below, respectively)
vim.keymap.set('n', '<leader>w/', ':vsplit<Space>')
vim.keymap.set('n', '<leader>w-', ':split<Space>')

-- close current buffer, tab, and window
vim.keymap.set('n', '<leader>bd', '<cmd>bdelete<CR>')
vim.keymap.set('n', '<leader>bD', '<cmd>bdelete!<CR>')
vim.keymap.set('n', '<leader>td', '<cmd>tabclose<CR>')
vim.keymap.set('n', '<leader>wd', '<cmd>close<CR>')
vim.keymap.set('n', '<leader>wD', '<cmd>close!<CR>')
vim.keymap.set('n', '<leader>wq', '<cmd>qa<CR>')
vim.keymap.set('n', '<leader>wQ', '<cmd>qa!<CR>')
vim.keymap.set('n', '<leader>bs', '<cmd>w<CR>')
vim.keymap.set('n', '<leader>bx', '<cmd>x<CR>')

-- center the screen on the cursor's line after a jump
vim.keymap.set('n', 'G', 'Gzz')
vim.keymap.set('n', 'n', 'nzz')
vim.keymap.set('n', 'N', 'Nzz')

-- enable moving cursor through soft-wrapped lines
vim.keymap.set('n', 'j', 'gj')
vim.keymap.set('n', 'k', 'gk')
vim.keymap.set('n', '<Down>', 'g<Down>')
vim.keymap.set('n', '<Up>', 'g<Up>')

-- toggle list mode and soft line wrapping
vim.keymap.set('n', '<leader>vl', '<cmd>set list!<CR>')
vim.keymap.set('n', '<leader>vw', '<cmd>set wrap!<CR>')

-- line highlight
vim.keymap.set('n', '<leader>vh', '<cmd>set cursorline!<CR>')

-- language-specific indents
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "lua", "markdown", "haskell" },
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
  end,
})
