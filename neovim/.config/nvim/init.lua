-- Neovim's configuration file

-- PLUGIN MANAGER
require("config.lazy")

-- GENERAL OPTIONS
vim.opt.shadafile = "NONE"
vim.opt.virtualedit = 'onemore'     -- enable moving the cursor past the last char in normal mode
vim.opt.clipboard = 'unnamedplus'   -- use system's clipboard
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
vim.opt.splitbelow = true
vim.opt.number = true               -- set line numbering on
vim.opt.relativenumber = true       -- set numbering relative to the current line
vim.opt.scrolloff = 10              -- min lines above/below the cursor
vim.opt.sidescrolloff = 5           -- min columns to the left/right of the cursor
vim.opt.showcmd = true              -- show partial commands on the status line
vim.opt.list = true                 -- display special chars
vim.opt.wrapscan = false            -- disable cycles when searching
vim.opt.cursorline = true
vim.opt.cursorcolumn = false

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
  vim.opt.showbreak ='↳ '

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
  vim.cmd("colorscheme slate")

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

-- select the current line
vim.keymap.set('n', '<Leader>l', 'vg_')
vim.keymap.set('n', '<Leader>L', '^vg_')

-- copy the current line (minus newline)
vim.keymap.set('n', 'yY', '^vg_y')

-- navigate tabs
vim.keymap.set('n', '<Leader>tn', '<Cmd>tabnext<CR>')
vim.keymap.set('n', '<Leader>tp', '<Cmd>tabprevious<CR>')

-- navigate buffers
vim.keymap.set('n', '<Leader>bn', '<Cmd>bnext<Cr>')
vim.keymap.set('n', '<Leader>bp', '<Cmd>bprev<Cr>')
vim.keymap.set('n', '<Leader>bi', '<Cmd>buffers<CR>:buffer<Space>')

-- navigate windows
vim.keymap.set('n', '<c-Left>', '<Cmd>wincmd h<CR>')
vim.keymap.set('n', '<c-Down>', '<Cmd>wincmd j<CR>')
vim.keymap.set('n', '<c-Up>', '<Cmd>wincmd k<CR>')
vim.keymap.set('n', '<c-Right>', '<Cmd>wincmd l<CR>')
vim.keymap.set('n', '<Leader>w<Left>', '<Cmd>wincmd h<CR>')
vim.keymap.set('n', '<Leader>w<Down>', '<Cmd>wincmd j<CR>')
vim.keymap.set('n', '<Leader>w<Up>', '<Cmd>wincmd k<CR>')
vim.keymap.set('n', '<Leader>w<Right>', '<Cmd>wincmd l<CR>')

-- split current window (open new ones to the right and below, respectively)
vim.keymap.set('n', '<Leader>w/', ':vsplit<Space>')
vim.keymap.set('n', '<Leader>w-', ':split<Space>')

-- close current buffer, tab, and window
vim.keymap.set('n', '<Leader>bd', '<Cmd>bdelete<CR>')
vim.keymap.set('n', '<Leader>bD', '<Cmd>bdelete!<CR>')
vim.keymap.set('n', '<Leader>td', '<Cmd>tabclose<CR>')
vim.keymap.set('n', '<Leader>wd', '<Cmd>close<CR>')
vim.keymap.set('n', '<Leader>wD', '<Cmd>close!<CR>')
vim.keymap.set('n', '<Leader>wq', '<Cmd>qa<CR>')
vim.keymap.set('n', '<Leader>wQ', '<Cmd>qa!<CR>')
vim.keymap.set('n', '<Leader>bs', '<Cmd>w<CR>')
vim.keymap.set('n', '<Leader>bx', '<Cmd>x<CR>')

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
vim.keymap.set('n', '<Leader>vl', '<Cmd>set list!<CR>')
vim.keymap.set('n', '<Leader>vw', '<Cmd>set wrap!<CR>')

-- line highlight
vim.keymap.set('n', '<Leader>vh', '<Cmd>set cursorline!<CR>')

-- language-specific indenting with two whitespaces
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "lua", "markdown" },
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
  end,
})

