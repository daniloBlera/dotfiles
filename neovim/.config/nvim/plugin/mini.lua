-- a library of utilities
vim.pack.add({ 'https://github.com/nvim-mini/mini.nvim' })
vim.cmd.colorscheme('miniwinter')
require('mini.basics').setup()
require('mini.icons').setup()
require('mini.surround').setup()
require('mini.statusline').setup()
require('mini.tabline').setup()
require('mini.indentscope').setup()
