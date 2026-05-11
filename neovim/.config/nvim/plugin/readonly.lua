-- mark files with read-only
vim.pack.add({ 'https://github.com/bgaillard/readonly.nvim' })

require('readonly').setup({
  display_modes = {
    command_line = { enabled = false },
  },
  pattern = {
    vim.fn.expand('~') .. '/.config/glirc/logs/*',
    vim.fn.expand('~') .. '/.ssh/*'
  },
})
