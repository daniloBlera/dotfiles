-- display available key bindings
vim.pack.add({ 'https://github.com/folke/which-key.nvim' })

local function showKeys()
  require('which-key').show({ global = false })
end

vim.keymap.set('n', '<leader>?', showKeys, { desc = "Buffer local keymaps (which-key)"})
