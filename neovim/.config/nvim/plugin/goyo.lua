-- zen-ish mode
vim.pack.add({
  'https://github.com/junegunn/limelight.vim',
  'https://github.com/junegunn/goyo.vim'
})

-- set how wide the editing area should be
vim.g.goyo_width = 110

-- when enabling goyo
vim.api.nvim_create_autocmd('User', {
  pattern = 'GoyoEnter',
  callback = function()
    vim.cmd('Limelight0.8')
    vim.cmd('set nolist')
    vim.cmd('set nocursorline')
    vim.cmd('set nocursorcolumn')
  end,
})

-- when disabling goyo
vim.api.nvim_create_autocmd('User', {
  pattern = 'GoyoLeave',
  callback = function()
    vim.cmd('Limelight!')
  end,
})

-- toggle goyo (and limelight)
vim.keymap.set('n', '<leader>vz', '<cmd>Goyo<cr>', { desc = 'Toggle Goyo mode' })
