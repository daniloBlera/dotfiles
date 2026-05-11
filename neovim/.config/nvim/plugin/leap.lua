-- quick text motions
vim.pack.add({ 'https://codeberg.org/andyg/leap.nvim' })

-- for a description of how the jumps work, check:
--    https://codeberg.org/andyg/leap.nvim#how-to-use-it-tl-dr

-- s{char1}<enter>
-- s{char1}{char2}<label-or-any-key>
vim.keymap.set({ 'n', 'x', 'o' }, 's', '<Plug>(leap)')
vim.keymap.set('n', 'S', '<Plug>(leap-from-window)')
