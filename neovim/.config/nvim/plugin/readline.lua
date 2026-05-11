-- readline motions
vim.pack.add({ 'https://github.com/hiberabyss/readline.nvim' })

local rl = require 'readline'
vim.keymap.set({ 'n', 'i' }, '<M-f>', rl.forward_word)
vim.keymap.set({ 'n', 'i' }, '<M-b>', rl.backward_word)
vim.keymap.set({ 'n', 'i' }, '<C-a>', rl.beginning_of_line)
vim.keymap.set({ 'n', 'i' }, '<C-e>', rl.end_of_line)
vim.keymap.set({ 'n', 'i' }, '<M-d>', rl.kill_word)
vim.keymap.set({ 'n', 'i' }, '<M-BS>', rl.backward_kill_word)
vim.keymap.set({ 'n', 'i' }, '<C-w>', rl.unix_word_rubout)
vim.keymap.set({ 'n', 'i' }, '<C-k>', rl.kill_line)
vim.keymap.set({ 'n', 'i' }, '<C-u>', rl.backward_kill_line)
