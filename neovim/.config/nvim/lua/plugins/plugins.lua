return {
  -- readline-based motions for insert mode
  "tpope/vim-rsi",

  -- comment lines
  "tpope/vim-commentary",

  -- surround regions with matching pairs of quotes, brackets, etc.
  "tpope/vim-surround",

  -- status and tab bars
  -- "itchyny/lightline.vim",

  -- status line
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' }
  },

  -- text motions based on single or two characters
  {
    "justinmk/vim-sneak",
    keys = {
      -- two characters jump
      { 's', '<plug>Sneak_s', mode = 'n' },
      { 'S', '<plug>Sneak_S', mode = 'n' },
      { 's', '<plug>Sneak_s', mode = 'x' },
      { 'S', '<plug>Sneak_S', mode = 'x' },

      -- replacing the f and t built-ins
      { 'f', '<plug>Sneak_f', mode = 'n' },
      { 'F', '<plug>Sneak_F', mode = 'n' },
      { 'f', '<plug>Sneak_f', mode = 'x' },
      { 'F', '<plug>Sneak_F', mode = 'x' },

      { 't', '<plug>Sneak_t', mode = 'n' },
      { 'T', '<plug>Sneak_T', mode = 'n' },
      { 't', '<plug>Sneak_t', mode = 'x' },
      { 'T', '<plug>Sneak_T', mode = 'x' },
    },
  },

  -- distraction-free, "zen" mode
  {
    'junegunn/goyo.vim',
    dependencies = {
      -- dim text blocks outside the cursor
      'junegunn/limelight.vim'
    },
    lazy = false,
    config = function()
        vim.g.goyo_width = 110

        -- settings when enabling goyo
        vim.api.nvim_create_autocmd('User', {
          pattern = 'GoyoEnter',
          callback = function()
            vim.cmd('Limelight0.8')
            vim.cmd('set nolist')
            vim.cmd('set nocursorline')
            vim.cmd('set nocursorcolumn')
          end,
        })

        -- settings when disabling goyo
        vim.api.nvim_create_autocmd('User', {
          pattern = 'GoyoLeave',
          callback = function()
            vim.cmd('Limelight!')
          end,
        })
    end,
    keys = {
      { '<leader>vz', '<cmd>Goyo<cr>', desc = 'Toggle Goyo mode' },
    },
  },

  -- a dark theme people seem to like
  {
    'folke/tokyonight.nvim',
    lazy = false,
    priority = 1000,
  },

  -- pretty purple theme
  {
    'theacodes/witchhazel',
    lazy = false,
    priority = 1000,
  },

  -- a nice set of light and dark themes that are easy on the eyes
  {
    'EdenEast/nightfox.nvim',
    lazy = false,
    priority = 1000,
  },

  -- you've probably seen these before
  {
    'catppuccin/nvim',
    name = 'catppuccin',
    priority = 1000,
  },

  -- show available keybindings in a popup
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
    },
    keys = {
      {
        "<leader>?",
        function()
          require("which-key").show({ global = false })
        end,
        desc = "Buffer Local Keymaps (which-key)",
      },
    },
  },

  -- fuzzy finder for files, buffers, etc
  {
    'nvim-telescope/telescope.nvim', tag = '0.1.8',
    dependencies = {
      'nvim-lua/plenary.nvim',
      { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' }
    },
    config = function()
      local builtin = require('telescope.builtin')
      vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Telescope find files' })
      vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
      vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope buffers' })
      vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })
    end
  },

  -- pattern-match files for opening in read-only mode
  {
    'bgaillard/readonly.nvim',
    opts = {
      display_modes = {
        command_line = { enabled = false },
      },
      pattern = {
        vim.fn.expand('~') .. '/.config/glirc/logs/*',
        vim.fn.expand('~') .. '/.ssh/*',
      }
    },
    lazy = false,
  },
}
