return {
  -- readline-based motions for insert mode <3
  "tpope/vim-rsi",
  
  -- comment all the stuff
  "tpope/vim-commentary",

  -- surround regions with matching pairs of quotes, brackets, etc.
  "tpope/vim-surround",

  -- status and tab bars
  "itchyny/lightline.vim",

  -- text motions based on single or double characters
  {
    "justinmk/vim-sneak",
    keys = {
      { 's', '<Plug>Sneak_s', mode = 'n' },
      { 'S', '<Plug>Sneak_S', mode = 'n' },
      { 's', '<Plug>Sneak_s', mode = 'x' },
      { 'S', '<Plug>Sneak_S', mode = 'x' },

      { 'f', '<Plug>Sneak_f', mode = 'n' },
      { 'F', '<Plug>Sneak_F', mode = 'n' },
      { 'f', '<Plug>Sneak_f', mode = 'x' },
      { 'F', '<Plug>Sneak_F', mode = 'x' },

      { 't', '<Plug>Sneak_t', mode = 'n' },
      { 'T', '<Plug>Sneak_T', mode = 'n' },
      { 't', '<Plug>Sneak_t', mode = 'x' },
      { 'T', '<Plug>Sneak_T', mode = 'x' },
    },
  },

  -- highway to the distraction-free zone
  {
    "junegunn/goyo.vim",
    lazy = false,
    config = function()
        vim.g.goyo_width = 100
    end,
    keys = {
      { "<leader>vz", "<cmd>Goyo<cr>", desc = "Toggle Goyo mode" },
    },
  },

  -- reduce visibility of text away from the cursor
  {
    "junegunn/limelight.vim",
    lazy = false,
    keys = {
        { "<leader>vf", "<cmd>Limelight!! 0.8<cr>", desc = "Toggle Limelight" },
    },
  },

  -- the colorscheme should be available when starting Neovim
  {
    "folke/tokyonight.nvim",
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
    config = function()
      -- load the colorscheme here
      vim.cmd([[colorscheme tokyonight]])
    end,
  },

  -- show available keybindings in a popup
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
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

  -- manage matching pairs
  {
    'echasnovski/mini.pairs',
    version = false,
    config = function()
      require('mini.pairs').setup()
    end
  },
}
