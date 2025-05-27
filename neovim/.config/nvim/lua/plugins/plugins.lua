return {
  -- readline-based motions for insert mode <3
  "tpope/vim-rsi",
  
  -- comment all the stuff
  "tpope/vim-commentary",

  -- surround regions with matching pairs of quotes, brackets, etc.
  "tpope/vim-surround",

  -- status and tab bars
  "itchyny/lightline.vim",

  -- text motions based on single or two characters
  {
    "justinmk/vim-sneak",
    keys = {
      -- two characters jump
      { 's', '<Plug>Sneak_s', mode = 'n' },
      { 'S', '<Plug>Sneak_S', mode = 'n' },
      { 's', '<Plug>Sneak_s', mode = 'x' },
      { 'S', '<Plug>Sneak_S', mode = 'x' },

      -- replacing the f and t built-ins
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
      { "<leader>z", "<cmd>Goyo<cr>", desc = "Toggle Goyo mode" },
    },
  },

  -- reduce the visibility of distant text
  {
    "junegunn/limelight.vim",
    lazy = false,
    keys = {
        { "<leader>f", "<cmd>Limelight!! 0.8<cr>", desc = "Toggle Limelight" },
    },
  },

  -- a dark theme lazy people seem to like
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    -- config = function()
    --   vim.cmd([[colorscheme tokyonight-night]])
    -- end,
  },

  -- pretty purple colors
	{
		"theacodes/witchhazel",
		lazy = false,
		priority = 1000,
		-- config = function()
		-- 	vim.cmd([[colorscheme witchhazel-hypercolor]])
		-- end,
	},

  -- a nice set of light and dark themes that are easy on the eyes
  {
    "EdenEast/nightfox.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      -- light themes: dayfox, dawnfox
      -- dark themes: duskfox, nightfox, nordfox, terafox, carbonfox
      vim.cmd([[colorscheme terafox]])
    end,
  },

  -- you've probably seen these before
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    -- config = function()
    --   vim.cmd([[colorscheme catppuccin-mocha]])
    -- end,
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
}
