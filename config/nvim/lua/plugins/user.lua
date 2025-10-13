-- [nfnl] fnl/plugins/user.fnl
local uu = require("dotfiles.util")
local function _1_()
  _G.vim.g["selenized_variant"] = "normal"
  _G.vim.o["background"] = "dark"
  return vim.cmd("colorscheme selenized")
end
local function _2_()
  local MiniPairs = require("mini.pairs")
  return MiniPairs.setup()
end
local function _3_()
  local leap = require("leap")
  return leap.create_default_mappings()
end
local function _4_()
  local flit = require("flit")
  return flit.setup()
end
local function _5_()
  local lualine = require("lualine")
  return lualine.setup({theme = "selenized"})
end
return {uu.tx("Olical/nfnl", {priority = 9001, ft = {"fennel"}}), uu.tx("bakpakin/fennel.vim"), uu.tx("loganswartz/selenized.nvim", {dependencies = {"rktjmp/lush.nvim"}, config = _1_}), uu.tx("echasnovski/mini.pairs", {config = _2_, version = false}), uu.tx("nvim-telescope/telescope.nvim", {dependencies = {"nvim-lua/plenary.nvim"}, keys = {{"<leader>ff", "<cmd>Telescope find_files<cr>"}}, tag = "0.1.8"}), uu.tx("ggandor/leap.nvim", {dependencies = {"tpope/vim-repeat"}, config = _3_}), uu.tx("ggandor/flit.nvim", {dependencies = {"ggandor/leap.nvim"}, config = _4_}), uu.tx("nvim-lualine/lualine.nvim", {dependencies = {"loganswartz/selenized.nvim", "nvim-tree/nvim-web-devicons"}, config = _5_})}
