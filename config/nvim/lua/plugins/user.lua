-- [nfnl] Compiled from fnl/plugins/user.fnl by https://github.com/Olical/nfnl, do not edit.
local uu = require("dotfiles.util")
local function _1_()
  vim.g["selenized_variant"] = "normal"
  vim.o["background"] = "light"
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
return {uu.tx("Olical/nfnl", {priority = 9001, ft = {"fennel"}}), uu.tx("bakpakin/fennel.vim"), uu.tx("loganswartz/selenized.nvim", {dependencies = {"rktjmp/lush.nvim"}, config = _1_}), uu.tx("echasnovski/mini.pairs", {config = _2_, version = false}), uu.tx("nvim-telescope/telescope.nvim", {dependencies = {"nvim-lua/plenary.nvim"}, keys = {{"<leader>ff", "<cmd>Telescope find_files<cr>"}}, tag = "0.1.8"}), uu.tx("ggandor/leap.nvim", {dependencies = {"tpope/vim-repeat"}, config = _3_}), uu.tx("ggandor/flit.nvim", {dependencies = {"ggandor/leap.nvim"}, config = _4_})}
