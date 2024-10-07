-- [nfnl] Compiled from fnl/plugins/user.fnl by https://github.com/Olical/nfnl, do not edit.
local uu = require("dotfiles.util")
local function _1_()
  vim.g["selenized_variant"] = "normal"
  vim.o["background"] = "light"
  return vim.cmd("colorscheme selenized")
end
return {uu.tx("Olical/nfnl", {priority = 9001, ft = {"fennel"}}), uu.tx("bakpakin/fennel.vim"), uu.tx("shortcuts/no-neck-pain.nvim"), uu.tx("loganswartz/selenized.nvim", {dependencies = {"rktjmp/lush.nvim"}, config = _1_}), uu.tx("windwp/nvim-autopairs", {event = "InsertEnter", config = true}), uu.tx("nvim-telescope/telescope.nvim", {dependencies = {"nvim-lua/plenary.nvim"}, keys = {{"<leader>ff", "<cmd>Telescope find_files<cr>"}}, tag = "0.1.8"})}
