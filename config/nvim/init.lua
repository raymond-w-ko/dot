-- just for bootstrapping "packer" and "aniseed"

local fmt = string.format
local fn = vim.fn
local cmd = vim.api.nvim_command

local pack_path = vim.fn.stdpath("data") .. "/site/pack"

function ensure(user, repo)
  local install_path = fmt("%s/packer/start/%s", pack_path, repo)
  if fn.empty(fn.glob(install_path)) > 0 then
    cmd(fmt("!git clone https://github.com/%s/%s %s", user, repo, install_path))
    cmd(fmt("packadd %s", repo))
  end
end

ensure("wbthomason", "packer.nvim")
ensure("Olical", "aniseed")

vim.g["aniseed#env"] = {
  module = "rko.init",
  compile = true,
}
