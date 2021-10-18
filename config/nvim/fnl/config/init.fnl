(module config.init
  {autoload {core aniseed.core
             nvim aniseed.nvim
             str aniseed.string}})

(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")

(require :config.plugin)
