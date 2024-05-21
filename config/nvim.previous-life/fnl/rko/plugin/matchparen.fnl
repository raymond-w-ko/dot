(module rko.plugin.matchparen
  {autoload {nvim aniseed.nvim
             matchparen matchparen}})

(matchparen.setup
  {:on_startup true
   :hl_group "MatchParen"
   :augroup_name "matchparen"})
