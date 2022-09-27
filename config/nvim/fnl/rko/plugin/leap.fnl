(module rko.plugin.leap
  {autoload {nvim aniseed.nvim
             core aniseed.core
             leap leap}})

(leap.setup {})
(leap.set_default_keymaps)
(defn setup-leap-colors []
  (vim.api.nvim_set_hl 0 "LeapMatch" {:bg "#ffffff"})
  (vim.api.nvim_set_hl 0 "LeapLabelPrimary" {:bg "#f2bfbf" :fg "#3c3836"})
  (vim.api.nvim_set_hl 0 "LeapLabelSecondary" {:bg "#f2bfbf" :fg "#3c3836"}))

(setup-leap-colors)
(vim.api.nvim_create_augroup "rkoLeap" {:clear true})
(vim.api.nvim_create_autocmd "ColorScheme" {:group "rkoLeap" :callback setup-leap-colors})
