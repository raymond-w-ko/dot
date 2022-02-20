(module rko.plugin.formatter
  {autoload {nvim aniseed.nvim
             formatter formatter}})

(formatter.setup
  {:filetype
   {:javascript
    [(fn []
       (let [fname (-> (nvim.buf_get_name 0)
                       (nvim.fn.fnameescape))]
         {:exe "prettier"
          :args ["--stdin-filepath" fname]
          :stdin true}))]
    :java
    [(fn []
       (let [fname (-> (nvim.buf_get_name 0)
                       (nvim.fn.fnameescape))]
         {:exe "prettier"
          :args ["--stdin-filepath" fname]
          :stdin true}))]}})

(vim.keymap.set "n" :<leader>f ":w<cr>:Format<cr>" {})
