require'nvim-treesitter.configs'.setup {
  -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  -- ensure_installed = "maintained",
  -- ignore_install = { "javascript" }
  highlight = {
    enable = true,
    -- disable = { "c", "rust" },
  },
  rainbow = {
    enable = true,
    -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
    extended_mode = true,
  }
}
