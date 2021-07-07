require'nvim-treesitter.configs'.setup {
  -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  -- ensure_installed = "maintained",
  -- ignore_install = { "javascript" }
  highlight = {
    enable = false,
    disable = { "edn", "json" },
  },
  indent = {
    enable = true,
  },
  query_linter = {
    enable = true,
    use_virtual_text = true,
    lint_events = {"BufWrite", "CursorHold"},
  },
  rainbow = {
    enable = true,
    -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
    extended_mode = true,
  }
}
