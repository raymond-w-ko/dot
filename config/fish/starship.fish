# Keep the prompt settings beside this loader, including when sourced via a symlink.
if status is-interactive; and command -q starship
    set -gx STARSHIP_CONFIG (path resolve (path dirname (status filename))/starship.toml)
    starship init fish | source
end
