#!/bin/bash

# -----------------------------------------------------------------------------
# Custom Bash PS1 Prompt
#
# This script sets a three-line bash prompt with the following structure:
# Line 1: (date)-()
# Line 2: (git branch)-(python venv)-(current working directory)
# Line 3: user @ hostname (ret code: N) $
#
# To use this script:
# 1. Save it to a file, for example: ~/.custom_bash_prompt.sh
# 2. Make it executable: chmod +x ~/.custom_bash_prompt.sh
# 3. Source this file from your ~/.bashrc file by adding the following line:
#    if [ -f ~/.custom_bash_prompt.sh ]; then
#        . ~/.custom_bash_prompt.sh
#    fi
# 4. Reload your .bashrc or open a new terminal: source ~/.bashrc
# -----------------------------------------------------------------------------

# Attempt to source git-prompt.sh for __git_ps1 function
# This function is used to display the current Git branch.
# Add or modify paths if git-prompt.sh is located elsewhere on your system.
if [ -f "$HOME/.git-prompt.sh" ]; then
    source "$HOME/.git-prompt.sh"
elif [ -f "/usr/lib/git-core/git-sh-prompt" ]; then # Common on Debian/Ubuntu
    source "/usr/lib/git-core/git-sh-prompt"
elif [ -f "/usr/share/git/completion/git-prompt.sh" ]; then # Older systems
    source "/usr/share/git/completion/git-prompt.sh"
elif [ -f "/usr/share/git-core/contrib/completion/git-prompt.sh" ]; then # Common on Fedora/RHEL
    source "/usr/share/git-core/contrib/completion/git-prompt.sh"
elif [ -f "/opt/local/share/git-core/contrib/completion/git-prompt.sh" ]; then # MacPorts
    source "/opt/local/share/git-core/contrib/completion/git-prompt.sh"
elif type brew &>/dev/null && [ -f "$(brew --prefix)/etc/bash_completion.d/git-prompt.sh" ]; then # Homebrew on macOS
    source "$(brew --prefix)/etc/bash_completion.d/git-prompt.sh"
fi

# Function to get the current Git branch, formatted as (branch) or ()
get_git_branch_for_ps1() {
    local branch_name
    if command -v __git_ps1 >/dev/null 2>&1; then
        # __git_ps1 "%s" returns just the branch name.
        # We'll wrap it in parentheses if it's not empty.
        branch_name=$(__git_ps1 "%s")
        if [ -n "$branch_name" ]; then
            echo "($branch_name)"
        else
            echo "()" # Not in a git repo, or __git_ps1 returned empty
        fi
    else
        echo "()" # __git_ps1 command not found
    fi
}

# Function to get the current Python virtual environment, formatted as (venv) or ()
get_python_venv_for_ps1() {
    if [ -n "$VIRTUAL_ENV" ]; then
        # Display the base name of the virtual environment path
        echo "($(basename "$VIRTUAL_ENV"))"
    else
        echo "()" # Not in a Python virtual environment
    fi
}

# --- Define the PS1 variable ---

# Line 1: (date)-()
# \D{%Y-%m-%d} displays the date in YYYY-MM-DD format.
PS1_LINE1='(\\D{%Y-%m-%d})-()'

# Line 2: (git branch)-(python venv)-(cwd)
# \$(...) is command substitution that gets executed each time the prompt is displayed.
# \w is the current working directory, with $HOME abbreviated with a tilde.
PS1_LINE2='\$(get_git_branch_for_ps1)-\$(get_python_venv_for_ps1)-\w'

# Line 3: user @ hostname (ret code: N) $
# \u is the username.
# \h is the hostname up to the first '.'.
# \$? is the return code of the previously executed command.
# \\$ displays a '$' for a regular user or a '#' for the root user, followed by a space.
PS1_LINE3='\u @ \h (ret code: \$?) \\$ '

# Combine the lines into the final PS1 string, separated by newlines (\n)
export PS1="\${PS1_LINE1}\n\${PS1_LINE2}\n\${PS1_LINE3}"

# Optional: If you use __git_ps1, you might want to enable some of its features.
# These are commented out by default. Uncomment to enable.
export GIT_PS1_SHOWDIRTYSTATE=1          # Mark dirty working trees (uncommitted changes)
export GIT_PS1_SHOWSTASHSTATE=1          # Mark if something is stashed
export GIT_PS1_SHOWUNTRACKEDFILES=1      # Mark presence of untracked files
export GIT_PS1_SHOWUPSTREAM="auto"       # Show upstream tracking information (ahead, behind, etc.)
export GIT_PS1_HIDE_IF_PWD_IGNORED=1     # Hide git status if current directory is ignored
export GIT_PS1_COMPRESSSPARSESTATE=1     # Compress sparse checkout status

# --- End of Custom Bash PS1 Prompt ---
