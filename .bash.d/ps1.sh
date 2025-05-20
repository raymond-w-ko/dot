#!/bin/bash

# -----------------------------------------------------------------------------
# Custom Bash PS1 Prompt
#
# This script sets a three-line bash prompt with the following structure:
# Line 1: (date)-()
# Line 2: (git branch)-(python venv)-(current working directory)
# Line 3: user @ hostname (ret: N) $
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

# Ensure the promptvars shell option is enabled (usually is by default)
# This allows prompt strings to undergo variable and command expansion.
shopt -s promptvars

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
        branch_name=$(__git_ps1 "%s")
        if [ -n "$branch_name" ]; then
            echo "($branch_name)"
        else
            echo "()"
        fi
    else
        echo "()"
    fi
}

# Function to get the current Python virtual environment, formatted as (venv) or ()
get_python_venv_for_ps1() {
    if [ -n "$VIRTUAL_ENV" ]; then
        echo "($(basename "$VIRTUAL_ENV"))"
    else
        echo "()"
    fi
}

get_cpu_load_for_ps1() {
    local load_1min=""

    # Determine OS and get 1-minute load average
    case "$(uname -s)" in
        Linux)
            if [ -r "/proc/loadavg" ]; then
                # Read the first field (1-minute average) from /proc/loadavg
                load_1min=$(awk '{print $1}' /proc/loadavg)
            fi
            ;;
        Darwin) # macOS
            if command -v sysctl >/dev/null; then
                # Get the first load average value (1-minute) from sysctl
                # Output of 'sysctl -n vm.loadavg' is typically like "0.30 0.25 0.20"
                load_1min=$(sysctl -n vm.loadavg 2>/dev/null | awk '{print $1}')
            fi
            ;;
        # You could add more OS checks here or a generic uptime parse as a fallback
        # *)
        #     # Fallback for other systems using uptime (parsing can be fragile)
        #     if command -v uptime >/dev/null; then
        #        load_1min=$(uptime | awk -F'[Ll]oad average[s]*: ' '{if(NF>1)print $2}' | cut -d, -f1 | sed 's/[^0-9.]//g')
        #     fi
        #     ;;
    esac

    if [ -n "$load_1min" ]; then
        echo "CPU: $load_1min"  # e.g., "CPU: 0.36"
    else
        echo "" # Return empty string if load couldn't be determined
    fi
}

# --- Define the PS1 variable ---

# Line 1: (date)
# \D{%Y-%m-%d} displays the date in YYYY-MM-DD format.
PS1_LINE1='(\D{%Y-%m-%d} \D{%H:%M:%S}) ($(get_cpu_load_for_ps1))'

# Line 2: (git branch) (python venv) (cwd)
# \$(...) is command substitution that gets executed each time the prompt is displayed.
# \w is the current working directory, with $HOME abbreviated with a tilde.
PS1_LINE2='$(get_git_branch_for_ps1) $(get_python_venv_for_ps1) \w'

# Line 3: user @ hostname (ret: N) $
# \u is the username.
# \h is the hostname up to the first '.'.
# \$? is the return code of the previously executed command.
# \\$ displays a '$' for a regular user or a '#' for the root user, followed by a space.
PS1_LINE3='\u @ \h (ret: $?) \$ '

# Combine the lines into the final PS1 string.
# Using \\n ensures that a literal backslash-n is part of the PS1 string value,
# which Bash then interprets as a newline when displaying the prompt.
export PS1="  ${PS1_LINE1}\\n  ${PS1_LINE2}\\n${PS1_LINE3}"

# Optional: If you use __git_ps1, you might want to enable some of its features.
# export GIT_PS1_SHOWDIRTYSTATE=1
# export GIT_PS1_SHOWSTASHSTATE=1
# export GIT_PS1_SHOWUNTRACKEDFILES=1
# export GIT_PS1_SHOWUPSTREAM="auto"

# --- End of Custom Bash PS1 Prompt ---
