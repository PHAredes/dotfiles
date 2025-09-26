#
# ~/.bashrc
#

# General Aliases
alias firefox="google-chrome-stable"
alias cl="clear"
alias bashrc="vim ~/.bashrc"
alias holefill="holefill.mjs"
alias hypr="vim ~/.config/hypr/hyprland.conf"
alias ohmyzsh="mate ~/.oh-my-zsh"
alias update-bend="cargo install hvm bend-lang bend-language-server"
alias k-serve="start.sh"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export VISUAL="nvim"
eval "$(/home/pedro/.local/bin/mise activate bash)"

[ -f "/home/pedro/.ghcup/env" ] && . "/home/pedro/.ghcup/env" # ghcup-env

# Add emacs to path
export PATH="$HOME/.config/emacs/bin:$PATH"

# Source secrets file
source ~/.secrets

# Set default editor
export EDITOR="emacsclient -nw -c -a ''"
