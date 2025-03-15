#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export VISUAL="nvim"
eval "$(/home/pedro/.local/bin/mise activate bash)"

[ -f "/home/pedro/.ghcup/env" ] && . "/home/pedro/.ghcup/env" # ghcup-env