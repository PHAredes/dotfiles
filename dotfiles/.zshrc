# OMZ setup
export ZSH="$HOME/.oh-my-zsh"

# Set p10k as OMZ theme
ZSH_THEME="minimal"

plugins=(
  git
  zsh-syntax-highlighting
  zsh-autosuggestions
)

# sourcing OMZ
source $ZSH/oh-my-zsh.sh

# Alias
alias firefox="google-chrome-stable"
alias cl="clear"
alias zshrc="vim ~/.zshrc"
alias holefill="holefill.mjs"
alias hypr="vim ~/.config/hypr/hyprland.conf"
alias ohmyzsh="mate ~/.oh-my-zsh"
alias update-bend="cargo install hvm bend-lang bend-language-server"

# Mise setup
eval "$(~/.local/bin/mise activate zsh)"

# Add emacs to path
export PATH="$HOME/.config/emacs/bin:$PATH"

source ~/.secrets

# ghcup setup
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# cabal varenv
export PATH=$HOME/.cabal/bin:$PATH
alias k-serve="start.sh"
export EDITOR="emacsclient -nw -c -a ''"
