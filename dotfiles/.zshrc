# p10k setup
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# OMZ setup
export ZSH="$HOME/.oh-my-zsh"

# Set p10k as OMZ theme
ZSH_THEME="powerlevel10k/powerlevel10k"

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

# source p10k, run `p10k configure` or edit ~/.p10k.zsh to customize
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# source colorscheme
source ~/gruvbox-material-dark.zsh

# Mise setup
eval "$(~/.local/bin/mise activate zsh)"

# Add emacs to path
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/.elan/bin:$PATH"

source ~/.secrets

# ghcup setup
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# cabal varenv
export PATH=$HOME/.cabal/bin:$PATH
