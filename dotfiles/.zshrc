# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# OMZ setup
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="powerlevel10k/powerlevel10k"

plugins=(
  git
  mise
)

source $ZSH/oh-my-zsh.sh

# ============= Environment Variables =============
export EDITOR="emacsclient -nw -c -a ''"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"

# Bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# GHCup
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# ============= Zoxide Config =============
export _ZO_ECHO=1
export _ZO_EXCLUDE_DIRS="$HOME/.cache:$HOME/.local/share/node_modules"
export _ZO_FZF_OPTS="--height=40% --layout=reverse"
export _ZO_RESOLVE_SYMLINKS=1

eval "$(zoxide init zsh --cmd cd)"

# ============= Aliases =============
alias firefox="google-chrome-stable"
alias cl="clear"
alias zshrc="vim ~/.zshrc"
alias hypr="vim ~/.config/hypr/hyprland.conf"
alias holefill="holefill.mjs"
alias update-bend="cargo install hvm bend-lang bend-language-server"
alias k-serve="start.sh"

# eza aliases
alias ls='eza --icons'
alias ll='eza -lh --icons'
alias la='eza -lha --icons'
alias lt='eza --tree --icons'

# ============= P10k Theme =============
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# ============= Mise Setup =============
export MISE_SHELL=zsh
export __MISE_ORIG_PATH="$PATH"

eval "$(~/.local/bin/mise activate zsh)"

mise() {
  local command
  command="${1:-}"
  if [ "$#" = 0 ]; then
    command /home/pha/.local/bin/mise
    return
  fi
  shift

  case "$command" in
  deactivate|shell|sh)
    if [[ ! " $@ " =~ " --help " ]] && [[ ! " $@ " =~ " -h " ]]; then
      eval "$(command /home/pha/.local/bin/mise "$command" "$@")"
      return $?
    fi
    ;;
  esac
  command /home/pha/.local/bin/mise "$command" "$@"
}

_mise_hook() {
  eval "$(/home/pha/.local/bin/mise hook-env -s zsh)";
}

typeset -ag precmd_functions;
if [[ -z "${precmd_functions[(r)_mise_hook]+1}" ]]; then
  precmd_functions=( _mise_hook ${precmd_functions[@]} )
fi

typeset -ag chpwd_functions;
if [[ -z "${chpwd_functions[(r)_mise_hook]+1}" ]]; then
  chpwd_functions=( _mise_hook ${chpwd_functions[@]} )
fi

_mise_hook

if [ -z "${_mise_cmd_not_found:-}" ]; then
    _mise_cmd_not_found=1
    if typeset -f command_not_found_handler >/dev/null; then
        functions -c command_not_found_handler _command_not_found_handler
    fi

    typeset -gA _mise_cnf_tried

    _mise_fallback() {
        local _cmd="$1"; shift
        if typeset -f _command_not_found_handler >/dev/null; then
            _command_not_found_handler "$_cmd" "$@"
            return $?
        else
            print -u2 -- "zsh: command not found: $_cmd"
            return 127
        fi
    }

    command_not_found_handler() {
        local cmd="$1"; shift

        if [[ "$cmd" == "mise" || "$cmd" == mise-* || -n "${_mise_cnf_tried["$cmd"]}" ]]; then
            _mise_fallback "$cmd" "$@"
            return $?
        fi

        if /home/pha/.local/bin/mise hook-not-found -s zsh -- "$cmd"; then
            _mise_hook
            if command -v -- "$cmd" >/dev/null 2>&1; then
                "$cmd" "$@"
                return $?
            fi
        else
            _mise_cnf_tried["$cmd"]=1
        fi

        _mise_fallback "$cmd" "$@"
    }
fi
