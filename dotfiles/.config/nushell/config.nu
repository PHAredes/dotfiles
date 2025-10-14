alias firefox = google-chrome-stable
alias cl = clear
alias bashrc = vim ~/.bashrc
alias hypr = vim ~/.config/hypr/hyprland.conf
alias ohmyzsh = mate ~/.oh-my-zsh
alias k-serve = start.sh

# Default editor
$env.config.buffer_editor = ["emacsclient", "-nw"]

$env.config.show_banner = false

$env.PATH = ($env.PATH | prepend '/home/pedro/.cargo/bin')

# Load Gruvbox theme
source ~/.config/nushell/misc/gruvbox.nu
$env.config.color_config = ($env.config | merge { color_config: $gruvbox_theme })

use ($nu.default-config-dir | path join mise.nu)