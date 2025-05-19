# Dotfiles

My personal dotfiles (no guarantees it will work on your machine).
I use Arch btw.

## Requirements

### Core Dependencies

#### LLM API

You need to set an environment variable for you groq api key to have access to LLM stuff; it requires an account

- `GROQ_API_KEY` [Groq API key](https://console.groq.com/keys)

You can also setup [other models and providers](https://github.com/karthink/gptel?tab=readme-ov-file#setup) such as OpenAI, Anthropic and Kagi

#### Github access

For Igist, follow [this](https://github.com/KarimAziev/igist?tab=readme-ov-file#auth)

#### Basic dependencies

```bash
# Core system
sudo pacman -S stow git emacs hyprland curl

# Optional AUR helper (choose one)
# yay or paru
```

### Hyprland Dependencies

```bash
# Window Manager essentials
sudo pacman -S waybar swaybg mako fcitx5 bemenu kitty nemo brightnessctl polkit-gnome

# Screenshot and media
sudo pacman -S hyprshot pulseaudio # or pipewire-pulse (it bugged so much I gave up and got back to pulseaudio)
```

### Emacs Packages

Most Emacs packages will be automatically installed via package.el, including:
- evil
- evil-commentary
- which-key
- nerd-icons
- doom-modeline
- gptel
- igist
- autothemer
- eev
- symbol-overlay

### Fonts

```bash
sudo pacman -S ttf-cascadia-code ttf-nerd-fonts-symbols
```

### Themes (included in dotfiles)

- [Gruvbox GTK Theme](https://github.com/Fausto-Korpsvart/Gruvbox-GTK-Theme)
- Gruvbones

### Optional Dependencies

```bash
# Browser
sudo pacman -S google-chrome

# For AI integration
# Requires Groq API key set as GROQ_API_KEY environment variable
```

Note: Some packages might be available in the AUR rather than the main repositories. Use your preferred AUR helper (yay/paru) to install them.

## Overview

This repository contains my personal configuration files for:

- Emacs
- Hyprland ricing and OS specific settings

I use [GNU Stow](https://www.gnu.org/software/stow/) to manage the symlinks.

Also, I don't like to have tons of different files for GNU Emacs configuration, so I have a single file ".emacs" instead of an "init.el", and a directory ".emacs.d" for the packages and themes.

## Installation

```bash
git clone https://github.com/PHAredes/dotfiles.git ~/dotfiles
cd ~/dotfiles
stow dotfiles # This will symlink everything into your home directory
stow .emacs.d # This will symlink the emacs configuration directory
stow .emacs # This will symlink the emacs configuration "init.el" file
```

I recommend you `rm -rf ~/dotfiles/.git` and do your own thing from there.

## Credits

Most things here are inspired by [bennetthardwick](https://github.com/bennetthardwick/dotfiles) and [b-coimbra](https://github.com/b-coimbra/.emacs.d). I also got a lot of help from [edrx](https://github.com/edrx/eev) for automation with eev and Elisp.

If you don't use Emacs, check out these awesome AI scripts by [VictorTaelin](https://github.com/VictorTaelin/AI-scripts). While I now use [gptel](https://github.com/karthink/gptel) + eev for my workflow, these scripts are still great alternatives.

Pro tip: chatsh.mjs and holefill.mjs worked way better with Bun at my potato machine, Node.js took longer than Groq API to respond.

- Gruvbox theme: https://github.com/morhetz/gruvbox
- Hyprland ricing: https://github.com/bennetthardwick/dotfiles
- GTK theme: https://github.com/Fausto-Korpsvart/Gruvbox-GTK-Theme
- Megumacs: https://github.com/b-coimbra/.emacs.d
- Eev: https://github.com/edrx/eev

## TODO

- Add an automated script if I ever need to setup a new machine from scratch.
