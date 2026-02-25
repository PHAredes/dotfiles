# AGENTS.md - Dotfiles Repository Guide

This repository contains personal dotfiles for Emacs configuration and Agda mathematical libraries, primarily managed using GNU Stow.

## Repository Structure

- `emacs/` - Contains Emacs configuration with:
  - `.emacs.d/` - Main Emacs configuration directory with modules
  - `.emacs` - Entry point for Emacs configuration
- `agda/` - Contains Agda mathematical libraries including:
  - `.agda/cubical/` - Cubical Agda standard library
  - `.agda/agda-stdlib/` - Standard Agda library
- `.gitmodules` - Git submodules configuration
- `README.md` - Main documentation

## Emacs Configuration Structure

- `emacs/.emacs.d/init.el` - Main entry point
- `emacs/.emacs.d/lisp/init-packages.el` - Package management with straight.el
- `emacs/.emacs.d/lisp/init-core.el` - Core Emacs settings
- `emacs/.emacs.d/lisp/modules/` - Feature modules:
  - `ai.el` - AI integration
  - `completion.el` - Completion system
  - `editor.el` - Editor settings
  - `email.el` - Email configuration
  - `lang-agda.el` - Agda language support
  - `lang-go.el` - Go language support
  - `lang-haskell.el` - Haskell language support
  - `lang-janet.el` - Janet language support
  - `lang-markdown.el` - Markdown support
  - `lang-typescript.el` - TypeScript support
  - `lsp.el` - LSP support
  - `tools.el` - Tool configurations
  - `ui.el` - UI customization

## Agda Library Structure

- `agda/.agda/cubical/` - Cubical Agda library for Homotopy Type Theory
- `agda/.agda/agda-stdlib/` - Standard Agda library
- Both libraries use the `.agda-lib` configuration format

## Essential Commands

### For Dotfiles Management
```bash
# Install all dotfiles using stow
cd ~/dotfiles
stow emacs        # Symlink Emacs config
stow agda         # Symlink Agda config
stow .gitmodules  # Symlink git submodules config
```

### For Agda Development
```bash
# Navigate to the Agda library directory
cd ~/dotfiles/agda/.agda/cubical/

# Compile Agda files with the correct flags
agda --cubical --safe --no-import-sorts -WnoUnsupportedIndexedMatch --guardedness <file>.agda

# For type-checking all files
find . -name "*.agda" -exec agda --cubical --safe --no-import-sorts -WnoUnsupportedIndexedMatch --guardedness {} \;
```

### For Emacs Development
```bash
# Start Emacs with this configuration
emacs -q -l ~/.emacs.d/init.el

# Or if the stow symlink is in place
emacs
```

### Development Commands
- Install core dependencies: `sudo pacman -S stow git emacs hyprland curl`
- Emacs packages are automatically installed via package.el and straight.el
- Install fonts: `sudo pacman -S ttf-cascadia-code ttf-nerd-fonts-symbols`

## Code Patterns and Conventions

### Emacs Lisp
- Uses `use-package` for package configuration
- Uses `straight.el` as the package manager
- Follows a modular architecture with separate files for each feature
- Uses the `after!` macro for deferred execution after package loading

### Agda
- Uses Cubical Agda features (higher inductive types, univalence)
- Follows mathematical naming conventions
- Code is organized by mathematical domains (Algebra, Homotopy, Categories, etc.)
- Uses the `.agda` file extension
- The library uses `--cubical` flag for type-checking

## Testing Approach

### Agda
- Files are type-checked individually using the `agda` command
- The library includes a `Cubical.Everything.html` which serves as a comprehensive index
- Mathematical correctness is verified through type-checking rather than traditional testing

### Emacs Configuration
- Configuration can be tested in development by starting a new Emacs instance
- Use `M-x checkdoc` to verify documentation strings
- Use `M-x eval-buffer` to test elisp code interactively

## Important Gotchas and Non-Obvious Patterns

1. **Stow Usage**: This repository uses GNU Stow for symlink management. Run `stow` commands from the `/home/user/dotfiles` directory where each subdirectory represents a "package" (in stow terms).

2. **Emacs Configuration**: Uses a non-standard structure where there's both a `.emacs` file and `.emacs.d` directory. The `.emacs` file is symlinked to the user's home directory, and `.emacs.d` contains the actual configuration.

3. **Agda Dependencies**: The Agda configuration relies on specific library versions. The cubical library version must be compatible with the installed Agda version (currently works with Agda v2.8.0).

4. **AI Integration**: Requires setting the `GROQ_API_KEY` environment variable for LLM functionality. Alternative providers can also be configured.

5. **Module Loading**: The Emacs configuration loads all modules from the `lisp/modules` directory recursively, so any new module should be placed in that directory.

6. **Agda Mode**: The Emacs configuration includes Agda mode setup with custom keybindings (like `<leader>d` for definition lookup).

## Environment Requirements

- Arch Linux (as mentioned in README)
- GNU Stow for dotfile management
- Emacs 27+ for full functionality
- Agda 2.8.0 for Cubical library compatibility
- Git for version control

## Project-Specific Context

This appears to be a personal development environment setup focused on formal mathematics and theorem proving using Agda, with a customized Emacs environment. The configuration is optimized for working with Homotopy Type Theory and formal verification tasks.

The AI integration suggests this setup is used for assisted development workflows, with support for various programming languages beyond Agda.