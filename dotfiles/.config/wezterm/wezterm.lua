local wezterm = require 'wezterm'

return {
  -- Fonte e tamanho
  font = wezterm.font 'Cascadia Code',
  font_size = 12.0,
  -- Ajuste da altura da célula (equivalente a modify_font cell_height 110%)
  cell_width = 1.0,
  line_height = 1.1, -- Ajusta a altura da linha para 110%

  -- Desativar ligaturas
  harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' },

  -- Padding da janela
  window_padding = {
    left = 8,
    right = 8,
    top = 8,
    bottom = 8,
  },

  -- Não perguntar confirmação ao fechar a janela
  window_close_confirmation = 'NeverPrompt',

  color_scheme = 'zenwritten_light',
  -- Detecção de URLs
  hyperlink_rules = {
    -- Regras padrão para detectar URLs
    {
      regex = [[(https?://\S+)]],
      format = '$1',
      highlight = 0,
    },
  },

  -- Opacidade do fundo
  window_background_opacity = 0.9,

  -- Esconder decorações da janela
  window_decorations = 'NONE',

  -- Atraso de entrada (equivalente a input_delay 1)
  -- WezTerm não tem configuração direta para isso, mas é geralmente rápido o suficiente

  -- Desativar som de campainha
  audible_bell = 'Disabled',

  -- Configuração para Wayland
  enable_wayland = true,
  enable_tab_bar = false,
}
