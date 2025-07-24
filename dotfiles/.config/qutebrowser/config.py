config.load_autoconfig()

c.tabs.background = True
c.new_instance_open_target = 'window'
c.downloads.position = 'bottom'

config.bind(',ce', 'config-edit')
config.bind(',p', 'config-cycle -p content.plugins ;; reload')

config.bind(',rta', 'open {url}top/?sort=top&t=all')
config.bind(',rtv', 'spawn termite -e "rtv {url}"')
config.bind(',c', 'spawn -d chromium {url}')

# Keyboardio
config.bind('<Shift-Left>', 'back')
config.bind('<Shift-Down>', 'tab-next')
config.bind('<Shift-Up>', 'tab-prev')
config.bind('<Shift-Right>', 'forward')

c.search.incremental = False
c.editor.command = ['code', '-nw', '{}']

#c.qt.args = ['ppapi-widevine-path=/usr/lib/qt/plugins/ppapi/libwidevinecdmadapter.so']

c.content.javascript.enabled = True
config.source('gruvbox.py')

c.fonts.statusbar = '10pt Cascadia Mono'

# Font family for standard fonts.
# Type: FontFamily
c.fonts.web.family.standard = 'Inter'
