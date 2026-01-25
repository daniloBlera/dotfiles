-- Configuration file for the `vis` editor

-- load standard vis module, providing parts of the Lua API
require('vis')

vis.events.subscribe(vis.events.INIT, function()
    -- Your global configuration options
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win) -- luacheck: no unused args
    -- Your per window configuration options e.g.
    -- vis:command('set number')
    vis:command('set tabwidth 4')
    vis:command('set autoindent on')
    vis:command('set expandtab on')
    vis:command('set number on')
    vis:command('set showtabs on')
    vis:command('set shownewlines on')
    vis:command('set showspaces on')
end)
