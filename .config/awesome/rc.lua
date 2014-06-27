-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local vicious = require('vicious')

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(awful.util.getdir("config") .. "/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
editor = os.getenv("EDITOR") or "vi"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"
local wheel_down_button = 5
local wheel_up_button = 4

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    awful.layout.suit.max,
    awful.layout.suit.tile,
    --awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.floating,
    --awful.layout.suit.tile.top,
    --awful.layout.suit.fair,
    --awful.layout.suit.fair.horizontal,
    --awful.layout.suit.spiral,
    --awful.layout.suit.spiral.dwindle,
    --awful.layout.suit.max.fullscreen,
    --awful.layout.suit.magnifier,
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, layouts[1])
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   --{ "manual", terminal .. " -e man awesome" },
   --{ "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibox
-- Create a wibox for each screen and add it
local bottom_wibox = {}
local top_wibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, wheel_down_button, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, wheel_up_button, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({
                                                      theme = { width = 250 }
                                                  })
                                              end
                                          end),
                     awful.button({ }, wheel_down_button, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, wheel_up_button, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

local function format_speed(speed, output)
    if speed >= 1024 then
        speed = speed / 1024
        table.insert(output, string.format('%.02f', speed))
        table.insert(output, ' MB/s')
    else
        table.insert(output, tostring(speed))
        table.insert(output, ' KB/s')
    end
    return speed
end

local info_widgets = {}

local function make_separator()
    local separator = wibox.widget.textbox()
    separator:set_text(' :: ')
    table.insert(info_widgets, separator)
end

local function get_ip_addr(dev)
    local text = io.popen('ip addr show ' .. dev .. ' | grep inet'):read('*all')
    local index1 = text:find('inet ')
    local index2 = text:find('/')
    if index1 and index2 then
        local ip = text:sub(index1 + 5, index2 - 1)
        return ip
    else
        return 'IP N/A'
    end
end

-- ethernet
local eth_dev = io.popen('ip addr | grep enp'):read('*all')
local index1 = eth_dev:find(' ')
local index2 = eth_dev:find(' ', index1 + 1)
eth_dev = eth_dev:sub(index1 + 1, index2 - 2)
local eth_widget = wibox.widget.textbox() 
local function format_func(widget, args)
    local output = {}
    --for k, v in pairs(args) do
        --table.insert(output, k)
    --end
    table.insert(output, eth_dev)
    table.insert(output, ' - ')

    table.insert(output, get_ip_addr(eth_dev))
    table.insert(output, ' - ')

    table.insert(output, 'down ')
    local down = tonumber(args['{'.. eth_dev .. ' down_kb}'])
    format_speed(down, output)

    table.insert(output, ' - up ')
    local up = tonumber(args['{' .. eth_dev .. ' up_kb}'])
    format_speed(up, output)

    output = table.concat(output)
    return output
end
vicious.register(eth_widget, vicious.widgets.net, format_func, 1, eth_dev)
table.insert(info_widgets, eth_widget)

make_separator()

-- wifi
local wifi_dev = io.popen('iwconfig | grep wlp'):read('*all')
local index = wifi_dev:find(' ')
wifi_dev = wifi_dev:sub(1, index - 1)
local wifi_widget = wibox.widget.textbox() 
local function format_func(widget, args)
    local output = {}

    local ssid = args['{ssid}']
    local quality = tonumber(args['{linp}'])

    table.insert(output, '<span color="%s"> ')
    table.insert(output, wifi_dev)
    table.insert(output, ' ')
    if quality and quality > 0 and ssid ~= 'N/A' then
        table.insert(output, get_ip_addr(wifi_dev))
        table.insert(output, ' ')
    end
    table.insert(output, ssid)
    table.insert(output, ' @ ')
    -- linp should be link, but is mispelled in library
    table.insert(output, tostring(quality))
    table.insert(output, '%% ')
    table.insert(output, args['{rate}'])
    table.insert(output, 'MB/s')


    table.insert(output, '</span>')

    output = table.concat(output)

    local color
    if quality >= 90 then
        color = 'green'
    elseif quality >= 55 then
        color = 'yellow'
    else
        color = 'red'
    end
    output = output:format(color)

    return output
end
vicious.register(wifi_widget, vicious.widgets.wifi, format_func, 7, wifi_dev)
table.insert(info_widgets, wifi_widget)

make_separator()

-- battery
local battery_widget = wibox.widget.textbox()
local function format_func(widget, args)
    local state = args[1]
    local percent = tonumber(args[2])
    local time = args[3]
    local color
    if percent <= 20 then
        color = 'red'
    elseif percent <= 50 then
        color = 'yellow'
    else
        color = 'green'
    end
    local output = '<span color="%s">bat %d%% %s</span>'
    output = output:format(color, percent, time)
    return output
end
vicious.register(battery_widget, vicious.widgets.bat, format_func, 61, "BAT0")
table.insert(info_widgets, battery_widget)

make_separator()

-- CPU core temperature
local temp_widget = wibox.widget.textbox()
local function format_func(widget, args)
    local temp = tonumber(args[1])
    local color
    if temp >= 70 then
        color = 'red'
    elseif temp >= 55 then
        color = 'yellow'
    else
        color = 'green'
    end
    local output = '<span color="%s">cpu core1: %d °C</span> '
    output = output:format(color, temp)
    return output
end
vicious.register(temp_widget, vicious.widgets.thermal,
                 format_func, nil,
                 {'coretemp.0/hwmon/hwmon0/', 'core', 'temp1_input'})
table.insert(info_widgets, temp_widget)

-- CPU graph
local cpu_widget = awful.widget.graph()
cpu_widget:set_width(50)
cpu_widget:set_background_color("#494B4F")
cpu_widget:set_color(
    {
        type = "linear",
        from = { 0, 0 },
        to = { 10,0 },
        stops = { {0, "#FF5656"}, {0.5, "#88A175"}, {1, "#AECF96" }}
    })
vicious.register(cpu_widget, vicious.widgets.cpu, "$1", 2)
table.insert(info_widgets, cpu_widget)

make_separator()

-- memory widget
local mem_widget = wibox.widget.textbox()
vicious.register(mem_widget, vicious.widgets.mem,
                 'mem $1% <span color="#555555">($2MB / $3MB)</span>', 13)
table.insert(info_widgets, mem_widget)

make_separator()

-- free space on /
local fs_widget = wibox.widget.textbox()
vicious.register(fs_widget, vicious.widgets.fs, "/ ${/ avail_gb} GB", 599)
table.insert(info_widgets, fs_widget)

make_separator()

-- free space on /mnt/data
local fs_widget = wibox.widget.textbox()
vicious.register(fs_widget, vicious.widgets.fs, "/mnt/data/ ${/mnt/data avail_gb} GB", 599)
table.insert(info_widgets, fs_widget)

make_separator()

clock_widget = awful.widget.textclock('%a %b %d, %I:%M:%S %p', 5)
table.insert(info_widgets, clock_widget)

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, wheel_down_button, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, wheel_up_button, function () awful.layout.inc(layouts, 1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create the wibox
    bottom_wibox[s] = awful.wibox({ position = "bottom", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(mylayoutbox[s])
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()

    for _, widget in ipairs(info_widgets) do
        right_layout:add(widget)
    end

    if s == 1 then
        local separator = wibox.widget.textbox()
        separator:set_text(' :: ')
        table.insert(info_widgets, separator)
        right_layout:add(wibox.widget.systray())
    end

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    --layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    bottom_wibox[s]:set_widget(layout)

    -- create top toolbar with just tasklist

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    top_wibox[s] = awful.wibox({ position = "top", screen = s })
    local layout = wibox.layout.fixed.horizontal()
    layout:add(mytasklist[s])

    top_wibox[s]:set_widget(layout)
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, wheel_down_button, awful.tag.viewnext),
    awful.button({ }, wheel_up_button, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    --awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.toggletag(tag)
                          end
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
         awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- buttons for the titlebar
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                )

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
