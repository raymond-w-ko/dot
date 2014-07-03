local awful = require("awful")
-- Widget and layout library
local wibox = require("wibox")
-- such a bad hack to load vicious, since proper awesome wm uses Lua 5.2
package.path = package.path .. ';/usr/share/lua/5.2/?/init.lua;/usr/share/lua/5.2/?.lua'
local vicious = require('vicious')

local info_widgets = {}

local function format_speed(speed, output)
    if speed >= 1024 then
        speed = speed / 1024
        table.insert(output, string.format('%.02f', speed))
        table.insert(output, ' MB')
    else
        table.insert(output, tostring(speed))
        table.insert(output, ' KB')
    end
    return speed
end

local function make_separator()
    local separator = wibox.widget.textbox()
    separator:set_text(' :: ')
    table.insert(info_widgets, separator)
end

local function get_ip_addr(dev)
    local text = io.popen('ip addr show ' .. dev .. ' | grep inet'):read('*all')
    local index1 = text:find('inet ')
    local index2 = text:find('/')
    local ip, success
    if index1 and index2 then
        ip = text:sub(index1 + 5, index2 - 1)
        success = true
    else
        ip = ''
        success = false
    end
    ip = '<span color="#FF7F00">' .. ip .. '</span>'
    return ip, success
end

local function get_dev_speed_stats(dev, ip_addr, args, output)
  table.insert(output, ' ')

  table.insert(output, ip_addr)
  table.insert(output, ' - ')

  table.insert(output, 'D ')
  local down = tonumber(args['{'.. eth_dev .. ' down_kb}'])
  format_speed(down, output)

  table.insert(output, ' - U ')
  local up = tonumber(args['{' .. eth_dev .. ' up_kb}'])
  format_speed(up, output)
end

-- ethernet
local eth_dev = io.popen('ip addr | grep enp'):read('*all')
local index1 = eth_dev:find(' ')
local index2 = eth_dev:find(' ', index1 + 1)
eth_dev = eth_dev:sub(index1 + 1, index2 - 2)
local eth_widget = wibox.widget.textbox() 
local function format_func(widget, args)
    local output = {}

    table.insert(output, '<span color="%s">')
    table.insert(output, eth_dev)

    local ip_addr, active = get_ip_addr(eth_dev)
    if active then
      get_dev_speed_stats(eth_dev, ip_addr, args, output)
    end

    table.insert(output, '</span>')

    output = table.concat(output)

    local color
    if active then
        color = 'green'
    else
        color = 'red'
    end
    output = output:format(color)
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

    table.insert(output, '<span color="%s">')
    table.insert(output, wifi_dev)
    table.insert(output, ' ')
    if quality and quality > 0 and ssid ~= 'N/A' then
        local ip_addr, active = get_ip_addr(wifi_dev)
        table.insert(output, ip_addr)
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
    if quality >= 80 then
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
    local output = '<span color="%s">cpu %d °C</span> '
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
                 'mem $1% <span color="#555555">$2MB</span>', 13)
table.insert(info_widgets, mem_widget)

make_separator()

-- free space on /
local fs_widget = wibox.widget.textbox()
vicious.register(fs_widget, vicious.widgets.fs, "/ ${/ avail_gb} GB", 599)
table.insert(info_widgets, fs_widget)

make_separator()

-- free space on /mnt/data
local text = io.popen('mount | grep /mnt/data'):read('*all')
if text:len() > 0 then
    local fs_widget = wibox.widget.textbox()
    vicious.register(fs_widget, vicious.widgets.fs, "/mnt/data/ ${/mnt/data avail_gb} GB", 599)
    table.insert(info_widgets, fs_widget)

    make_separator()
end


clock_widget = awful.widget.textclock('%a %b %d, %I:%M:%S %p', 5)
table.insert(info_widgets, clock_widget)

return info_widgets
