#!/bin/sh
# Switches between dvorak, diktor and basic layouts

if [[ $1 == -d ]]; then
    setxkbmap us,ru dvp,diktor -option 'grp:rctrl_toggle'
elif [[ $1 == -b ]]; then
    setxkbmap us,ru -option 'grp:rctrl_toggle'
elif [[ $1 == -t ]]; then
    if setxkbmap -print | grep -q "dvp"; then
        setxkbmap us,ru -option 'grp:rctrl_toggle'
    else
        setxkbmap us,ru dvp,diktor -option 'grp:rctrl_toggle'
    fi
elif [[ $1 == -login ]]; then
    setxkbmap us,us dvp, -option 'grp:rctrl_toggle'
else
    echo "Use -d for dvp and diktor, -b for basic layout, -t to toggle"
fi
