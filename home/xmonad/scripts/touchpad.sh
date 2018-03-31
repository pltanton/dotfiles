#!/bin/sh

TOUCHPAD="SynPS/2 Synaptics TouchPad"

if [[ $1 == -e ]]; then
    xinput enable "$TOUCHPAD"
elif [[ $1 == -d ]]; then
    xinput disable "$TOUCHPAD"
elif [[ $1 == -t ]]; then
    if [[ $(xinput list-props "$TOUCHPAD" | grep "Device Enabled") == *:*1 ]]; then
        xinput disable "$TOUCHPAD"
    else
        xinput enable "$TOUCHPAD"
    fi
else
    echo "Use with -e -d or -t option"
fi
