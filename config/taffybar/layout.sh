#!/usr/bin/env bash

layout=$(/home/anton/.nix-profile/bin/xkblayout-state print %s)

if [ $layout = "diktor" ]; then
    layout="ru"
fi

echo $layout
