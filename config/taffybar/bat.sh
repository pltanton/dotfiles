#!/usr/bin/env bash

BAT_PREFIX_PATH=/sys/class/power_supply
HIGH=0
NOW=0

for BAT in $(ls $BAT_PREFIX_PATH | grep BAT); do
    CUR_HIGH=`cat $BAT_PREFIX_PATH/$BAT/energy_full`
    HIGH=$(($CUR_HIGH + $HIGH))

    CUR_NOW=`cat $BAT_PREFIX_PATH/$BAT/energy_now`
    NOW=$(($CUR_NOW + $NOW))
done

BAT_EMPTY=
BAT_QUATER=
BAT_HALF=
BAT_THREE_QUATER=
BAT_FULL=
BAT_AC=

BAT_PERCENTS="$(($NOW * 100 / $HIGH))"

if (( $(cat $BAT_PREFIX_PATH/AC/online) )); then
    BAT_ICON=$BAT_AC
elif (( $BAT_PERCENTS < 6 )); then
    BAT_ICON=$BAT_EMPTY
elif (( $BAT_PERCENTS < 26 )); then
    BAT_ICON=$BAT_QUATER
elif (( $BAT_PERCENTS < 51 )); then
    BAT_ICON=$BAT_HALF
elif (( $BAT_PERCENTS < 76 )); then
    BAT_ICON=$BAT_THREE_HALF
else
    BAT_ICON=$BAT_FULL
fi

echo $BAT_ICON $BAT_PERCENTS%
