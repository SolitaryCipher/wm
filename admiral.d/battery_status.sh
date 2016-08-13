#!/bin/bash
. lemonbar_config.sh

BATPATH=/sys/class/power_supply/BAT0

POWER_SUPPLY_INFO=`cat $BATPATH/uevent`

get_value() 
{
    LINE=$(echo "$POWER_SUPPLY_INFO" | grep -wE "$1")
    echo $LINE | cut -d= -f2
}


#. $BATPATH/uevent
CAPACITY=$(get_value "POWER_SUPPLY_CAPACITY")
STATUS=$(get_value "POWER_SUPPLY_STATUS")

ENERGY=$(get_value "POWER_SUPPLY_ENERGY_NOW")
POWER=$(get_value "POWER_SUPPLY_POWER_NOW")
TOTAL=$(get_value "POWER_SUPPLY_ENERGY_FULL")
echo $ENERGY

HOURS=""
MINS=""
COLOR=""

if [ $STATUS == "Discharging" ]
then
    LIFE=$(  echo "scale=0;$ENERGY/$POWER" | bc -l)
    RESULT=$(echo "scale=3;$ENERGY/$POWER" | bc -l)
    RESULT=$(echo "$RESULT"                | bc -l)
    RESULT=$(echo "$RESULT - $LIFE"        | bc -l)
    RESULT=$(echo "$RESULT * 60"           | bc -l)

    HOURS=$LIFE
    MINS=`printf '%02.0f\n' $RESULT`
    if [ ! -z $HOURS ] && [ ! -z $MINS ]; then
      TIME=" ($HOURS:$MINS)"
    fi
elif [ $STATUS == "Charging" ]
then
    RESULT=$(echo "$TOTAL - $ENERGY"       | bc -l)
    LIFE=$(  echo "scale=0;$RESULT/$POWER" | bc -l)
    RESULT=$(echo "scale=3;$RESULT/$POWER" | bc -l)
    RESULT=$(echo "$RESULT * 60 * 1.3"     | bc -l)

    HOURS=$LIFE
    MINS=`printf '%02.0f\n' $RESULT`

    if [ ! -z $HOURS ] && [ ! -z $MINS ]; then
      TIME=" ($HOURS:$MINS)"
    fi
fi

if [ $STATUS == "Charging" ] || ( [ $STATUS == "Unknown" ] && [ "$CAPACITY" -gt 99 ]) 
then
    COLOR=$(bar_low)
elif [ "$CAPACITY" -lt 25 ]
then
    COLOR=$(bar_critical)
elif [ "$CAPACITY" -lt 50 ]
then
    COLOR=$(bar_warn)
else
    COLOR=$(bar_good)
fi

if [ "$CAPACITY" -gt 100 ]
then
    CAPACITY=100
fi

#BATTERY_COMMAND="urxvt -name barapp -e ~/.config/admiral.d/bar_app.sh ~/.local/bin/gcal2.sh"
#bar_clickable "$BATTERY_COMMAND" "$COLOR $CAPACITY% $STATUS$TIME "
echo "$COLOR $CAPACITY% $STATUS$TIME "
echo ""
#echo "$HOURS:$MINS"

