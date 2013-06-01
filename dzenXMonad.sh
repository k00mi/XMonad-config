#!/bin/zsh

bar() {
    echo $1 | gdbar -fg '#aecf96' -bg '#494b4f' -nonl
}

mpd() {
     output=$(mpc | head -2)

     if [ $(echo "$output" | wc -l) -eq 1 ]; then
       echo "[STOPPED]"
       return 1
     fi

     state=$(echo $output | tail -1 | awk '{print $1 }' | tr -d '[]')

     case $state in
       playing) state='|>';;
       paused)  state='||';;
     esac

     echo -n "$state "
     echo -n "$(echo $output | head -1) "
     echo -n "$(bar $(echo $output | tail -1 | awk '{print $4 }' | tr -d '()%'))"
}

battery() {
    state=$(acpi -b | cut -d ' ' -f 3 | tr -d ',')

    case $state in
      Discharging) state='↓';;
      Unknown)     state='?';;
      Charging)    state='↑';;
      Full)        state='⬒';;
    esac

    percentage=$(acpi -b | cut -d "," -f 2 | tr -d " %")

    echo -n "$state$percentage%"
}

while true; do
  echo " $(battery) | $(mpd) | $(date +'%a %B %d, %H:%M') "
    sleep 3
done | dzen2 -x 220 -y 0 -h 14 -ta right -fn "inconsolata:size=9" -bg "#161616"
