#!bin/bash
final_string=""
for proc in $(ls /proc | grep -E '^[0-9]+$')
do
    # WCZYTYWANIE folderu /proc/PID/stat
    vals=$(cat /proc/$proc/stat 2>/dev/null)
    IFS=')'
    read -a valuesarr <<< "$vals"
    IFS=' '
    read -a valuesarr_pid_and_comm <<< "${valuesarr[0]}"
    values_split_in_two=${valuesarr[1]}
    rest_of_values=($values_split_in_two)

    # WCZYTYWANIE folderu /proc/PID/status
    values=$(cat /proc/$proc/status 2>/dev/null)
    # PID
    pid=$(grep -w "Pid" <<< $values)
    pid=${pid:5}
    # PPID
    ppid=$(grep -w "PPid" <<< $values)
    ppid=${ppid:6}
    # COMM
    comm=$(grep -w "Name" <<< $values)
    comm=${comm:6}
    # STATE
    state=$(grep -w "State" <<< $values)
    state=${state:7}
    # TTY
    tty=$(readlink /proc/$proc/fd/0)
    if [[ -z $tty ]]; then
        tty="?"
    fi
    # RSS
    rss=${rest_of_values[21]}
    # PGID
    pgid=$(grep -w "NSpgid" <<< $values)
    pgid=${pgid:8}
    # SID
    sid=$(grep -w "NSsid" <<< $values)
    sid=${sid:7}

    delimiter="~"

    if [[ ! -z $pid ]]; then
        final_string=$final_string"\n"$pid$delimiter$ppid$delimiter$comm$delimiter$state$delimiter$tty$delimiter$rss$delimiter$pgid$delimiter$sid

    fi


    

    done
echo -e $final_string | column -s '~' --table --table-columns PID,PPID,COMM,STATE,TTY,RSS,PGID,SID
