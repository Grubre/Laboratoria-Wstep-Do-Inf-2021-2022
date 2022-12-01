output=""
for proc in $(find /proc -maxdepth 1 | grep "[0-9]")
do
    # codes
    PID=${proc:6}
    STATE_CODE=`expr 52 - 3`
    PPID_CODE=`expr 52 - 4`
    RSS_CODE=`expr 52 - 24`
    SID_CODE=`expr 52 - 6`
    PGID_CODE=`expr 52 - 5`
    CODES=($STATE_CODE $PPID_CODE $RSS_CODE $SID_CODE $PGID_CODE)
    COMM=$(cat $proc/comm 2>/dev/null)
    TTY=$(readlink $proc/fd/1 2>/dev/null)
    if [ -z $TTY ]
    then
        TTY="?"
    fi
        
    if [[ ! -z "$COMM" ]]
    then
        output+=$PID$"\t"
        output+=$COMM$"\t"
        for code in ${CODES[@]}
        do
            output+=$(awk '{ print $(NF-'$code') } ' $proc/stat 2>/dev/null)$"\t"
        done
        output+=$TTY
        output+='\n'
    fi

done
echo -e $output | column -s $'\t' --table --table-columns PID,COMM,STATE,PPID,RSS,SID,PGID,TTY
