output=""
for proc in $(find /proc -maxdepth 1 | grep "[0-9]")
do
    # codes
    PID=${proc:6}
    STATE_CODE=`expr 52 - 3`
    PPID_CODE=`expr 52 - 4`
    TTY_CODE=`expr 52 - 7`
    RSS_CODE=`expr 52 - 24`
    SID_CODE=`expr 52 - 6`
    PGID_CODE=`expr 52 - 5`
    CODES=($STATE_CODE $PPID_CODE $TTY_CODE $RSS_CODE $SID_CODE $PGID_CODE)
    COMM=$(cat $proc/comm)
    
    output+=$PID$"\t"
    output+=$COMM$"\t"
    for code in ${CODES[@]}
    do
        output+=$(awk '{ print $(NF-'$code') } ' $proc/stat 2>/dev/null)$"\t"
    done

    output+='\n'
done
echo -e $output | column -s $'\t' --table --table-columns PID,COMM,STATE,PPID,TTY,RSS,SID,PGID 
# echo -e $output 
