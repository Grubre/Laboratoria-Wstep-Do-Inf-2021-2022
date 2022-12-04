# cat /proc/net/dev
# ==================================================================================================
# GET TRANSFER
# ==================================================================================================
RECEIVE_TRANSFER_COL=2
TRANSMITTED_TRANSFER_COL=10
print_received_transfer () {
    echo $(awk 'NR>2{print $'$RECEIVE_TRANSFER_COL'}' /proc/net/dev | awk '{s+=$1}END{print s}')
}
print_sent_transfer () {
    echo $(awk 'NR>2{print $'$TRANSMITTED_TRANSFER_COL'}' /proc/net/dev | awk '{s+=$1}END{print s}')
}
SENT_VALS=(0 0 0 0 0 0 0 0 0 0)
RECEIVED_VALS=(0 0 0 0 0 0 0 0 0 0)
DIAG_WIDTH=5
DIAG_HEIGHT=8
move_bars () {
    for ((sent_iter = ${#SENT_VALS[@]}-1; sent_iter > 0; sent_iter--)); do
        SENT_VALS[$sent_iter]=${SENT_VALS[$sent_iter - 1]}
        RECEIVED_VALS[$sent_iter]=${RECEIVED_VALS[$sent_iter - 1]}
    done
    RECEIVED_VALS=$(expr $(print_received_transfer) - $prev_received_transfer | cat)
    SENT_VALS=$(expr $(print_sent_transfer) - $prev_sent_transfer | cat)
}
repeat_char () {
    for ((i = 0; i < $2; i++)); do
        echo -n "$1"
    done
}
draw_diagram () {
    diag_in=("$@")
    # calculate max
    max=${diag_in[0]}
    for n in "${diag_in[@]}" ; do
        ((n > max)) && max=$n
    done
    if [[ $max -eq "0" ]]; then
        max=1
    fi
    repeat_char "=" $(expr $DIAG_WIDTH "*" 10 + 2)
    echo
    for ((diag_loop = $DIAG_HEIGHT; diag_loop > 0; diag_loop--)); do
        echo -n "|"
        for n in "${diag_in[@]}" ; do
            # echo -n $(echo "scale=0; $DIAG_HEIGHT * $n / $max" | bc) "<" $diag_loop
            if [ $(echo "scale=0; $DIAG_HEIGHT * $n / $max" | bc) -ge $diag_loop ]
            then
                repeat_char "█" $DIAG_WIDTH
            else
                repeat_char "." $DIAG_WIDTH
            fi
        done
        echo -n "|"
        if [[ $diag_loop -eq $DIAG_HEIGHT ]]; then
            echo -n " $(echo $max | numfmt --to=iec)                 "
        fi
        echo
    done

    repeat_char "=" $(expr $DIAG_WIDTH "*" 10 + 2)
    echo
}

# GET PER THREAD USAGE
# IN OTHER FILE
per_thread_info () {
    currentDate=$(date +%s%N | cut -b1-13)
    currentStats=$(cat /proc/stat)    

    cpus=$(echo "$currentStats" | grep -P 'cpu' | awk -F " " '{print $1}')

    for cpu in $cpus
    do
        currentLine=$(echo "$currentStats" | grep "$cpu ")
        user=$(echo "$currentLine" | awk -F " " '{print $2}')
        nice=$(echo "$currentLine" | awk -F " " '{print $3}')
        system=$(echo "$currentLine" | awk -F " " '{print $4}')
        idle=$(echo "$currentLine" | awk -F " " '{print $5}')
        iowait=$(echo "$currentLine" | awk -F " " '{print $6}')
        irq=$(echo "$currentLine" | awk -F " " '{print $7}')
        softirq=$(echo "$currentLine" | awk -F " " '{print $8}')
        steal=$(echo "$currentLine" | awk -F " " '{print $9}')
        guest=$(echo "$currentLine" | awk -F " " '{print $10}')
        guest_nice=$(echo "$currentLine" | awk -F " " '{print $11}')

        previousLine=$(echo "$previousStats" | grep "$cpu ")
        prevuser=$(echo "$previousLine" | awk -F " " '{print $2}')
        prevnice=$(echo "$previousLine" | awk -F " " '{print $3}')
        prevsystem=$(echo "$previousLine" | awk -F " " '{print $4}')
        previdle=$(echo "$previousLine" | awk -F " " '{print $5}')
        previowait=$(echo "$previousLine" | awk -F " " '{print $6}')
        previrq=$(echo "$previousLine" | awk -F " " '{print $7}')
        prevsoftirq=$(echo "$previousLine" | awk -F " " '{print $8}')
        prevsteal=$(echo "$previousLine" | awk -F " " '{print $9}')
        prevguest=$(echo "$previousLine" | awk -F " " '{print $10}')
        prevguest_nice=$(echo "$previousLine" | awk -F " " '{print $11}')    

        PrevIdle=$((previdle + previowait))
        Idle=$((idle + iowait))

        PrevNonIdle=$((prevuser + prevnice + prevsystem + previrq + prevsoftirq + prevsteal))
        NonIdle=$((user + nice + system + irq + softirq + steal))

        PrevTotal=$((PrevIdle + PrevNonIdle))
        Total=$((Idle + NonIdle))

        totald=$((Total - PrevTotal))
        idled=$((Idle - PrevIdle))

        CPU_Percentage=$(awk "BEGIN {print ($totald - $idled)/$totald*100}")

        if [[ "$cpu" != "cpu" ]]; then
            printf "$cpu %.2f%%        \n" "$CPU_Percentage"
        fi
    done
}
# UPTIME
print_uptime () {
    uptime=$(cat /proc/uptime | cut -d ' ' -f1)
    echo -en "\n"
    eval "echo Uptime = $(date -ud "@$uptime" +'$((%s/3600/24)) days %H hours %M minutes %S seconds')"
}
# BATTERY
print_battery () {
    full=$(cat /sys/class/power_supply/BAT1/uevent | grep "POWER_SUPPLY_CHARGE_FULL_DESIGN=" | cut -d '=' -f2)
    now=$(cat /sys/class/power_supply/BAT1/uevent | grep "POWER_SUPPLY_CHARGE_NOW=" | cut -d '=' -f2)
    echo "\nBattery = "$(echo "scale=2; 100 * $now / $full" | bc)"%"
}
# LOADAVG
print_loadavg () {
    echo "\nLoadavg = $(cat /proc/loadavg | cut -d " " -f1,2,3 --output-delimiter ', ')"
}
# MEMINFO
print_memory_usage () {
    total=$(cat /proc/meminfo | grep "MemTotal" | sed 's/  */ /g' | cut -d " " -f2)
    free=$(cat /proc/meminfo | grep "MemFree" | sed 's/  */ /g' | cut -d " " -f2)
    buffers=$(cat /proc/meminfo | grep "Buffers" | sed 's/  */ /g' | cut -d " " -f2)
    cache=$(cat /proc/meminfo | grep -w "Cached" | sed 's/  */ /g' | cut -d " " -f2)
    echo "\nMemory usage = "$(expr $total - $free - $buffers - $cache | numfmt --to=iec --from-unit=1000)
}
prev_received_transfer=$(expr $(print_received_transfer))
prev_sent_transfer=$(expr $(print_sent_transfer))

tput clear
while [[ true ]]; do
    output=""
    previousDate=$(date +%s%N | cut -b1-13)
    previousStats=$(cat /proc/stat)

    # sleep 1

    # PRINT TRANSFER
    move_bars
    tput cup 0 0
    output+="RECEIVED"$"\n"
    output+=$(draw_diagram "${RECEIVED_VALS[@]}")$"\n"
    output+="SENT"$"\n"
    output+=$(draw_diagram "${SENT_VALS[@]}")$"\n"
    prev_received_transfer=$(print_received_transfer)
    prev_sent_transfer=$(print_sent_transfer)

    output+=$(per_thread_info)
    output+=$(print_uptime)
    output+=$(print_battery)
    output+=$(print_loadavg)
    output+=$(print_memory_usage)
    echo -e "$output"
done
