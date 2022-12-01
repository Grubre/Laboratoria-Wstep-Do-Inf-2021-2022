# cat /proc/net/dev
print_transfer () {
    thousand=1000
    million=1000000
    billion=1000000000
    if [[ $1 -lt $thousand ]]; then
        echo $1" b"
    elif [[ $1 -lt $million ]]; then
        val=`expr $1 / 1000`
        echo $val" kb"
    elif [[ $1 -lt $billion ]]; then
        val=`expr $1 / 1000000`
        echo $val" mb"

    fi
}

# GET TRANSFER

RECEIVE_TRANSFER_COL=2
TRANSMITTED_TRANSFER_COL=10
echo -n "Received transfer = "
print_transfer $(awk 'NR>2{print $'$RECEIVE_TRANSFER_COL'}' /proc/net/dev | awk '{s+=$1}END{print s}')
echo -n "Sent transfer = "
print_transfer $(awk 'NR>2{print $'$TRANSMITTED_TRANSFER_COL'}' /proc/net/dev | awk '{s+=$1}END{print s}')

# GET PER THREAD USAGE
max_cpu_fq=$(cat /sys/devices/system/cpu/cpufreq/policy0/cpuinfo_max_freq)
min_cpu_fq=$(cat /sys/devices/system/cpu/cpufreq/policy0/cpuinfo_min_freq)
cat /proc/cpuinfo | grep "cpu MHz" | cut -d " " -f3 | {
    for nr in $(cat)
    do
        echo "($nr*100*100-$min_cpu_fq)/($max_cpu_fq)" | bc -l
    done
}

# UPTIME
uptime=$(cat /proc/uptime | cut -d ' ' -f1)
echo "uptime = "$uptime

# while [[ true ]]; do
#     echo "abc"
#     sleep 1
# done
