#!/bin/bash
x=''
for file in $(find /proc -maxdepth 1 -type d -name [1-9]*) 
do
    if [ file!='' ];
    then
        buffor=`cat $file'/stat'`
        newline=''
        isThereNoName=true
        for (( i=0; i<${#buffor}; i++ )); 
        do
            c=`echo ${buffor:$i:1}`
            left="("
            right=")"
            if [[ "$c" == "$left" ]];
            then
                isThereNoName=false
            fi

            if [[ "$c" == "$right" ]];
            then
                isThereNoName=true
            fi

            if [[ "$c" == "$space" ]];
            then
                if $isThereNoName;
                then
                    newline+="|"
                else
                    newline+=" "
                fi
            else
                newline+=$c
            fi
        done
        x+=$newline
        x+='\n'
    fi
done
echo -e $x | column -t -s "|" --table-columns pid,comm,state,ppid,pgid,sid,tty,tpgid,flags,minflt,cminflt,majflt,cmajflt,utime,stime,cutime,priority,nice,num_threads,itrealvalue,starttime,vsize,rss,rsslim,startcode,endcode,startstack,kstkesp,kstkeip,signal,blocked,sigignore,sigcatch,wchan,nswap,cnswap,exit_signal,processor,rt_priority,policy,delayacct_blkio_ticks,guest_time,cguest_time,start_date,end_date,start_brk,arg_start,arg_end,env_start,env_end,exit_code --table-order ppid,pid,comm,state,tty,rss,pgid,sid --table-hide tpgid,flags,minflt,cminflt,majflt,cmajflt,utime,stime,cutime,priority,nice,num_threads,itrealvalue,starttime,vsize,rsslim,startcode,endcode,startstack,kstkesp,kstkeip,signal,blocked,sigignore,sigcatch,wchan,nswap,cnswap,exit_signal,processor,rt_priority,policy,delayacct_blkio_ticks,guest_time,cguest_time,start_date,end_date,start_brk,arg_start,arg_end,env_start,env_end,exit_code
