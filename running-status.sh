#!/bin/bash
#
# Creates a status report about the running R processes
# usage:
# $0
# $0 saturation

TYPE=$1
echo "TYPE is $TYPE"
echo $SHELL
ps -p $$

REMAINING=`tail -10 launch-report.log | grep -oP "(?<=remaining sets: )(\w+)$" | tail -1`
C=`grep -c "/c" setlist.txt`
D=`grep -c "/d" setlist.txt`

if [ "$TYPE" = "saturation" ]; then
  LAST_FILE=`tail -10 launch-report.log | grep -oP "(?<=saturation/)(\w+\.csv)" | tail -1`
  STARTED=`grep -n $LAST_FILE saturation-master-setlist.txt | awk -F: '{print $1}'`
  FILES=`ps aux | grep -oP "(?<=inputFile saturation\/).+\.csv" | sed ':a;N;$!ba;s/\n/ -- /g'`
  FILES2=`ps aux | grep -oP "[ ].+\.csv"  | sed ':a;N;$!ba;s/\n/ -- /g'`
else
  LAST_FILE=`tail -10 launch-report.log | grep -oP "(?<=data/)(\w+\.csv)" | tail -1`
  STARTED=`grep -n $LAST_FILE main-master-setlist.txt | awk -F: '{print $1}'`
  FILES=`ps aux | grep -oP "(?<=inputFile csv\/).+\.csv" | sed ':a;N;$!ba;s/\n/ -- /g'`
fi

FINISHED=`grep -c " Finished @ " r-report.log`

printf "Already started: %s,\t finished: %s, remaining: %s (datasets: %d, data providers: %d)\n" $STARTED $FINISHED $REMAINING $C $D
echo `uptime` | grep -oP "(load average: .+)$"
printf "Last file started:\t %s\n" $LAST_FILE
printf "Currently processing:\t %s\n" $FILES
echo $FILES2
