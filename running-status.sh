#!/bin/bash
#
# Creates a status report about the running R processes
# usage:
# $0
# $0 saturation

TYPE=$1
echo "TYPE is $TYPE"

REMAINING=`tail -10 launch-report.log | grep -oP "(?<=remaining sets: )(\w+)$" | tail -1`
C=`grep -c "/c" setlist.txt`
D=`grep -c "/d" setlist.txt`

if [ "$TYPE" = "saturation" ]; then
  LAST_FILE=`tail -100 launch-report.log | grep -oP "(?<=saturation/)(\w+\.csv)" | tail -1`
  STARTED=`grep -n $LAST_FILE saturation-master-setlist.txt | awk -F: '{print $1}'`
  FILES=`ps aux | grep -oP "(?<=inputFile \/projects\/pkiraly\/2018-03-23\/split\/saturation\/).+\.csv" | sed ':a;N;$!ba;s/\n/--/g'`
else
  LAST_FILE=`tail -10 launch-report.log | grep -oP "(?<=data/)(\w+\.csv)" | tail -1`
  if [ "$LAST_FILE" != "" ]; then
    STARTED=`grep -n $LAST_FILE main-master-setlist.txt | awk -F: '{print $1}'`
  else
    STARTED=`wc -l main-master-setlist.txt`
  fi
  # FILES=`ps aux | grep -U -oP "(?<=inputFile data\/).+\.csv" | grep -oP "^(.+)(?=\.)" | sed ':a;N;$!ba;s/\n/ -- /g'`
  # FILES=`ps aux | grep -U -oP "(?<=inputFile data\/).+\.csv" | grep -oP "^(.+)(?=\.)" | xargs`
  FILES=`ps aux | grep -U -P "(?<=inputFile data\/).+\.csv" | awk '{printf "%20s...(%s) ", $17, $10}' | tr ' ' '_' # | sed 's/)_/) /'`
fi

FINISHED=`grep -c " Finished @ " r-report.log`

printf "Already started: %s,\t finished: %s, remaining: %s (datasets: %d, data providers: %d)\n" $STARTED $FINISHED $REMAINING $C $D
echo `uptime` | grep -oP "(load average: .+)$"
printf "Last file started:\t %s\n" $LAST_FILE
printf "Currently processing:\t %s\n" $FILES
