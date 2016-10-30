TYPE=$1
REMAINING=`tail -10 launch-report.log | grep -oP "(?<=remaining sets: )(\w+)$" | tail -1`
C=`grep -c "/c" setlist.txt`
D=`grep -c "/d" setlist.txt`

if [ "$TYPE" = "saturation" ]; then
  LAST_FILE=`tail -10 launch-report.log | grep -oP "(?<=saturation/)(\w+\.csv)" | tail -1`
  STARTED=`grep -n $LAST_FILE saturation-master-setlist.txt | awk -F: '{print $1}'`
else
  LAST_FILE=`tail -10 launch-report.log | grep -oP "(?<=data/)(\w+\.csv)" | tail -1`
  STARTED=`grep -n $LAST_FILE main-master-setlist.txt | awk -F: '{print $1}'`
fi

FINISHED=`grep -c " Finished @ " r-report.log`

echo "Already started: $STARTED, finished: $FINISHED, remaining: $REMAINING (datasets: $C, data providers: $D)"
echo `uptime` | grep -oP "(load average: .+)$"
