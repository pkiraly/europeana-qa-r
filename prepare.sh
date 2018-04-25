TYPE=$1
rm -f r-report.log
rm -f launch-report.log

if [ "$TYPE" = "saturation" ]; then
  cp saturation-master-setlist.txt setlist.txt
elif [ "$TYPE" = "uniqueness" ]; then
  cp uniqueness-master-setlist.txt setlist.txt
else
  cp main-master-setlist.txt setlist.txt
fi
