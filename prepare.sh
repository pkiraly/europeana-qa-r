TYPE=$1
rm -f r-report.log
rm -f launch-report.log

if [ "$TYPE" = "multilinguality" ]; then
  cp multilinguality-master-setlist.txt setlist.txt
elif [ "$TYPE" = "uniqueness" ]; then
  cp uniqueness-master-setlist.txt setlist.txt
else
  cp completeness-master-setlist.txt setlist.txt
fi
