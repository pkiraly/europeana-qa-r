<?php
/**
 * cp master-setlist.txt setlist.txt
 */

define('MAX_THREADS', 10);
define('SET_FILE_NAME', 'setlist.txt');

$parameters = [
  '--drawCompletenessGraph F',
  '--drawEntropyGraph F',
  '--produceJson T',
  '--calculateExistence F',
  '--calculateCardinalities F',
  '--calculateFrequencyTables F',
  '--calculateBasicStatistics F'
];
$all_parameters = join($parameters, ' ');


$endTime = time() + 60;
$i = 1;
while (time() < $endTime) {
  $threads = exec('ps aux | grep "[=]R/histogram.R" | wc -l');
  // echo 'threads: ', $threads, "\n";
  // echo date('h:i:s', time()), ' ', date('h:i:s', $endTime), "\n";
  if ($threads < MAX_THREADS) {
    if (filesize(SET_FILE_NAME) > 3) {
      launch_threads($threads);
    }
  }
  sleep(1);
}

function launch_threads($running_threads) {
  global $all_parameters;

  if (filesize(SET_FILE_NAME) > 3) {
    $contents = file_get_contents(SET_FILE_NAME);
    $lines = explode("\n", $contents);
    $files = [];
    $slots = MAX_THREADS - $running_threads;
    for ($i = 1; $i <= $slots; $i++) {
      if (count($lines) > 0) {
        $files[] = array_shift($lines);
      }
    }
    printf("Running threads: %d, slots: %d, new files: %d\n", $running_threads, $slots, count($files));
    $contents = join("\n", $lines);
    file_put_contents(SET_FILE_NAME, $contents);
    foreach ($files as $file) {
      printf("%s launching set: %s, remaining sets: %d\n", date("Y-m-d H:i:s"), $file, count($lines));
      // echo 'nohup Rscript R/histogram.R --inputFile ' . $file . ' ' . $all_parameters . ' >>r-report.log 2>>r-report.log &' . "\n";
      exec('nohup Rscript R/histogram.R --inputFile ' . $file . ' ' . $all_parameters . ' >>r-report.log 2>>r-report.log &');
    }
  }
}
