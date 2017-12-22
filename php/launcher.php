<?php
/**
 * cp master-setlist.txt setlist.txt
 */

define('MAX_THREADS', 6);
define('SET_FILE_NAME', 'setlist.txt');

$endTime = time() + 60;
$i = 1;
while (time() < $endTime) {
  $threads = exec('ps aux | grep "[=]get-histograms-and-stats2.R" | wc -l');
  # echo 'threads: ', $threads, "\n";
  if ($threads < MAX_THREADS) {
    if (filesize(SET_FILE_NAME) > 3) {
      launch_threads($threads);
    }
  }
  sleep(2);
}

function launch_threads($running_threads) {
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
    file_put_contents('setlist.txt', $contents);
    foreach ($files as $file) {
      printf("%s launching set: %s, remaining sets: %d\n", date("Y-m-d H:i:s"), $file, count($lines));
      exec('nohup Rscript get-histograms-and-stats2.R --inputFile ' . $file . ' --drawCompletenessGraph F --drawEntropyGraph F --produceJson T >>r-report.log 2>>r-report.log &');
    }
  }
}
