<?php
/**
 * cp master-setlist.txt setlist.txt
 */

define('MAX_THREADS', 7);
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
    $sets = [];
    $slots = MAX_THREADS - $running_threads;
    for ($i = 1; $i <= $slots; $i++) {
      if (count($lines) > 0) {
        $sets[] = array_shift($lines);
      }
    }
    printf("Running threads: %d, slots: %d, new sets: %d\n", $running_threads, $slots, count($sets));
    $contents = join("\n", $lines);
    file_put_contents('setlist.txt', $contents);
    foreach ($sets as $set) {
      printf("%s launching set: %s, remaining sets: %d\n", date("Y-m-d H:i:s"), $set, count($lines));
      exec('nohup Rscript get-histograms-and-stats2.R ' . $set . ' T F T >>r-report.txt 2>>r-report.txt &');
    }
  }
}
