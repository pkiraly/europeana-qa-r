<?php
define('MAX_THREADS', 15);
define('SET_FILE_NAME', 'setlist.txt');

for ($i = 0; $i<12; $i++) {
  printf("round %d\n", $i);
  $threads = exec('ps aux | grep "[=]get-histograms-and-stats.R" | wc -l');
  # echo 'threads: ', $threads, "\n";
  if ($threads < MAX_THREADS) {
    if (filesize(SET_FILE_NAME) > 3) {
      launch_threads($threads);
    }
  }
  sleep(5);
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
      printf("%s launching set: %s\n", date("Y-m-d H:i:s"), $set);
      exec('nohup Rscript get-histograms-and-stats.R ' . $set . ' F F F >>r-report.txt 2>>r-report.txt &');
    }
  }
}
