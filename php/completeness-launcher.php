<?php
/**
 * cp master-setlist.txt setlist.txt
 */

define('OUTPUT_DIRECTORY', '/projects/pkiraly/europeana-qa-data/v2018-03/json');
define('MAX_THREADS', 10);
define('SET_FILE_NAME', 'setlist.txt');
define('CMD_TEMPLATE', 'nohup Rscript R/completeness.R --inputFile %s %s >>r-report.log 2>>r-report.log &');

$parameters = [
  '--outputDirectory ' . OUTPUT_DIRECTORY,
  '--drawCompletenessGraph F',
  '--drawEntropyGraph F',
  '--produceJson T',
  '--calculateExistence T',
  '--calculateCardinalities T',
  '--calculateFrequencyTables T',
  '--calculateBasicStatistics T'
];
$all_parameters = join($parameters, ' ');

$endTime = time() + 60;
$i = 1;
while (time() < $endTime) {
  $threads = exec('ps aux | grep "[=]R/completeness.R" | wc -l');
  # echo 'threads: ', $threads, "\n";
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
    file_put_contents('setlist.txt', $contents);
    foreach ($files as $file) {
      printf("%s launching set: %s, remaining sets: %d\n", date("Y-m-d H:i:s"), $file, count($lines));
      $cmd = sprintf(CMD_TEMPLATE, $file, $all_parameters);
      // echo $cmd, "\n";
      exec($cmd);
    }
  }
}
