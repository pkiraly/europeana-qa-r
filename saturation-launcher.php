<?php
/**
 * before start:
 *   ./prepare.sh saturation
 *
 * launch:
 *   crontab -e
 *   *\/ 1 * * * * cd ~/git/europeana-qa-r && php saturation-launcher.php >> launch-report.log
 *
 * monitoring:
 *   watch './running-status.sh saturation'
 */

define('MAX_THREADS', 7);
define('SET_FILE_NAME', 'setlist.txt');

$Rfile = 'saturation-extended.R';
$endTime = time() + 60;
$i = 1;
while (time() < $endTime) {
  $threads = exec('ps aux | grep "[=]' . $Rfile . '" | wc -l');
  # echo 'threads: ', $threads, "\n";
  if ($threads < MAX_THREADS) {
    if (filesize(SET_FILE_NAME) > 3) {
      launch_threads($threads);
    }
  }
  sleep(2);
}

function launch_threads($running_threads) {
  global $Rfile;

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
      exec('nohup Rscript ' . $Rfile . ' --inputFile ' . $file . ' --drawSaturationGraph T --calculateSaturation T --produceJson T >>r-report.log 2>>r-report.log &');
    }
  }
}
