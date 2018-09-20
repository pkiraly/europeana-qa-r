<?php
require_once('options.php');

$opts = make_options([
  'f:' => 'fileName:',
  'd:' => 'outputDir:'
]);
$options = getopt($opts[0], $opts[1]);

$start = microtime(TRUE);
$fileName = $options['fileName'];
if (!file_exists($fileName))
  die("The input file ($fileName) is not existing.\n");

// '/projects/pkiraly/2018-03-23/split/multilinguality';
$dir = $options['outputDir'];
if (!file_exists($dir))
  die("The output dir ($dir) is not existing.\n");

$start = microtime(TRUE);
$in = fopen($fileName, "r");
$out = [];
// $intersections = ['c' => [], 'd' => []];
$ln = 1;
while (($line = fgets($in)) != false) {
  if (strpos($line, ',') != false) {
    if ($ln++ % 100000 == 0) {
      echo number_format($ln, 0, '.', '.'), ' ';
    }
    $row = str_getcsv($line);
    $c = $row[1];
    $d = $row[2];
    $files = [
      sprintf('c%s', $c),
      sprintf('d%s', $d),
      sprintf('cd-%d-%d', $c, $d),
    ];
    foreach ($files as $file) {
      if (!isset($out[$file])) {
        $out[$file] = [];
      }
      $out[$file][] = $line;
      if (count($out[$file]) == 500) {
        file_put_contents($dir . '/' . $file . '.csv', join("", $out[$file]), FILE_APPEND);
        unset($out[$file]);
      }
    }
  }
}
fclose($in);

foreach ($out as $file => $lines) {
  file_put_contents($dir . '/' . $file . '.csv', join("", $lines), FILE_APPEND);
}

$duration = microtime(TRUE) - $start;
echo 'DONE in ', gmdate("H:i:s", (int)$duration), "\n";
