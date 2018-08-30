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

// '/projects/pkiraly/2018-03-23/split/completeness';
$dir = $options['outputDir'];
if (!file_exists($dir))
  die("The output dir ($dir) is not existing.\n");

$in = fopen($fileName, "r");
$out = [];
$intersections = ['c' => [], 'd' => []];
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

    if (!isset($intersections['c'][$c][$d])) {
      $intersections['c'][$c][$d] = ['count' => 0, 'file' => $files[2]];
    }
    $intersections['c'][$c][$d]['count']++;
    if (!isset($intersections['d'][$d][$c])) {
      $intersections['d'][$d][$c] = ['count' => 0, 'file' => $files[2]];
    }
    $intersections['d'][$d][$c]['count']++;

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

file_put_contents($dir . '/../intersections.json', json_encode($intersections));

$duration = microtime(TRUE) - $start;
echo 'DONE in ', gmdate("H:i:s", (int)$duration), "\n";
