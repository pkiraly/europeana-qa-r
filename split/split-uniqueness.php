<?php

$start = microtime(TRUE);
$fileName = $argv[1];
$in = fopen($fileName, "r");
$dir = '/projects/pkiraly/2018-03-23/split/uniqueness';
$out = [];
// $intersections = ['c' => [], 'd' => []];
$ln = 1;
while (($line = fgets($in)) != false) {
  if (strpos($line, ',') != false) {
    if ($ln++ % 100000 == 0) {
      echo number_format($ln), ' ';
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
