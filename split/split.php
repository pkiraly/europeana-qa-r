<?php

$fileName = $argv[1];
$in = fopen($fileName, "r");
$dir = '/projects/pkiraly/2018-03-23/split/completeness';
$out = [];
$intersections = ['c' => [], 'd' => []];
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
      'c' . $c,
      'd' . $d,
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
