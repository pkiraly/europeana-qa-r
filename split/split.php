<?php

$fileName = $argv[1];
$in = fopen($fileName, "r");
$out = [];
$ln = 1;
while (($line = fgets($in)) != false) {
  if (strpos($line, ',') != false) {
    if ($ln++ % 100000 == 0) {
      echo $ln, ' ';
    }
    $row = str_getcsv($line);
    $files = array(
      'c' . $row[1],
      'd' . $row[2]
    );
    foreach ($files as $file) {
      if (!isset($out[$file])) {
        $out[$file] = [];
      }
      $out[$file][] = $line;
      if (count($out[$file]) == 500) {
        file_put_contents('data/' . $file . '.csv', join("", $out[$file]), FILE_APPEND);
        unset($out[$file]);
      }
    }
  }
}
fclose($in);

foreach ($out as $file => $lines) {
  file_put_contents('data/' . $file . '.csv', join("", $lines), FILE_APPEND);
}
