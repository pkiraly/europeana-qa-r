<?php

$fileName = $argv[1];
$in = fopen($fileName, "r");
$out = [];
while (($line = fgets($in)) != false) {
  if (strpos($line, ',') != false) {
    $row = str_getcsv($line);
    $files = array(
      'c' . $row[0],
      'd' . $row[1]
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
  echo $file, ' ', count($lines), "\n";
  file_put_contents('data/' . $file . '.csv', join("", $lines), FILE_APPEND);
}

