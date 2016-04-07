<?php

$in = fopen("result6.csv", "r");
// $out = array();
while (($line = fgets($in)) != false) {
  if (strpos($line, ',') != false) {
    $row = str_getcsv($line);
    $files = array('c' . $row[0], 'd' . $row[1]);
    // $files = array('d' . $row[1]);
    foreach ($files as $file) {
      //if (!isset($out[$file])) {
      //  $out[$file] = fopen('data/' . $file . '.csv', 'a+');
      // }
      file_put_contents('data/' . $file . '.csv', $line, FILE_APPEND);
      // fwrite($out[$file], $line);
    }
  }
}

/*
foreach ($out as $file => $outf) {
  fclose($outf);
}
*/

fclose($in);
