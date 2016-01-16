<?php

$graphs = array(
  'total' => array('label' => 'Completeness', 'fields' => array("edm:ProvidedCHO/@about", "Proxy/dc:title", "Proxy/dcterms:alternative",
  	"Proxy/dc:description", "Proxy/dc:creator", "Proxy/dc:publisher", "Proxy/dc:contributor", "Proxy/dc:type", "Proxy/dc:identifier",
  	"Proxy/dc:language", "Proxy/dc:coverage", "Proxy/dcterms:temporal", "Proxy/dcterms:spatial", "Proxy/dc:subject", "Proxy/dc:date",
  	"Proxy/dcterms:created", "Proxy/dcterms:issued", "Proxy/dcterms:extent", "Proxy/dcterms:medium", "Proxy/dcterms:provenance",
  	"Proxy/dcterms:hasPart", "Proxy/dcterms:isPartOf", "Proxy/dc:format", "Proxy/dc:source", "Proxy/dc:rights", "Proxy/dc:relation",
  	"Proxy/edm:isNextInSequence", "Proxy/edm:type", "Proxy/edm:rights", "Aggregation/edm:rights", "Aggregation/edm:provider",
  	"Aggregation/edm:dataProvider", "Aggregation/edm:isShownAt", "Aggregation/edm:isShownBy", "Aggregation/edm:object", "Aggregation/edm:hasView")),
  'mandatory' => array('label' => 'Mandatory elements', 'fields' => array("edm:ProvidedCHO/@about", "Proxy/dc:title", "Proxy/dc:description",
  	"Proxy/dc:type", "Proxy/dc:coverage", "Proxy/dcterms:spatial", "Proxy/dc:subject", "Proxy/edm:rights", "Aggregation/edm:rights",
  	"Aggregation/edm:provider", "Aggregation/edm:dataProvider", "Aggregation/edm:isShownAt", "Aggregation/edm:isShownBy")),
  'descriptiveness' => array('label' => 'Descriptiveness', 'fields' => array("Proxy/dc:title", "Proxy/dcterms:alternative", "Proxy/dc:description",
  	"Proxy/dc:creator", "Proxy/dc:language", "Proxy/dc:subject", "Proxy/dcterms:extent", "Proxy/dcterms:medium", "Proxy/dcterms:provenance",
  	"Proxy/dc:format", "Proxy/dc:source")),
  'searchability' => array('label' => 'Searchability', 'fields' => array("Proxy/dc:title", "Proxy/dcterms:alternative", "Proxy/dc:description", 
  	"Proxy/dc:creator", "Proxy/dc:publisher", "Proxy/dc:contributor", "Proxy/dc:type", "Proxy/dc:coverage", "Proxy/dcterms:temporal", 
  	"Proxy/dcterms:spatial", "Proxy/dc:subject", "Proxy/dcterms:hasPart", "Proxy/dcterms:isPartOf", "Proxy/dc:relation", "Proxy/edm:isNextInSequence",
  	"Proxy/edm:type", "Aggregation/edm:provider", "Aggregation/edm:dataProvider")),
  'contextualization' => array('label' => 'Contextualization', 'fields' => array("Proxy/dc:description", "Proxy/dc:creator", "Proxy/dc:type",
  	"Proxy/dc:coverage", "Proxy/dcterms:temporal", "Proxy/dcterms:spatial", "Proxy/dc:subject", "Proxy/dcterms:hasPart", "Proxy/dcterms:isPartOf",
  	"Proxy/dc:relation", "Proxy/edm:isNextInSequence")),
  'identification' => array('label' => 'Identification', 'fields' => array("Proxy/dc:title", "Proxy/dcterms:alternative", "Proxy/dc:description",
  	"Proxy/dc:type", "Proxy/dc:identifier", "Proxy/dc:date", "Proxy/dcterms:created", "Proxy/dcterms:issued", "Aggregation/edm:provider",
  	"Aggregation/edm:dataProvider")),
  'browsing' => array('label' => 'Browsing', 'fields' => array("Proxy/dc:creator", "Proxy/dc:type", "Proxy/dc:coverage", "Proxy/dcterms:temporal",
  	"Proxy/dcterms:spatial", "Proxy/dc:date", "Proxy/dcterms:hasPart", "Proxy/dcterms:isPartOf", "Proxy/dc:relation", "Proxy/edm:isNextInSequence",
  	"Proxy/edm:type", "Aggregation/edm:isShownAt", "Aggregation/edm:isShownBy", "Aggregation/edm:hasView")),
  'viewing' => array('label' => 'Viewing', 'fields' => array("Aggregation/edm:isShownAt", "Aggregation/edm:isShownBy", "Aggregation/edm:object",
  	"Aggregation/edm:hasView")),
  'reusability' => array('label' => 'Re-usability', 'fields' => array("Proxy/dc:publisher", "Proxy/dc:date", "Proxy/dcterms:created",
  	"Proxy/dcterms:issued", "Proxy/dcterms:extent", "Proxy/dcterms:medium", "Proxy/dc:format", "Proxy/dc:rights", "Proxy/edm:rights",
  	"Aggregation/edm:rights", "Aggregation/edm:isShownBy", "Aggregation/edm:object")),
  'multilinguality' => array('label' => 'Multilinguality', 'fields' => array("Proxy/dc:title", "Proxy/dcterms:alternative", "Proxy/dc:description",
  	"Proxy/dc:language", "Proxy/dc:subject"))
);

$title = 'Metadata Quality Assurance Framework';
$collectionId = $argv[1];
$id = strstr($collectionId, '_', true);

$stats = json_decode(file_get_contents("$id.json"));
$assocStat = array();
foreach ($stats as $obj) {
  $key = $obj->_row;
  unset($obj->_row);
  $assocStat[$key] = $obj;
}

$content = sprintf("<html><head><title>%s | %s</title><style>* {font-family: arial;}h2 {margin-top: 40px;} h1, h2, h3 {color: maroon}</style></head><body>\n", $collectionId, $title);
$content .= sprintf("<h1>%s</h1>\n", $collectionId);
foreach ($graphs as $name => $function) {
  $content .= sprintf("<h2>%s</h2>\n", $function['label']);

  $content .= "<h3>Fields covered</h3>\n";
  $content .= sprintf("<p>%s</p>\n", join($function['fields'], ", "));

  $content .= "<h3>Basic statistics</h3>\n";
  $content .= "<table>\n";
  foreach (get_object_vars($assocStat[$name]) as $key => $value) {
    if ($key == 'recMin') {
      $content .= sprintf("<tr><td>%s</td><td><a href=\"completenessTable.php?id=%s\">%s</a></td>\n", 'A record with minimal score', $value, $value);
    } else if ($key == 'recMax') {
      $content .= sprintf("<tr><td>%s</td><td><a href=\"completenessTable.php?id=%s\">%s</a></td>\n", 'A record with maximal score', $value, $value);
    } else {
      $content .= sprintf("<tr><td>%s</td><td>%s</td>\n", $key, $value);
    }
  }
  $content .= "</table>\n";

  $content .= "<h3>Graphs</h3>\n";
  $content .= sprintf("<img src=\"img/%s/%s-%s.png\" />\n", $id, $id, $name);
}
$content .= "<p><a href=\"index.html\">List of collections</a></p>\n";
$content .= "</body></html>\n";

file_put_contents($id . '.html', $content);