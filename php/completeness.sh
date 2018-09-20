FILE=$1
VERSION=$2

INPUT_DIR=/projects/pkiraly/data-export/$VERSION/split/completeness
OUTPUT_DIR=/projects/pkiraly/europeana-qa-data/v$VERSION/json
R_DIR=/home/pkiraly/git/europeana-qa-r/R

cd ..
Rscript ${R_DIR}/completeness.R --inputFile ${INPUT_DIR}/${FILE}.csv --outputDirectory ${OUTPUT_DIR} \
  --drawCompletenessGraph F \
  --drawEntropyGraph F \
  --produceJson T \
  --calculateExistence T \
  --calculateCardinalities T \
  --calculateFrequencyTables T \
  --calculateBasicStatistics T

cd php
