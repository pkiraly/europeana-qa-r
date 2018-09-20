FILE=$1
VERSION=$2

INPUT_DIR=/projects/pkiraly/data-export/$VERSION/split/saturation
OUTPUT_DIR=/projects/pkiraly/europeana-qa-data/v$VERSION/json
R_DIR=/home/pkiraly/git/europeana-qa-r/R

cd ..
Rscript ${R_DIR}/multilinguality.R --inputFile ${INPUT_DIR}/${FILE}.csv \
  --outputDirectory ${OUTPUT_DIR} \
  --drawSaturationGraph F \
  --drawTopSaturationGraph F \
  --calculateSaturation T \
  --produceJson T

