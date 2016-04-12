# europeana-qa-r
Statistical analyses in R language for Europeana. Part of [Metadata Quality Assurance Framework](http://pkiraly.github.io) project.

This module runs different statistical analyses on the output of the [europeana-qa-spark](https://github.com/pkiraly/europeana-qa-spark/) module, which is a big CSV file (46M+ rows, 54 columns). The three first rows of the file is dataset identifier, data provider identifier and record identifier. Because we would ike to display statistics on subsets of the whole recordset, we split the big file into smaller chunks collecting all records belonging to a particular dataset (where the file name pattern is 'c' + id + '.csv') and a particular data provider ('d' + id + '.csv'). Later we might introduce other subsets as well.

The main file is `get-histograms-and-stats.R` the rest of the project are utilities to serve this file or the workflow. This file process one recordset, so to process all, you should create a batch file, which list all the individual datasets.

Run:

    nohup ./run-all-r.sh &

where `run-all.sh` contains lines such as

    Rscript get-histograms-and-stats.R data/d1000.csv
    Rscript get-histograms-and-stats.R data/d1001.csv
    Rscript get-histograms-and-stats.R data/d1002.csv
    Rscript get-histograms-and-stats.R data/d1003.csv
    Rscript get-histograms-and-stats.R data/d1004.csv
    ...

Follow where is the process standing

    grep -n `tail nohup.out | grep -oP "(?<=data/)(\w+\.csv)" | tail -1` run-all-r.sh

The process creates the following files:

 * `json/d1000.json` -- the main descriptive statistics for the completeness and uniqueness scores
 * `json/d1000.hist.json` -- the histograms of the completeness and uniqueness scores
 * `json/d1000.count.json` -- contains the number of recors of the set
 * `json/d1000.freq.json` -- the field frequencies
 * `img/d1000/d1000-[metric].png` -- graphs for exploratory data analyses (metric is `browsing`, `descriptiveness`, `mandatory`, `reusability`, `total`, `contextualization`, `identification`, `multilinguality`, `searchability`, `viewing`)

