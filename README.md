# europeana-qa-r
Statistical analyses in R language for [Europeana Metadata Quality Assurance](http://pkiraly.github.io)

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
