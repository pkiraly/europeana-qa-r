library("optparse")

option_list = list(
  make_option(
    c("-f", "--inputFile"), type="character", default=NULL, metavar="character",
    help="input file (.csv)"
  ),
  make_option(
    c("-c", "--drawCompletenessGraph"), type="logical", default=TRUE, metavar="logical",
    help="draw completeness graph"
  ),
  make_option(
    c("-e", "--drawEntropyGraph"), type="logical", default=FALSE, metavar="logical",
    help="draw entropy graph"
  ),
  make_option(
    c("-x", "--calculateExistence"), type="logical", default=TRUE, metavar="logical",
    help="calculate frequencies"
  ),
  make_option(
    c("-r", "--calculateCardinalities"), type="logical", default=TRUE, metavar="logical",
    help="calculate cardinalities"
  ),
  make_option(
    c("-t", "--calculateFrequencyTables"), type="logical", default=TRUE, metavar="logical",
    help="calculate frequency tables"
  ),
  make_option(
    c("-j", "--produceJson"), type="logical", default=TRUE, metavar="logical",
    help="produce JSON files"
  )
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$inputFile)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}
