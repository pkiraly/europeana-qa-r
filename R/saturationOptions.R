library("optparse")

option_list = list(
  make_option(
    c("-f", "--inputFile"), type="character", default=NULL, metavar="character",
    help="input file (.csv)"
  ),
  make_option(
    c("-d", "--outputDirectory"), type="character", default=NULL, metavar="character",
    help="base output directory"
  ),
  make_option(
    c("-s", "--calculateSaturation"), type="logical", default=TRUE, metavar="logical",
    help="calculate saturation"
  ),
  make_option(
    c("-a", "--drawSaturationGraph"), type="logical", default=TRUE, metavar="logical",
    help="draw saturation graph"
  ),
  make_option(
    c("-t", "--drawTopSaturationGraph"), type="logical", default=TRUE, metavar="logical",
    help="draw saturation graph"
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
