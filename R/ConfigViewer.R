#' ConfigViewer
#'
#' Interactive method for simultaneously viewing single-snp 
#' manhattan plots alongside FINEMAP results, with graphical
#' representation of the most probable causal SNP configurations.
#' You can plot one to three manhattan plots, simultaneously
#' viewing single-snp, FINEMAP, and conditional results.
#' For viewing manhattan plots with color-coded correlation to the top  
#' SNP, you may upload either the .z file and .ld file used for 
#' FINEMAPing, or a correlations data frame with columns 'rsid' and 'corr'.
#' For large LD matrices, creating this correlations data frame will
#' speed up plot generation time significantly.
#'
#' @import htmlwidgets
#'
#' @export
#'
#'
#' @param pvalues An obligatory data frame containing the column names 'rsid', 
#'		'position', and 'pvalue'. Assuming the results are from a single-snp test.
#'	 	Other columns may be included, but will be ignored in plotting.
#' @param snp_probs An optional data frame containing the column names 'rsid',
#'		'snp_prob', and 'snp_log10bf'. Corresponds to the .snp
#'		output file from FINEMAP.
#' @param cond_pvalues An optional data frame containing the column names 'rsid',
#'		and 'cond_pvalue'. Other columns may be included, but will be 
#'		ignored in plotting- meaning SNP positions come from the pvalues
#'		object. Assuming the results are from a conditional test.
#' @param config_probs An optional data frame containing the column names 
#'		'rank', 'config', 'config_prob', and 'config_log10bf'. Corresponds to 
#'		the .config output file from FINEMAP.
#' @param correlations An optional data frame containing the column names
#'		'rsid' and 'corr', giving each rsid's correlation to the top SNP. For 
#'		datasets with a large LD correlation matrix, creating this object
#' 		speeds up plot generation time significantly. Provide either correlations object,
#' 		or z_file and ld_file together.
#' @param z_file An optional data frame containing the column name 'rsid'. 
#' 		Provide either correlations object, or z_file and ld_file together.
#' 		Corresponds to the .z file input to FINEMAP.
#' @param ld_file An optional correlations matrix of SNPs. Number of rows and
#'		columns must match the z_file number of rows. Corresponds to the .ld
#'		file input to FINEMAP.
#' @param pval_threshold Threshold for plot color coding and threshold line
#'		placement of conventional manhattan plots. Default 1e-6.
#' @param snp_bf_threshold Threshold for plot color coding and threshold line 
#'		placement of FINEMAP manhattan plot. Default 100, corresponding to a 
#'		log10BF of 2.
#' @param topconfigs Number of top configurations to display. 
#' @param subsample Logical value asking whether, for datasets containing more
#'		than 5000 variants, the nonsignificant (in p value and bayes factor)
#'		variants should be subsampled. Speeds up plot responsiveness significantly
#' 		for large datasets.
#' @param labels Logical values asking whether plot labels (FINEMAP, single-snp,
#'		conditional test) should be applied. Default: on only when conditional test
#' 		results included.
#' @param width Manual setting of plot width- will disable dynamic resizing.
#' @param height Manual setting of plot height- will disable dynamic resizing.
#'


ConfigViewer <- function(pvalues, snp_probs = NULL, cond_pvalues = NULL, config_probs = NULL, 
							correlations = NULL, z_file = NULL, ld_file = NULL,
							pval_threshold = 1e-6, snp_bf_threshold = 100, topconfigs = 5,
							subsample = TRUE, labels = FALSE,
							 width = NULL, height = NULL) {
    
	elementId = NULL
	labels = labels
	
	# Run through data input checks to make sure everything is OK

	# Check that data frames are data frames
	if( any(class(pvalues) != "data.frame")) {
		stop("'pvalues' must be a data frame" )
	}
	if( !is.null(snp_probs) & any(class(snp_probs) != "data.frame")){
		stop("'snp_probs' must be a data frame")
	}
	if( !is.null(cond_pvalues) & any(class(cond_pvalues) != "data.frame")){
		stop("'cond_pvalues' must be a data frame")
	}
	if( !is.null(config_probs) & any(class(config_probs) != "data.frame")){
		stop("'config_prob' must be a data frame")
	}
	if( !is.null(correlations) & any(class(correlations) != "data.frame")){
		stop("'correlations' must be a data frame")
	}
	if( !is.null(z_file) & any(class(z_file) != "data.frame")){
		stop("'z_file' must be a data frame")
	}
	# Check that numeric inputs are actually numeric
	if( !is.numeric(pval_threshold)){
		stop("pval_threshold' must be numeric")
	}
	if( !is.numeric(snp_bf_threshold)){
		stop("'snp_bf_threshold' must be numeric")
	}
	if( !is.numeric(topconfigs)){
		stop("'topconfigs' must be numeric")
	}

	# Check that ld file is a matrix
	if( !is.null(ld_file) & class(ld_file) != "matrix"){
		stop("ld_file must be a matrix")
	}

	# Check that correlations data frame and z/ld files are not given at the same time
	if( ( !is.null(correlations) & !is.null(z_file) ) |  ( !is.null(correlations) & !is.null(ld_file) )){
		stop("Give either correlations data frame or z_file and ld_file, not both")
	}
	# Check that BOTH z and ld files are given, and that the dimensions match (nrow to nrow/ncol)
	if( ( !is.null(z_file) & is.null(ld_file) ) | ( is.null(z_file) & !is.null(ld_file) )  ){
		stop("Must give both z_file and ld_file, or give neither")
	}
	if(!is.null(z_file) & !is.null(ld_file)){
		if(nrow(z_file) != nrow(ld_file)){
			stop("Number of rows in z_file must match number of rows in ld_file")
		}
		if(nrow(z_file) != ncol(ld_file)){
			stop("Number of rows in z_file must match number of columns in ld_file. Are you sure it's a correlation matrix?")
		}
	}

	# Check data frames for correct column names
	if( all(c("rsid", "position", "pvalue") %in% colnames(pvalues)) == FALSE ){
		stop('pvalues data frame must contain colnames "rsid", "position", and "pvalue"')
	}
	if( !is.null(snp_probs) & all(c("rsid","snp_prob","snp_log10bf") %in% colnames(snp_probs)) == FALSE){
		stop('snp_probs data frame must contain colnames "rsid", "snp_prob", and "snp_log10bf"')
	}
	if( !is.null(cond_pvalues) & all(c("rsid", "cond_pvalue") %in% colnames(cond_pvalues)) == FALSE ){
		stop('cond_pvalues data frame must contain colnames "rsid", and "cond_pvalue"')
	}
	if( !is.null(config_probs) & all(c("rank", "config", "config_prob", "config_log10bf") %in% colnames(config_probs)) == FALSE){
		stop('config_probs data frame must contain colnames "rank", "config", "config_prob", and "config_log10bf"' )
	}
	if( !is.null(z_file) & all(c("rsid") %in% colnames(z_file)) == FALSE){
		stop('z_file data frame must contain colnames "rsid"')
	}
	if( !is.null(correlations) & all(c("rsid", "corr") %in% colnames(correlations)) == FALSE){
		stop('correlations data frame must contain colnames "rsid" and "corr"')
	}

	# Check that rsids are not duplicated
	if( any(duplicated(pvalues$rsid)) == TRUE){
		stop("Duplicate rsids in pvalue data frame- no duplicate rsids allowed.")
	}
	if( !is.null(snp_probs) & any(duplicated(snp_probs$rsid)) == TRUE){
		stop("Duplicate rsids in snp_probs data frame- no duplicate rsids allowed.")
	}
	if( !is.null(cond_pvalues) & any(duplicated(cond_pvalues$rsid)) == TRUE){
		stop("Duplicate rsids in cond_pvalues data frame- no duplicate rsids allowed.")
	}
	if( !is.null(z_file) & any(duplicated(z_file$rsid)) == TRUE){
		stop("Duplicated rsids in z_file data frame- no duplicate rsids allowed.")
	}
	if( !is.null(correlations) & any(duplicated(correlations$rsid)) == TRUE){
		stop("Duplicated rsids in correlations data frame- no duplicate rsids allowed.")
	}
	### Process data for to feed to js code ###
	# Create vector of input dataset names and dataset info needed later
	datasetnames <- c()
	thresholds <- c()
	labelprefix <- c()
	yaxislabels <- c()
	plotlabels <- c()
	
	# Find which rsids are in all given datasets
	rsids <- pvalues$rsid # rsids in pvalue dataset
	
	if(!is.null(snp_probs)){
		datasetnames <- c(datasetnames, "logBF")
		thresholds <- c(thresholds, log10(snp_bf_threshold))
		labelprefix <- c(labelprefix, 'SNP Prob: ')
		yaxislabels <- c(yaxislabels, 'log10 (Bayes factor)')
		plotlabels <- c(plotlabels, "FINEMAP")
		
		# 	Replace all snp_probs = 0 with 1e-6
		snp_probs$snp_prob[snp_probs$snp_prob == 0] <- 1e-6
		
		rsids <- rsids[which(rsids %in% snp_probs$rsid)] #rsids in pvalue and snp_probs datasets
	}
	
	# Define these objects here, so that log10BF plots is on top if it exists
	datasetnames <- c(datasetnames,"logpval")
	thresholds <- c(thresholds, -log10(pval_threshold))
	labelprefix <- c(labelprefix, 'pvalue: ')
	yaxislabels <- c(yaxislabels, '-log10 (pvalue)')
	plotlabels <- c(plotlabels, "SINGLE-SNP TEST")
	
	if(!is.null(cond_pvalues)){
		datasetnames <- c(datasetnames, "condlogpval")
		thresholds <- c(thresholds, -log10(pval_threshold))
		labelprefix <- c(labelprefix, 'pvalue: ')
		yaxislabels <- c(yaxislabels, '-log10 (pvalue)')
		plotlabels <- c(plotlabels, 'CONDITIONAL TEST')
		
		rsids <- rsids[which(rsids %in% cond_pvalues$rsid)] #rsids in pvalue/cond, and snp_probs if exists
	}
	
	if(length(rsids) == 0){
		stop("No matching rsids between given datasets")
	}
	
	# Truncate datasets with this rsid list
	pvalues <- pvalues[which(pvalues$rsid %in% rsids), ]
	
	if(!is.null(snp_probs)){
		snp_probs <- snp_probs[which(snp_probs$rsid %in% rsids), ]
	}
	if(!is.null(cond_pvalues)){
		cond_pvalues <- cond_pvalues[which(cond_pvalues$rsid %in% rsids), ]
	}
	
	# Merge datasets to a single object, to ensure order of all data is correct
	merged <- pvalues[ , c("rsid", "position","pvalue")]
	
	if(!is.null(snp_probs)){
		merged <- merge(merged, snp_probs[,c("rsid", "snp_prob", "snp_log10bf")])
	}
	if(!is.null(cond_pvalues)){
		merged <- merge(merged, cond_pvalues[,c("rsid", "cond_pvalue")])
	}
	
	# Check that all rsids are present in correlation file, truncate correlation file, and merge
	if( !is.null(correlations)){
		corrdata <- "yes"
		if( all(rsids %in% correlations$rsid) == FALSE ){
			stop("Missing correlation information for some rsids")
		}
		
		correlations <- correlations[which(rsids %in% correlations$rsid), ]
		merged <- merge(merged, correlations[,c("rsid","corr")])
	}
	# Check that all these rsids are present in zfile, extract top snp correlation from ld file, truncate file, and merge
	if( !is.null(z_file)){
		corrdata <- "yes"
		
		if( all(rsids %in% z_file$rsid) == FALSE){
			stop("Missing correlation information for some rsids")
		}
		
		topsnp <- pvalues$rsid[which.min(pvalues$pvalue)]
		z_file$corr <- ld_file[which(z_file$rsid == topsnp), ]
		
		z_file <- z_file[which(z_file$rsid %in% rsids), ]
		
		merged <- merge(merged, z_file[,c("rsid","corr")])
	
	}
	
	# Check that all the rsids in the top x configs are present in rsids vector
	if(!is.null(config_probs)){
		config_probs <- config_probs[1:topconfigs,]
		
		# Extract top config probabilities
		rank_config_probs <- config_probs$config_prob
		
		config_probs$config <- as.character(config_probs$config)
		configslist <- strsplit(config_probs$config, ",")
		
		configsvector <- unique(unlist(configslist))
		
		if(any(configsvector %in% merged$rsid) == FALSE){
			stop("Top configs contain rsids absent from a given dataset.")
		}
		
		merged$ranks <- "notintopconfigs"
		
		for(i in 1:length(configslist)){
			for(j in 1:length(configslist[[i]]) ){
				rank <- paste("rank", i, sep = "")
				snp <- configslist[[i]][j]
				
				if( merged$ranks[which(merged$rsid %in% snp)] != "notintopconfigs"){
					merged$ranks[which(merged$rsid %in% snp)] <- paste(merged$ranks[which(merged$rsid %in% snp)], rank)
				} else {		
					merged$ranks[which(merged$rsid %in% snp)] <- rank
				}
			}
		}
		
	}
	
	# Create color vectors based on given thresholds
	merged$pvalcolors <- NA
	merged$pvalcolors[which(merged$pvalue <= pval_threshold)] <- "deepskyblue"
	merged$pvalcolors[which(merged$pvalue > pval_threshold)] <- "navy"
	
	if(!is.null(snp_probs)){
		merged$BFcolors <- NA
		merged$BFcolors[which(merged$snp_log10bf >= log10(snp_bf_threshold))] <- "orange"
		merged$BFcolors[which(merged$snp_log10bf < log10(snp_bf_threshold))] <- "navy"	
	}
	
	# Create correlations colors
	if(!is.null(z_file) | !is.null(correlations)){
		merged$corrcolors <- NA
		
		merged$corrcolors[which(abs(merged$corr) >= 0.8)] <- "firebrick"
		merged$corrcolors[which(abs(merged$corr) >= 0.6 & abs(merged$corr) <0.8)] <- "orange"
		merged$corrcolors[which(abs(merged$corr) >= 0.4 & abs(merged$corr) <0.6)] <- "limegreen"
		merged$corrcolors[which(abs(merged$corr) >= 0.2 & abs(merged$corr) <0.4)] <- "deepskyblue"
		merged$corrcolors[which(abs(merged$corr) < 0.2)] <- "navy"
	}
	
	print(paste("Number of variants with complete information:", length(merged$rsid)))
	
	# If subsample = TRUE and total number of variants is above 2000, remove a percentage of nonsignificant rsids
	if(subsample == TRUE & nrow(merged) > 5000){
		nonsig_rsids <- merged$rsid[which(merged$pvalue > 0.05 & merged$snp_prob < 1e-5)]
		remove_rsids <- sample(nonsig_rsids, round(length(nonsig_rsids)*0.99), replace = FALSE)	
		merged <- merged[ -which(merged$rsid %in% remove_rsids), ]
		
		print(paste("Number of variants plotted (with subsampling of nonsigificant variants):", nrow(merged)))
	}
	
	# Take out all colons from rsid names
	merged$rsid <- gsub(":","-", merged$rsid)
	
	# List specific datasets to feed to JSON
	# allplots = data that are applied to all of the plots
	# configdata = specific data about top x configs
	# plotdata = plot data specific to each plot
	# labeldata = label data specific to each plot
	# datasetinfo = data about each of the plots, and how many there are
	
	allplots <- list( rsids = merged$rsid, positions = merged$position)
	
	if(!is.null(z_file) | !is.null(correlations)){
		allplots <- c( allplots, list(correlations = merged$corr, corrcolors = merged$corrcolors) )
	}
	if(!is.null(config_probs)){
		allplots <- c( allplots, list(configranks = merged$ranks))
		
		configdata <- list(configs = configslist, configprobs = config_probs$config_prob)
	}
	
	datasetinfo <- list( datasetnames = datasetnames, thresholds = thresholds, 
							labelprefix = labelprefix, yaxislabels = yaxislabels,
							plotlabels = plotlabels )
	
	plotdata <- list( logpval = -log10(merged$pvalue))
	labeldata <- list( logpval = merged$pvalue) 
	threshcolors <- list( logpval = merged$pvalcolors )
	
	if(!is.null(snp_probs)){
		plotdata <- c(plotdata, list( logBF = merged$snp_log10bf ))
		labeldata <- c(labeldata, list( logBF = merged$snp_prob))
		threshcolors <- c(threshcolors, list( logBF = merged$BFcolors))
	}
	
	if(!is.null(cond_pvalues)){
		plotdata <- c(plotdata, list( condlogpval = -log10(merged$cond_pvalue)))
		labeldata <- c(labeldata, list( condlogpval = merged$cond_pvalue))
		threshcolors <- c(threshcolors, list( condlogpval = merged$pvalcolors))
	}
	
    # create a list that contains the data to feed to JSON
    widgetdata <- list(
		allplots = allplots, plotdata = plotdata, labeldata = labeldata,
		threshcolors = threshcolors, datasetinfo = datasetinfo
    )
	
	if(!is.null(config_probs)){
		widgetdata <- c(widgetdata, list(  configdata = configdata ))
	}
	
	widgetdata <- c(widgetdata, list(labels = labels))

	# create widget
	htmlwidgets::createWidget(
	name = 'ConfigViewer',
	widgetdata,
	width = width,
	height = height,
	package = 'ConfigViewer',
	elementId = elementId,
	sizingPolicy = htmlwidgets::sizingPolicy(
		viewer.padding = 0,
		browser.defaultWidth = 700,
		browser.defaultHeight = 500
		)
  )
}

