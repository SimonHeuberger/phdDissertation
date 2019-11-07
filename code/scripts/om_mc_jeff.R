

library(hot.deck)
library(magrittr)
library(data.table)
library(tidyverse)
library(missForest)


load("OPMord.Rdata") # load the function that applies polr() to a dataset


# Write a function that replaces all values in an ordinal variable with the mid-cutpoints of the polr() lp values

OPMcut <- function(data, dv, OPMordOut){
    # turn dv into factor variable if it isn't one
  if(is.factor(data[, dv]) == FALSE){
    data[, dv] <- as.factor(data[, dv])
  }
    # store levels of dv and the number of the penultimate level
  lev <- levels(data[, dv])
  penult <- length(lev)-1
    # category span between first and second level
  cat_second_span <- abs(OPMordOut[["int.df"]][1,2] - OPMordOut[["int.df"]][2,2])
    # category span between penultimate and last level
  cat_penult_span <- abs(OPMordOut[["int.df"]][(penult-1),2] - OPMordOut[["int.df"]][penult,2])
    # beginning cutpoint for first level 
  cat_first_cut <- OPMordOut[["int.df"]][1,2] - cat_second_span
    # end cutpoint for last level 
  cat_last_cut <- OPMordOut[["int.df"]][penult,2] + cat_penult_span
   # empty vector to store middle between cutpoints for each level
  midpoints <- c()
    # mid-cutpoint for first level
  midpoints[1] <- (cat_first_cut + OPMordOut[["int.df"]][1,2])/2
    # mid-cutpoints for all except first and last levels 
  for (i in 1:(penult-1)){
    midpoints[i+1] <- (OPMordOut[["int.df"]][i,2] + OPMordOut[["int.df"]][(i+1),2])/2
  }
    # mid-cutpoint for last level 
  midpoints[length(lev)] <- (OPMordOut[["int.df"]][penult,2] + cat_last_cut)/2
    # empty df to store dv level replacements
  tmp <- data.frame(matrix(NA, nrow(data), length(lev)))
    # replace dv levels with mid-cutpoints
  for (x in 1:length(lev)){
    tmp[, x] <- ifelse(data[, dv] == lev[x], midpoints[x], NA)
  }
    # overwrite dv with mid-cutpoints replacement
  data[, dv] <- c(na.omit(c(t(tmp))))
  return(data)
}


# Create hot.deck.ord() function to scale the education variable

hot.deck.ord <-
function(data, ord, m = 5, method=c("best.cell", "p.draw"), cutoff=10, sdCutoff=1, optimizeSD = FALSE,   # adds "ord" to the list of required input
    optimStep = 0.1, optimStop = 5, weightedAffinity = FALSE, impContinuous = c("HD", "mice"), 
    IDvars = NULL, ...){
	method <- match.arg(method)
  impContinuous <- match.arg(impContinuous)
    if(weightedAffinity){
        warning("Affinity calculations made as a function of pearson correlations among variables coerced to class 'numeric'\ntake care when using this on categorical, especially nominal variables")
    }
    if(!is.null(IDvars)){
        IDdata <- data[, which(names(data) %in% IDvars), drop=FALSE]
        data <- data[,-which(names(data) %in% IDvars), drop=FALSE]
        allNA <- apply(data, 1, function(x)all(is.na(x)))
          if(any(allNA)){
              IDdata <- IDdata[-which(allNA), , drop=FALSE]
              data <- data[-which(allNA), , drop=FALSE]
          }
    }
    else{
        allNA <- apply(data, 1, function(x)all(is.na(x)))
          if(any(allNA)){
              data <- data[-which(allNA), , drop=FALSE]
          }
    }
    if(any(allNA)){
        warning(paste(sum(allNA), " observations with no observed data.  These observations were removed\n", sep="") )
    }
	facs <- sapply(1:ncol(data), function(x)is.factor(data[,x]))
	disc.miss <- which(is.discrete(data, cutoff) & apply(data, 2, function(x)any(is.na(x))))
	alldisc <- is.discrete(data, cutoff)
	allmiss <- which(is.na(data), arr.ind=TRUE)
	cont.miss <- allmiss[-which(allmiss[,2] %in% disc.miss), ]
    if(impContinuous == "HD" & method == "p.draw" & length(cont.miss) > 0){
        stop("Hot Deck imputation of continuous values can only be used with the best cell method\n")
    }
	whichna <- which(is.na(data), arr.ind=TRUE)
    if(impContinuous == "mice"){
	    whichna <- whichna[which(whichna[,2] %in% disc.miss), ]
    }
    if(optimizeSD & any(!alldisc)){
        mm <- 0
          while(sdCutoff <= optimStop & mm < m){
              tmp <- scaleContinuous(data, alldisc, sdx=1/sdCutoff)
              numdata <- sapply(1:ncol(tmp), function(i)as.numeric(tmp[,i]))
              R <- abs(cor(numdata, use="pairwise"))
              diag(R) <- 0
              unnaobs <- unique(whichna[,1])
                if(!weightedAffinity){
        	        aff <- t(sapply(unnaobs, function(x)affinity(numdata, x, weighted=FALSE)))
                  aff <- aff[match(whichna[,1], unnaobs), ]
                }
                if(weightedAffinity){
            	    aff <- t(apply(whichna, 1, function(x)affinity(numdata, x[1], x[2], R, weightedAffinity)))
                }
        	      if(any(!is.finite(aff))){
        	        aff[which(!is.finite(aff), arr.ind=TRUE)] <- 0
        	      }
              wnadat <- matrix(1, nrow=nrow(data), ncol=ncol(data))
              wnadat[which(is.na(data), arr.ind=TRUE)] <- 0
              wnadat <- t(wnadat[, whichna[,2]])
              aff <- aff*wnadat
              w <- apply(aff, 1, function(x)which(x == max(x)))
              donors <- lapply(1:nrow(whichna), function(x)na.omit(data[w[[x]], whichna[x,2]]))
        	    matches <- sapply(donors, length)
              mm <- min(matches)
              cat("SD Cutoff = ", sprintf("%.2f", sdCutoff), ", # Thin Cells = ", sum(matches < m), "\n", sep="")
                if(mm < m & sdCutoff == optimStop){
                  warning(paste("Optimization unsuccessful, ", sum(matches < m), " thin cells remain with SD cutoff of ", sdCutoff, "\n", sep=""))
                }
                if(sdCutoff < optimStop){
                  sdCutoff <- sdCutoff + optimStep
                }
          }

    }
  tmp <- scaleContinuous(data, alldisc, sdx=1/sdCutoff)
  ord.disc <- FALSE  # added by me for scaleContinuous for ordinal variable
  tmp[, ord] <- scaleContinuous(data[, ord], ord.disc, sdx = 1/sdCutoff)[,1]  # added by me for scaleContinuous for ordinal variable
    # this runs scaleContinuous on only the ordinal variable and replaces the unscaled version of the variable in tmp
  numdata <- sapply(1:ncol(tmp), function(i)as.numeric(tmp[,i]))
	R <- abs(cor(numdata, use="pairwise"))
	diag(R) <- 0
	max.emp.aff <- 	apply(R, 2, sum)[whichna[,2]]
	max.the.aff <- rep(dim(R)[2] - 1, nrow(whichna))
  unnaobs <- unique(whichna[,1])
    if(!weightedAffinity){
	    aff <- t(sapply(unnaobs, function(x)affinity(numdata, x, weighted=FALSE)))
      aff <- aff[match(whichna[,1], unnaobs), ]
    }
    if(weightedAffinity){
    	aff <- t(apply(whichna, 1, function(x)affinity(numdata, x[1], x[2], R, weightedAffinity)))
    }
	  if(any(!is.finite(aff))){
	    aff[which(!is.finite(aff), arr.ind=TRUE)] <- 0
	  }

  wnadat <- matrix(1, nrow=nrow(data), ncol=ncol(data))
  wnadat[which(is.na(data), arr.ind=TRUE)] <- 0
  wnadat <- t(wnadat[, whichna[,2]])
  aff <- aff*wnadat
	  if(method == "best.cell"){
		  w <- apply(aff, 1, function(x)which(x == max(x)))
		  donors <- lapply(1:nrow(whichna), function(x)na.omit(data[w[[x]], whichna[x,2]]))
		  matches <- sapply(donors, length)
		    if(any(matches < m)){
		      warning(paste(sum(matches < m ), " of ", length(matches), " imputations with # donors < ", m, ", consider increasing sdCutoff or using method='p.draw'\n", sep=""))
		    }
		  repl <- ifelse(matches < m, TRUE, FALSE)
	    draws <- lapply(1:length(donors), function(x)sample(donors[[x]], m, replace=repl[x]))
	  }
	  if(method == "p.draw"){
		  donors <- lapply(1:nrow(whichna), function(x)aggregate(aff[x, ], list(data[, whichna[x,2]]), mean, na.rm=TRUE))
	    draws <- lapply(1:length(donors), function(x)sample(donors[[x]][,1], m, replace=TRUE, prob=donors[[x]][,2]))
	  }
	res <- vector(mode="list", length=m)
	inp.D <- lapply(1:m, function(x)data)
	  for(md in 1:m){
		  for(i in 1:nrow(whichna)){
			  inp.D[[md]][whichna[i,1], whichna[i,2]] <- draws[[i]][md]
		  }
		  if(length(cont.miss) > 0 & impContinuous == "mice"){
			  mice.D <- mice(inp.D[[md]], m = 1, ...)
			  res[[md]] <- complete(mice.D)
		  }
		  else{
			  res[[md]] <- inp.D[[md]]
		  }
      if(!is.null(IDvars)){
        res[[md]] <- cbind(IDdata, res[[md]])
      }
	  }
	class(res)  <- c("mi","list")
	return(list(data = res, affinity = aff, donors = donors, draws = draws, max.emp.aff = max.emp.aff, max.the.aff = max.the.aff))
}



# Load the framing data
framing <- read.csv("second_framing_experiment.csv")

# Select needed columns
framing <- framing[, c("tb_supp", "tb", "pid", "educ", "inc", "age", "race", "gender", "empl", "interest", "media", "part" )]

# Make all needed columns binary, i.e. numeric/integer (needed for hot.deck())
framing <- mutate(framing,
                  C = ifelse(tb == "C", 1, 0),
                  M_Supp = ifelse(tb == "M_Supp", 1, 0),
                  P_Supp = ifelse(tb == "P_Supp", 1, 0),
                  M_Opp = ifelse(tb == "M_Opp", 1, 0),
                  P_Opp = ifelse(tb == "P_Opp", 1, 0),
                  Dem = ifelse(pid == "Democrat", 1, 0),
                  Rep = ifelse(pid == "Republican", 1, 0),
                  Ind = ifelse(pid == "Independent", 1, 0),
                  SthElse = ifelse(pid == "Something Else", 1, 0),
                  Black = ifelse(race == "Black or African-American", 1, 0),
                  Hisp = ifelse(race == "Hispanic", 1, 0),
                  White = ifelse(race == "White", 1, 0),
                  Female = ifelse(gender == "Female", 1, 0),
                  Male = ifelse(gender == "Male", 1, 0)
)

# Select needed columns and save complete data under a different name
framing_true <- framing <- framing[, c("Dem", "Rep", "Ind", "educ", "inc", "age", "Female", "Male", "Black", "Hisp", "White")]


# variables to insert NAs into. Two are binary, one is nominal, one is continuous
add.nas.columns <- c("Dem", "Rep", "inc", "age") 
no.nas <- framing_true[,!names(framing_true) %in% add.nas.columns] # separate framing_true into columns with and without NAs
yes.nas <- framing_true[,names(framing_true) %in% add.nas.columns]


prop <- .05 # the proportions of NAs to insert (for the loop)

methods <- c("true", "hd.ord", "hd.norm", "na.omit")

mc.iterations <- 10000

results.list <- rep(list(data.frame(matrix(NA, length(methods), length(add.nas.columns)+1))), mc.iterations)

winner.list <- rep(list(data.frame(matrix(NA, length(prop), length(add.nas.columns)))), mc.iterations)

pb <- txtProgressBar(min = 1, max = mc.iterations, style = 3) # creates the percentage progress bar across the sampled numbers


for(mc in 1:mc.iterations){
  
  setTxtProgressBar(pb, mc) # loads the percentage progress bar into the loop
  
  framing.nas <- cbind(no.nas, prodNA(yes.nas, noNA = prop)) # combine columns with NAs and columns without NAs
  na.rows.cols <- which(is.na(framing.nas), arr.ind=TRUE) # which rows and columns have NAs
  m <- na.rows.cols %>% nrow() # how many rows have NAs

  OPMord.frame.more.nas <- OPMord(data = na.omit(framing.nas), dv = "educ", evs = c("Dem", "Rep", "Ind", "inc", "age", "Female", "Male", "Black", "Hisp", "White")) # run ordinal polr() function on data

  OPMcut.frame.more.nas <- OPMcut(data = framing.nas, dv = "educ", OPMordOut = OPMord.frame.more.nas) # run function that replaces ordinal values with mid-cutpoints

  hot.deck.ord.frame.more.nas <- hot.deck.ord(OPMcut.frame.more.nas, m = m, ord = "educ", sdCutoff = sd(na.omit(framing.nas$educ)), method = "best.cell") # run hot.deck.ord(), which scales the ordinal variable

  hot.deck.norm.frame.more.nas <- hot.deck(OPMcut.frame.more.nas, m = m, sdCutoff = sd(na.omit(framing.nas$educ)), method = "best.cell") # run normal hot.deck()

  na.cols <- na.rows.cols %>% .[,2] %>% unique() # which column numbers have NAs in them
  na.colnames <- names(framing.nas)[na.cols] # names of columns with NAs in them
  
    for (x in 1:length(na.cols)){
      for (i in 1:m){
        means.hd.ord[i,x] <- hot.deck.ord.frame.more.nas$data[[i]] %>% .[, na.colnames[x]] %>% mean() # means from hot.deck.ord()
        means.hd.norm[i,x] <- hot.deck.norm.frame.more.nas$data[[i]] %>% .[, na.colnames[x]] %>% mean() # means from normal hot.deck()
      }
    }

  true <- sapply(framing_true[, na.colnames], mean) # true variable means
  hd.ord <- sapply(means.hd.ord, mean) # hd.ord variable means
  hd.norm <- sapply(means.hd.norm, mean) # hd.norm variable means
  na.omit <- sapply(na.omit(framing.nas[, na.colnames]), mean) # na.omit variable means
  results <- data.frame(rbind(true, abs(true-hd.ord), abs(true-hd.norm), abs(true-na.omit))) # combine all means in a df
  results[,5] <- c("true", "hd.ord", "hd.norm", "na.omit")
  colnames(results)[5] <- "method"
  rownames(results) <- NULL

  results.list[[mc]] <- results # store means for each NA proportion in list

    for (t in 1:length(add.nas.columns)){ # fill df with the 'winning' method for each variable
      winner.list[[mc]][1,t] <- as.character(results$method[results[,t] == min(results[,t])])
    }
}

saveRDS(results.list, file = "list.ord.cut.mc.results.rds")

saveRDS(winner.list, file = "list.ord.cut.mc.winner.rds")

