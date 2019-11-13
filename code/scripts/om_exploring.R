
#################################### ISSUE #################################### 
#I have the framing data with the education categories from the second framing
#experiment. I have the education categories people should be using, from the
#OPM model run on the ANES data. For the ANES data, I have an lp value of
#education for every ANES observation. My idea was now to have the same for
#every framing observation, and then use the lp values to measure differences
#for the hot deck affinity score. BUT: How do I get an lp value for education
#for every framing observation? I would have to run polr on framing for that.
#But then the whole idea behind the OP model -- that you run it on a meaningful
#data set to get the 'true' education categories -- is cast aside. Also BUT: I
#don't think it's statistically valid to use the actual values for the other
#variables but the lp values for the education variable to calculate distances.
#Maybe I can do some weighting with the OP results. From Jeff, initially, when
#he told me the ida behind ordinal hot decking: "Use the OP model from the
#blocking paper to weight the distances between the categories in matches (in
#the form of other participants)"


library(hot.deck)
library(magrittr)
library(Zelig)
library(data.table)
library(tidyverse)
library(here)
library(missForest)

ord.list <- readRDS(here("data", "ord_list.rds"))  # read in ord.list, which is the output of OPMord
names(ord.list)
ord.list$ord.new.lev

data(D)
hot.data <- D
data <- D # to avoid confusion with affinity() syntax below
dim(data)
data <- data[,-5] # remove unneeded fifth column

# change data to have only one NA (to better see what's happening)
data[10, 1] <- 0

## Make continuous variable into integer 1:5 ones (to resemble education)
set.seed(123)
data[,4] <-sample(5, 20, replace = TRUE)





##### original affinity() function #####
affinity <-
function(data, index, column=NULL, R = NULL, weighted=FALSE){
	tmp <- abs(data - matrix(data[index, ], nrow=nrow(data), ncol=ncol(data), byrow=TRUE)) < 1
	if(is.null(column) | is.null(R)){
		if(weighted)warning("Correlation Matrix and/or Column Number not provided, switching to Unweighted Affinity\n")
	}
	if(!weighted){
		affinity <- rowMeans(tmp, na.rm=TRUE)
	}
	if(weighted){
		tmp[which(is.na(tmp), arr.ind=T)] <- FALSE
		affinity <- tmp %*% R[ ,column]
	}
	affinity[index] <- 0
	affinity
}


##### original hot.deck() function #####
# hot.deck(data, m=5)$affinity # lists the affinity scores of the NA observation data[1,1] for all 20 observations (with the score of [1,1] for 'itself' being 0)
hot.deck <-
function(data, m = 5, method=c("best.cell", "p.draw"), cutoff=10, sdCutoff=1, optimizeSD = FALSE, 
    optimStep = 0.1, optimStop = 5, weightedAffinity = FALSE, impContinuous = c("HD", "mice"), 
    IDvars = NULL, ...){
	method <- match.arg(method)
    impContinuous <- match.arg(impContinuous)
# DA 9/15/14 Added warning about weighted affinity calculations and correlations among or with categorical variables. 
    if(weightedAffinity){
        warning("Affinity calculations made as a function of pearson correlations among variables coerced to class 'numeric'\ntake care when using this on categorical, especially nominal variables")
    }
# DA 9/10/14: If IDvars is specified, remove them from the data and save in a different file. 
    if(!is.null(IDvars)){
        IDdata <- data[, which(names(data) %in% IDvars), drop=FALSE]
        data <- data[,-which(names(data) %in% IDvars), drop=FALSE]
# DA 9/10/14: Added code to remove any observations that are always missing
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
# DA 10/2/14: moved the warning here in response to Toby's problem where the error was getting tripped
# even though there was no continuous data.  This goes a setp further and doesn't trip the warning unless
# there is any continuous data with missing observations.
    if(impContinuous == "HD" & method == "p.draw" & length(cont.miss) > 0){
        stop("Hot Deck imputation of continuous values can only be used with the best cell method\n")
    }
	whichna <- which(is.na(data), arr.ind=TRUE)
    if(impContinuous == "mice"){
	    whichna <- whichna[which(whichna[,2] %in% disc.miss), ]
    }
# DA 9/5/14: added condition any(!alldisc) so that optimization only happens when there are continuous variables on which to optimize
    if(optimizeSD & any(!alldisc)){
        mm <- 0
        while(sdCutoff <= optimStop & mm < m){
            tmp <- scaleContinuous(data, alldisc, sdx=1/sdCutoff)
            numdata <- sapply(1:ncol(tmp), function(i)as.numeric(tmp[,i]))
            R <- abs(cor(numdata, use="pairwise"))
            diag(R) <- 0
# DA 9/5/14: Commented out 2 lines below because I changed the looping mechanism for the affinity calculation
            # aff <- t(apply(whichna, 1, function(x)affinity(numdata, x[1], x[2], R, weightedAffinity)))
            # aff[which(!is.finite(aff), arr.ind=TRUE)] <- 0

# DA 9/5/14: changed the way affinity is calculated so no duplicates are calculated.  This should speed
# up computation particularly when there are many observations missing on many variables.  This also makes
# the weighted measure potentially much slower
            unnaobs <- unique(whichna[,1])
            if(!weightedAffinity){
        	    aff <- t(sapply(unnaobs, function(x)affinity(numdata, x, weighted=FALSE)))
                aff <- aff[match(whichna[,1], unnaobs), ]
            }
            if(weightedAffinity){
            	aff <- t(apply(whichna, 1, function(x)affinity(numdata, x[1], x[2], R, weightedAffinity)))
            }
        	if(any(!is.finite(aff))){aff[which(!is.finite(aff), arr.ind=TRUE)] <- 0}

# DA 9/5/14: added the following 4 lines to ensure that only valid donors (i.e., those with observed values) have 
# non-zero affinity scores.
            wnadat <- matrix(1, nrow=nrow(data), ncol=ncol(data))
            wnadat[which(is.na(data), arr.ind=TRUE)] <- 0
            wnadat <- t(wnadat[, whichna[,2]])
            aff <- aff*wnadat
            w <- apply(aff, 1, function(x)which(x == max(x)))
            donors <- lapply(1:nrow(whichna), function(x)na.omit(data[w[[x]], whichna[x,2]]))
        	matches <- sapply(donors, length)
            mm <- min(matches)
            cat("SD Cutoff = ", sprintf("%.2f", sdCutoff), ", # Thin Cells = ", sum(matches < m), "\n", sep="")
            if(mm < m & sdCutoff == optimStop){warning(paste("Optimization unsuccessful, ", sum(matches < m), " thin cells remain with SD cutoff of ", sdCutoff, "\n", sep=""))}
            if(sdCutoff < optimStop){sdCutoff <- sdCutoff + optimStep}
        }

    }
# DA 9/10/14: changed result of scaleContinuous here to tmp from data so that the draws for the donors will not come from the scaled, but from the unscaled data. 
  tmp <- scaleContinuous(data, alldisc, sdx=1/sdCutoff)
	numdata <- sapply(1:ncol(tmp), function(i)as.numeric(tmp[,i]))
	R <- abs(cor(numdata, use="pairwise"))
	diag(R) <- 0
	max.emp.aff <- 	apply(R, 2, sum)[whichna[,2]] # new
	max.the.aff <- rep(dim(R)[2] - 1, nrow(whichna)) # new
	
# DA 9/5/14: Commented out 2 lines below because I changed the looping mechanism for the affinity calculation
    # aff <- t(apply(whichna, 1, function(x)affinity(numdata, x[1], x[2], R, weightedAffinity)))
    # aff[which(!is.finite(aff), arr.ind=TRUE)] <- 0

# DA 9/5/14: changed the way affinity is calculated so no duplicates are calculated.  This should speed
# up computation particularly when there are many observations missing on many variables.  This also makes
# the weighted measure potentially much slower
    unnaobs <- unique(whichna[,1])
    if(!weightedAffinity){
	    aff <- t(sapply(unnaobs, function(x)affinity(numdata, x, weighted=FALSE)))
        aff <- aff[match(whichna[,1], unnaobs), ]
    }
    if(weightedAffinity){
    	aff <- t(apply(whichna, 1, function(x)affinity(numdata, x[1], x[2], R, weightedAffinity)))
    }
	if(any(!is.finite(aff))){aff[which(!is.finite(aff), arr.ind=TRUE)] <- 0}

# DA 9/5/14: added the following 4 lines to ensure that only valid donors (i.e., those with observed values) have 
# non-zero affinity scores.
    wnadat <- matrix(1, nrow=nrow(data), ncol=ncol(data))
    wnadat[which(is.na(data), arr.ind=TRUE)] <- 0
    wnadat <- t(wnadat[, whichna[,2]])
    aff <- aff*wnadat
	if(method == "best.cell"){
		w <- apply(aff, 1, function(x)which(x == max(x)))
		donors <- lapply(1:nrow(whichna), function(x)na.omit(data[w[[x]], whichna[x,2]]))
		matches <- sapply(donors, length)
		if(any(matches < m)){warning(paste(sum(matches < m ), " of ", length(matches), " imputations with # donors < ", m, ", consider increasing sdCutoff or using method='p.draw'\n", sep=""))}
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
# DA 9/10/14: added three lines to put ID variables back in dataset
        if(!is.null(IDvars)){
            res[[md]] <- cbind(IDdata, res[[md]])
        }
	}
	class(res)  <- c("mi","list")
	return(list(data = res, affinity = aff, donors = donors, draws = draws, max.emp.aff = max.emp.aff, max.the.aff = max.the.aff))
}





##### Test affinity() #####

index <- 1 # for affinity() function
affinity(data = data, index = 1) # the affinity() package function does not appear to work. 
# From the manual: 
# "data: A data frame or matrix of values for which affinity should be
# calculated."
# "index: A row number identifying the target observation. Affinity will be
# calculated between this observation and all others in the dataset."

# Where the error occurs in the affinity() function: 
data - matrix(data[index, ], nrow=nrow(data), ncol=ncol(data), byrow=TRUE) # the subtraction of the matrix from the data frame does not work
function_matrix <- matrix(data[index, ], nrow=nrow(data), ncol=ncol(data), byrow=TRUE) # create the function matrix
class(data[index, ]) # this is a data frame
class(function_matrix[1,]) # this is a list
# Take the numbers from data[index, ], but concatenate them:
num <- c(NA, 0, 1, 0.8860932, 0.310649)
num_matrix <- matrix(num, nrow=nrow(data), ncol=ncol(data), byrow=TRUE) # create the matrix with the numeric vector
class(num) # this is numeric
class(num_matrix[1,]) # this is numeric
data - matrix(num, nrow=nrow(data), ncol=ncol(data), byrow=TRUE) # the subtraction of the matrix from the data frame now works
as.numeric(data[index, ]) # this makes the function part numeric

# We need to adjust the affinity() function to wrap data[index,] in as.numeric(). Then it works:
data - matrix(as.numeric(data[index, ]), nrow=nrow(data), ncol=ncol(data), byrow=TRUE)
# Or we make data a matrix. Then it works as well:
as.matrix(data) - matrix(as.matrix(data)[index, ], nrow=nrow(data), ncol=ncol(data), byrow=TRUE) 
# Which, in the whole function, equates to:
affinity(data = as.matrix(data), index = 1)
aff_scores <- affinity(data = as.matrix(data), index = index)


# So the issue is that data can't be a data frame. It has to be a matrix. The
# documentation in the pdf is wrong. I assume they changed code in hot.deck()
# before affinity() was applied but then didn't check affinity() on its own
# still worked


# However, affinity() is part of hot.deck(), and it works when used within that:
hot.deck(data = data, m =5, method = "best.cell", weightedAffinity = FALSE)
hd_scores <- hot.deck(data = data, m =5, method = "best.cell", weightedAffinity = FALSE)

# However again, the fixed affinity() output is not the same as hd_scores$affinity:
aff_scores
#  [1] 0.00 1.00 0.75 1.00 1.00 0.75 0.75 0.75 0.75 0.75 0.50 0.25 0.25 0.50 0.50 1.00 1.00 1.00 1.00 1.00
hd_scores$affinity
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20]
# [1,]    0 0.75 0.75 0.75 0.75  0.5  0.5 0.25  0.5   0.5  0.25     0     0     0     0  0.75     1  0.75   0.5  0.75



##### Run hot.deck() line by line (with not needed stuff in the function removed) #####

data <- data
m <- 5
method <- "best.cell"

hot.deck <-
function(data, m = 5, method=c("best.cell", "p.draw"), cutoff=10, sdCutoff=1, optimizeSD = FALSE, 
    optimStep = 0.1, optimStop = 5, weightedAffinity = FALSE, impContinuous = c("HD", "mice"), 
    IDvars = NULL, ...){
        allNA <- apply(data, 1, function(x)all(is.na(x)))
        if(any(allNA)){
            data <- data[-which(allNA), , drop=FALSE]
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
  tmp <- scaleContinuous(data, alldisc, sdx=1/sdCutoff)
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
	if(any(!is.finite(aff))){aff[which(!is.finite(aff), arr.ind=TRUE)] <- 0}
    wnadat <- matrix(1, nrow=nrow(data), ncol=ncol(data))
    wnadat[which(is.na(data), arr.ind=TRUE)] <- 0
    wnadat <- t(wnadat[, whichna[,2]])
    aff <- aff*wnadat
	if(method == "best.cell"){
		w <- apply(aff, 1, function(x)which(x == max(x)))
		donors <- lapply(1:nrow(whichna), function(x)na.omit(data[w[[x]], whichna[x,2]]))
		matches <- sapply(donors, length)
		if(any(matches < m)){warning(paste(sum(matches < m ), " of ", length(matches), " imputations with # donors < ", m, ", consider increasing sdCutoff or using method='p.draw'\n", sep=""))}
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


# When running hot.deck() line by line above, I saw that this line created the affinity scores within hot.deck():
# > affinity(numdata, unnaobs, weighted=FALSE)
# [1] 0.00 0.75 0.75 0.75 0.75 0.50 0.50 0.25 0.50 0.50 0.25 0.00 0.00 0.00 0.00 0.75 1.00 0.75 0.50 0.75
# (unnaobs = index = 1)
# numdata is created by scaling the continuous variables in the data. affinity() on its own doesn't do that




##### Create adjusted affinity() to run with numdata instead of data (otherwise there would be conflicts with data in hot.deck() #####

adj_affinity <-
  function(newdata, index, column = NULL, R = NULL, weighted = FALSE){
    tmp <- abs(newdata - matrix(newdata[index, ], nrow=nrow(newdata), ncol=ncol(newdata), byrow=TRUE)) < 1
    if(is.null(column) | is.null(R)){
      if(weighted)warning("Correlation Matrix and/or Column Number not provided, switching to Unweighted Affinity\n")
      }
    if(!weighted){
      affinity <- rowMeans(tmp, na.rm=TRUE)
      }
    if(weighted){
      tmp[which(is.na(tmp), arr.ind=T)] <- FALSE
      affinity <- tmp %*% R[ ,column]
      }
    affinity[index] <- 0
	  affinity
}


# Now the two results are identical (only works if hot.deck() is first run line by line)
orig <- affinity(numdata, unnaobs, weighted=FALSE)
adj <- adj_affinity(numdata, unnaobs, weighted = FALSE)
all.equal(orig, adj)


# Also works with weighted versions, which I need (only works if hot.deck() is first run line by line)
orig_w <- affinity(numdata, whichna[1], whichna[2], R, weighted = TRUE)
adj_w <- adj_affinity(numdata, whichna[1], whichna[2], R, weighted = TRUE)
all.equal(orig_w, adj_w)


#### Create adjusted hot.deck() function to run line by line ####

data <- data
m <- 5
method <- "best.cell"
cutoff = 3 # this is where continuous variables start, i.e.: Anything above 3 categories is considered continuous (executed by is.discrete())
sdCutoff = 1
optimizeSD = FALSE
optimStep = 0.1
optimStop = 5
weightedAffinity = TRUE
#weightedAffinity = FALSE
IDvars = NULL



adj_hot.deck <-function(data, m = 5, method=c("best.cell", "p.draw"), cutoff=10, sdCutoff=1, optimizeSD = FALSE, 
    optimStep = 0.1, optimStop = 5, weightedAffinity = FALSE, impContinuous = c("HD", "mice"), 
    IDvars = NULL, ...){
  #method <- match.arg(method) # doesn't work when run line by line
  #impContinuous <- match.arg(impContinuous) # doesn't work when run line by line
    if(weightedAffinity){
        warning("Affinity calculations made as a function of pearson correlations among variables coerced to class 'numeric'\ntake care when using this on categorical, especially nominal variables")
      }
        allNA <- apply(data, 1, function(x)all(is.na(x)))
        if(any(allNA)){
            data <- data[-which(allNA), , drop=FALSE]
          }
    if(any(allNA)){
        warning(paste(sum(allNA), " observations with no observed data.  These observations were removed\n", sep="") )
      }
	facs <- sapply(1:ncol(data), function(x)is.factor(data[,x]))
	disc.miss <- which(is.discrete(data, cutoff) & apply(data, 2, function(x)any(is.na(x))))
	alldisc <- is.discrete(data, cutoff)
	allmiss <- which(is.na(data), arr.ind=TRUE)
	cont.miss <- allmiss[-which(allmiss[,2] %in% disc.miss), ]
#    if(impContinuous == "HD" & method == "p.draw" & length(cont.miss) > 0){
#        stop("Hot Deck imputation of continuous values can only be used with the best cell method\n")
#      }
	whichna <- which(is.na(data), arr.ind=TRUE)
#    if(impContinuous == "mice"){
#	    whichna <- whichna[which(whichna[,2] %in% disc.miss), ]
#	    }
  tmp <- scaleContinuous(data, alldisc, sdx=1/sdCutoff) # data frame, scaled data frame of numeric values
	numdata <- sapply(1:ncol(tmp), function(i)as.numeric(tmp[,i])) # matrix of same numeric values
	R <- abs(cor(numdata, use="pairwise")) # 'pairwise' is about how NAs are handled
	diag(R) <- 0
	max.emp.aff <- 	apply(R, 2, sum)[whichna[,2]]
	max.the.aff <- rep(dim(R)[2] - 1, nrow(whichna))
  unnaobs <- unique(whichna[,1])


  ## NOT WEIGHTED ##
# aff <- t(sapply(unnaobs, function(x)affinity(numdata, x, weighted=FALSE))) # affinity() is used # REPLACED
      
	tmp <- abs(numdata - matrix(numdata[unnaobs, ], nrow=nrow(numdata), ncol=ncol(numdata), byrow=TRUE)) < 1 # this turns everything into NA, TRUE, FALSE
  affinity <- rowMeans(tmp, na.rm=TRUE) # this is the same as this: rowMeans(ifelse(tmp == TRUE, 1, ifelse(tmp == FALSE, 0, NA)), na.rm = TRUE)  # so this only works for 0/1 (binary) data
  affinity[unnaobs] <- 0
#	aff <- t(sapply(unnaobs, affinity)) # NOT POSSIBLE AS REPLACEMENT because sapply needs a function as its second argument
  aff <- matrix(affinity, ncol = nrow(numdata)) # ADDED to replace sapply above
  aff <- aff[match(whichna[,1], unnaobs), ]
    
  ## WEIGHTED ##
#	aff <- t(apply(whichna, 1, function(x)affinity(numdata, x[1], x[2], R, weighted = TRUE))) # affinity() is used # REPLACED
	tmp <- abs(numdata - matrix(numdata[whichna[1], ], nrow=nrow(numdata), ncol=ncol(numdata), byrow=TRUE)) < 1 # this turns everything into NA, TRUE, FALSE
  tmp[which(is.na(tmp), arr.ind=T)] <- FALSE # this turns all NAs into FALSE
  affinity <- tmp %*% R[ ,whichna[2]] # this actually calculates the affinity scores for all 20 observations
  affinity[whichna[1]] <- 0
# aff <- t(apply(whichna, affinity)) # NOT POSSIBLE AS REPLACEMENT because apply needs a function as its second argument
	affinity <- t(affinity) # ADDED to replace apply above


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
#	  if(method == "p.draw"){
#	    donors <- lapply(1:nrow(whichna), function(x)aggregate(aff[x, ], list(data[, whichna[x,2]]), mean, na.rm=TRUE))
#	    draws <- lapply(1:length(donors), function(x)sample(donors[[x]][,1], m, replace=TRUE, prob=donors[[x]][,2]))
#	    }
	res <- vector(mode="list", length=m)
	inp.D <- lapply(1:m, function(x)data)
	  for(md in 1:m){
		  for(i in 1:nrow(whichna)){
			  inp.D[[md]][whichna[i,1], whichna[i,2]] <- draws[[i]][md]
			  }
#		  if(length(cont.miss) > 0 & impContinuous == "mice"){
#		    mice.D <- mice(inp.D[[md]], m = 1, ...)
#		    res[[md]] <- complete(mice.D)
#		    }
#	    else{
			  res[[md]] <- inp.D[[md]]
#			  }
      if(!is.null(IDvars)){
        res[[md]] <- cbind(IDdata, res[[md]])
        }
	    }
	class(res)  <- c("mi","list")
	return(list(data = res, affinity = aff, donors = donors, draws = draws, max.emp.aff = max.emp.aff, max.the.aff = max.the.aff))
}
        	      

# Run above line by line with weightedAffinity = FALSE        	      
res        	      
aff
donors
draws
max.emp.aff
max.the.aff

not_weighted <- hot.deck(data, method = "best.cell")
all.equal(aff, not_weighted$affinity) # Identical

# Run above line by line with weightedAffinity = TRUE
res        	      
aff
donors
draws
max.emp.aff
max.the.aff

weighted <- hot.deck(data, method = "best.cell", weightedAffinity = TRUE)
all.equal(aff, weighted$affinity) # Identical



# Both weighted and not weighted have this line:
# tmp <- abs(numdata - matrix(numdata[unnaobs, ], nrow=nrow(numdata), ncol=ncol(numdata), byrow=TRUE)) < 1 
# This creates a TRUE/FALSE matrix. Everything < 1 is FALSE, everything else is
# TRUE. This excludes a lot of values for a 1-5 variable, but it also does that
# for a continuous variable (which could be anything). However, the continuous
# variables get scaled to around 1/-1 (I tested this with numbers such as 20.35
# and -15.67). I struggle to see how < 1 can be suitable for a 1-5 discrete
# variable. I emailed Jeff about this (10/23).

# Jeff first suggested to use absolute education values. I pointed out that this
# doesn't fit with the OP idea. He then suggested that I run OP on this
# (non-ANES) data and use the resulting cutpoints



##### Run OP on the test data, use the resulting cutpoints in original hot.deck() #####

load(here("functions", "OPMord.Rdata"))

OPMord.out <- OPMord(data = na.omit(data), dv = "x4", evs = c("x1", "x2", "x3")) # on Jeff's suggestion, run OP model on this data and use the resulting cutoff points

OPMord.out$int.df # we're 'missing' the beginning cutoff for category 1 and the ending cutoff for category 5

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

# save("OPMcut", file = here("functions", "OPMcut.Rdata"))

OPMcut.out <- OPMcut(data = data, dv = "x4", OPMordOut = OPMord.out) # it works

hot.deck(OPMcut.out) # also works!!

# However, hot.deck() in its current form doesn't scale the educ variable.
# scaleContinuous() uses cutoff, which is by default set to 10. This means any
# variable with fewer than 10 unique values is treated as discrete and thus not
# scaled. I could set cutoff to 5, so it would cover educ. However, I'm wary
# that this would universally treat lots of other variables as continuous. A
# better way, I think, is to add code which treats the ordinal variable as a
# special case and scales it, in addition to anything above a cutoff of 10. To
# avoid confusion, I'm saving that as the new function hot.deck.ord()


##### Create hot.deck.ord() to scale educ variable #####

data <- OPMcut.frame.more.nas
m = m
ord = "educ"
sdCutoff = sd(na.omit(framing.nas$educ))
method = "best.cell"
cutoff=10

hot.deck.ord <-
function(data, ord, m = 5, method=c("best.cell", "p.draw"), cutoff=10, sdCutoff=1, optimizeSD = FALSE,   # adds "ord" to the list
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
    # runs scaleContinuous only on the ordinal variable and replaces the unscaled version of the variable in tmp
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

# save("hot.deck.ord", file = here("functions", "hot.deck.ord.Rdata"))


# From Jeff: "Get the thing programmed up and we can create a dataset where
# we intentionally make missing data so we know the answer. How well do we do
# compared to the truth. We can also loop this process to see if unbiased. New
# territory!"




##### Run OPMord(), OPMcut(), and hot.deck.ord() on (cleaned) framing data #####

# I want a dataset with more obversations -- I might as well use my framing data
# I exported the framing data from the second framing experiment .Rmd stored in
# odrive/papers)

framing <- read.csv(here("data", "second_framing_experiment.csv")) 

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

framing_true <- framing <- framing[, c("Dem", "Rep", "Ind", "educ", "inc", "age", "Female", "Male", "Black", "Hisp", "White")]

# Introduce one NA to one EV from my framing regression model
framing[1, "Dem"] <- NA # introduce one NA to EV Dem -- original/true value is 0

# Run OPMord() on framing data with introduced NA
OPMord.frame <- OPMord(data = na.omit(framing), dv = "educ", evs = c("Dem", "Rep", "Ind", "inc", "age", "Female", "Male", "Black", "Hisp", "White")) # on Jeff's suggestion, run OP model on this data and use the resulting cutoff points

# Run OPMcut() on framing data with introduced NA
OPMcut.frame <- OPMcut(data = framing, dv = "educ", OPMordOut = OPMord.frame) # it works

# Run hot.deck.ord() on framing data with introduced NA
hot.deck.frame <- hot.deck.ord(OPMcut.frame, m =1, ord = "educ", sdCutoff = sd(framing$educ), method = "best.cell") # it works and scales the ordinal variable correctly (I checked that by running the new function line by line)




##### Look at the imputed variable means when running hot.deck() on the test data #####

hdd <- hot.deck(data = hot.data)

na.rows.cols <- which(is.na(hot.data), arr.ind=TRUE)
m <- na.rows.cols %>% nrow() # number of imputations done by hot.deck / number of NAs in the dataset
na.cols <- na.rows.cols %>% .[,2] %>% unique() # which column numbers have NAs in them

na.colnames <- names(hot.data)[na.cols] # names of columns with NAs in them
means.df <- data.frame(matrix(NA, m, length(na.cols))) # empty df to store means

for (x in 1:length(na.cols)){
  for (i in 1:m){
    means.df[i,x] <- hdd$data[[i]] %>% .[, na.colnames[x]] %>% mean()
  }
}

means.df
sapply(means.df, mean)


# The number of imputations is the same as the number of NAs. The hot.deck()
# test data has 5 NAs, so it imputes 5 datasets. I have introduced 1 NA in the
# framing dataset, so it imputes 1 dataset. The estimate of the mean of the
# imputed variables is the same across the 5 imputations for the binary
# variables, but it changes for the continuous variables. The continuous
# estimates also change slightly between runs of hot.deck()






##### Randomly insert more NAs into framing data, for different types of variables (binary/continuous). Run OPMord(), OPMcut(), and hot.deck.ord() on the data. Also run hot.deck() on the data. Compare the resulting imputed variable means to the true values, hot.deck() means, and na.omit() means  #####



add.nas.columns <- c("Dem", "Rep", "inc", "age") # variables to insert NAs into # Two binary, one nominal, one continuous
no.nas <- framing_true[,!names(framing_true) %in% add.nas.columns] # separate framing_true into columns with and without NAs
yes.nas <- framing_true[,names(framing_true) %in% add.nas.columns]

prop <- c(.1) # the proportions of NAs for the loop
res.list <- list() # empty list to store result dfs

winner <- data.frame(matrix(NA, length(prop), length(add.nas.columns))) # empty df to store which method was closest to true value
colnames(winner) <- add.nas.columns
rownames(winner) <- prop

for(q in 1:length(prop)){
  set.seed(123)
  framing.nas <- cbind(no.nas, prodNA(yes.nas, noNA = prop[q])) # combine columns with NAs and columns without NAs
  na.rows.cols <- which(is.na(framing.nas), arr.ind=TRUE) # which rows and columns have NAs
  m <- na.rows.cols %>% nrow() # how many rows have NAs

  OPMord.frame.more.nas <- OPMord(data = na.omit(framing.nas), dv = "educ", evs = c("Dem", "Rep", "Ind", "inc", "age", "Female", "Male", "Black", "Hisp", "White"))

  OPMcut.frame.more.nas <- OPMcut(data = framing.nas, dv = "educ", OPMordOut = OPMord.frame.more.nas)

  hot.deck.ord.frame.more.nas <- hot.deck.ord(OPMcut.frame.more.nas, m = m, ord = "educ", sdCutoff = sd(na.omit(framing.nas$educ)), method = "best.cell")

  hot.deck.norm.frame.more.nas <- hot.deck(OPMcut.frame.more.nas, m = m, sdCutoff = sd(na.omit(framing.nas$educ)), method = "best.cell")

  na.cols <- na.rows.cols %>% .[,2] %>% unique() # which column numbers have NAs in them
  na.colnames <- names(framing.nas)[na.cols] # names of columns with NAs in them
  means.hd.norm <- means.hd.ord <- data.frame(matrix(NA, m, length(na.cols))) # empty dfs to store variable means for hd.ord and hd.norm

    for (x in 1:length(na.cols)){
      for (i in 1:m){
        means.hd.ord[i,x] <- hot.deck.ord.frame.more.nas$data[[i]] %>% .[, na.colnames[x]] %>% mean()
        means.hd.norm[i,x] <- hot.deck.norm.frame.more.nas$data[[i]] %>% .[, na.colnames[x]] %>% mean()
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
  
  res.list[[q]] <- results # store means for each NA proportion in list

    for (t in 1:length(add.nas.columns)){ # fill df with the 'winning' method for each variable
      winner[q, t] <- results$method[results[,t] == min(results[,t])]
    }
}



# I sent a short version of this to Jeff. He was surprised about the variation
# and told me to run it for 10,000 iterations, so we can take a look at the
# distribution. I set this up in "om_mc_jeff.R":
# Loop version, 10.000 iterations, .05 percent NAs inserted, without setting the
# seed.



