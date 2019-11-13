
# The function needs the inputs: data (data frame), dv (character), evs (character or string of characters)
# It outputs data, plr.out, plr.df, int.df, the new education levels, and the new numeric education levels
# The output can be called with $

OPMord <- function(data, dv, evs){
  library(MASS)
  library(data.table)
    # save original data with all NAs
  data.full.nas <- data
    # use na.omit version of the data in the function
  data <- na.omit(data)
    # turn dv into factor variable if it isn't one
  if(is.factor(data[, dv]) == FALSE){
    data[, dv] <- as.factor(data[, dv])
  }
  data[,paste(dv, ".num", sep = "")] <- as.numeric(data[,dv])
  variables <- c(dv, evs)
  a <- as.formula(data[,variables])
  plr.out <- polr(a, data = data, Hess=TRUE) 
    # turn plr.out output into a data frame
  plr.df <- data.frame(coef(summary(plr.out)))
    # empty vector for storage
  empty <- c()
    # loop to fill empty vector with intercept names
  for (i in 1:(length(levels(data[,dv]))-1)){
    empty[i] <- paste(levels(data[,dv])[i], "|", levels(data[,dv])[i+1], sep = "")
  }                                                                        
    # select only rows with intercept names
  int.df <- plr.df[empty,]
    # turn row names into a column
  int.df <- setDT(int.df, keep.rownames = TRUE)[]
    # removes class "data.table", which was added by setDT
  int.df <- data.frame(int.df)
  colnames(int.df) <- c("Intercepts", "Values", "SE", "t-values")
    # factorize intercepts with correctly ordered levels
  int.df$Intercepts <- factor(int.df$Intercepts, levels = int.df$Intercepts)
    # empty df to fill with assigned new cases
  df.cases <- data.frame(matrix(NA, nrow(data), length(levels(data[,dv]))))   
    # assign cases that fall underneath the lowest intercept with the respective education category, put results in first column
  df.cases[,1] <- ifelse(plr.out$lp <= int.df$Values[1], levels(data[,dv])[1], NA) 
    # assign cases that fall between all intercepts except the lowest and the highest, put results in all but first and last columns
  for(q in 1:(length(levels(data[,dv]))-2)){        
   df.cases[,q+1] <- ifelse(plr.out$lp > int.df$Values[q] & 
                           plr.out$lp <= int.df$Values[q+1], levels(data[,dv])[q+1], NA)
  }
    # assign all cases that fall above the highest intercept, put in last column
  df.cases[,length(levels(data[,dv]))] <- ifelse(plr.out$lp > int.df$Values[length(levels(data[,dv]))-1],
                                                     levels(data[,dv])[length(levels(data[,dv]))], NA)
    # combine all columns, omit NAs, add column to df
  data[,paste(dv, ".new", sep = "")] <- factor(c(na.omit(c(t(df.cases)))))   
    # empty small df to extract numbers for the respective re-estimated categories
  df.factors.1 <- data.frame(matrix(NA, length(levels(data[,paste(dv, ".new", sep = "")])), 2))
    # for each remaining category, extract its name and match it with its respective number, add by rows
  for (w in 1:length(levels(data[,paste(dv, ".new", sep = "")]))){
    df.factors.1[w,] <- c(unique(data[,paste(dv, ".num", sep = "")][data[,dv] == levels(data[,paste(dv, ".new", sep = "")])[w]]),
                        levels(data[,paste(dv, ".new", sep = "")])[w])
  }
    # empty long df to set up numbers for re-estimated categories for entire data set
  df.factors.2 <- data.frame(matrix(NA, nrow(data), nrow(df.factors.1)))
    # assign numbers, add by columns
  for (n in 1:nrow(df.factors.1)){
   df.factors.2[,n] <- ifelse(data[,paste(dv, ".new", sep = "")] == df.factors.1[n,2], df.factors.1[n,1], NA)
  }
    # combine all columns, omit NAs, add column to df
  data[,paste(dv, ".new.num", sep = "")] <- as.numeric(na.omit(c(t(df.factors.2))))
    # refactor levels in the order of the numbers
  data[,paste(dv, ".new", sep = "")] <- factor(data[,paste(dv, ".new", sep = "")], 
                                               levels = unique(data[,paste(dv, ".new", sep = "")][order(data[,paste(dv, ".new.num", sep = "")])]))
    # make numbers for .new.num column start at 1, based on newly refactored .new levels
  data[,paste(dv, ".new.num", sep = "")] <- as.numeric(data[,paste(dv, ".new", sep = "")])
    # have the function output data with all NAs, na.omit version of data, plr.out, plr.df, int.df, the new education levels, and the new numeric education levels
  output <- list("data.full.nas" = data.full.nas,
                 "data.short.na.omit" = data,
                 "plr.out" = plr.out,
                 "plr.df" = plr.df, 
                 "int.df" = int.df,
                 "ord.new.lev" = levels(data[,paste(dv, ".new", sep = "")]),
                 "ord.new.lev.num" = sort(unique(data[,paste(dv, ".new.num", sep = "")])))
  return(output)
}



## Everything except the int.df and df.factors stuff is very straightforward
## The int.df code assigns the binned cases according to the new thresholds and gives them the respective category names. I had to move the first and the last threshold outside of the loop because it needed different code, since the first falls beneath it and the last above it, but all others fall in-between
## The df.factors stuff is there for one stupid reason: The factor levels for education.new were not ordered the right way, substantively. So I had to come up with a way to order them, but of course R doesn't know that HS grad is supposed to be before Associate. So I needed code to order the levels of whatever categories remained. The only thing I could think of was to add an education.num vector when I set up df, extract the numbers for whatever re-estimated categories remain, create a long vector with those numbers and assign it to the df, and finally reorder the category factor levels based on these numbers. 12 lines of code for such a seemingly simple thing

library(here)

# save("OPMord", file = here("functions", "OPMord.Rdata"))
# can be loaded into any R session with load(here("functions", "OPMord.Rdata"))

data <- readRDS(here("data", "anes_cleaned.rds"))  # created in "opm_create_model.R"
dv <- "education"
evs <- c("age", "gender", "race", "income", "occupation", "pid")
fg <- OPMord(data = data, dv = dv, evs = evs)
names(fg)







### Original code I had before I developed the neater function (keeping it just in case) ###

# # run the polr() model and store polr() output 
# plr <- polr(education ~ age + gender + race + income + occupation + pid, data = df, Hess=TRUE)
# # turn plr.out output into a data frame
# coef.sum <- data.frame(coef(summary(plr)))
# # empty vector for storage
# empty <- c()
# # loop to fill empty vector with threshold names (i.e. "Up to 1st|1st-4th" etc.)
# for (i in 1:(length(levels(df$education))-1)){
#   empty[i] <- paste(levels(df$education)[i], "|", levels(df$education)[i+1], sep = "")
#   }                                                                        
# # select and store only rows with threshold names
# coef.int <- coef.sum[empty,]                                      
# # turn row names into a column
# coef.int <- setDT(coef.int, keep.rownames = TRUE)[]
# # removes class "data.table", which was added by setDT
# coef.int <- data.frame(coef.int)
# colnames(coef.int) <- c("Thresholds", "Coefficients", "SE", "t-values")
# # round every numeric column to 4 decimals
# coef.int[,c("Coefficients", "SE", "t-values")] <- round(coef.int[,c("Coefficients", "SE", "t-values")], 4)
# # factorize Thresholds with correctly ordered levels
# coef.int$Thresholds <- factor(coef.int$Thresholds, levels = coef.int$Thresholds)
# # plot and save Coefficients/Thresholds visualization with horizontal error bars of SE
# ggplot(coef.int, aes(Coefficients, Thresholds)) + geom_point(size = 1.5) + 
#   geom_errorbarh(aes(xmin=Coefficients-SE, xmax=Coefficients+SE), height = .3) + ggtitle("Ordered Thresholds for Education Categories#")
# ggsave(paste(wd, "/plots/thresholds.pdf", sep = ""), height=7, width=7, units='in')
# # save .csv with Thresholds, Coefficients, SEs, t-values
# write.csv(coef.int, file = paste(wd, "/data/thresholds.csv", sep = ""), row.names = FALSE)
# ## the next lines are to create and store fake data (sim.df), use predict() and store that data (pred.df), which I don't need any #more (for now)
# # sim.df <- data.frame(                                            
# #  age = sample(18:90, 100, replace = TRUE),
# #  gender = factor(sample(levels(df$gender), 100, replace = TRUE)),
# #  race = factor(sample(levels(df$race), 100, replace = TRUE)),
# #  income = factor(sample(levels(df$income), 100, replace = TRUE)), 
# #  occupation = factor(sample(levels(df$occupation), 100, replace = TRUE)),
# #  pid = factor(sample(levels(df$pid), 100, replace = TRUE)))
# # pred.df <- cbind(sim.df, predict(plr, sim.df, type = "class"))
# # pred.df.melt[[x]] <- melt(pred.df, id.vars = colnames(sim.df), variable.name = "Level", value.name = "Probability")
# # round all the binned cases in the lp column to 4 decimals
# plr$lp <- round(plr$lp, 4)      
# # empty df to fill with assigned new cases
# df.ints <- data.frame(matrix(NA, nrow(df), length(levels(df$education))))   
# # assign cases that fall underneath the lowest threshold with the respective education category, put results in first column
# df.ints[,1] <- ifelse(plr$lp <= coef.int$Coefficients[1], levels(df$education)[1], NA) 
# # assign cases that fall between all thresholds except the lowest and the highest, put results in all but first and last columns
# for(q in 1:(length(levels(df$education))-2)){        
#   df.ints[,q+1] <- ifelse(plr$lp > coef.int$Coefficients[q] & 
#                          plr$lp <= coef.int$Coefficients[q+1], levels(df$education)[q+1], NA)
# }
# # assign all cases that fall above the highest threshold, put in last column
# df.ints[,length(levels(df$education))] <- ifelse(plr$lp > coef.int$Coefficients[length(levels(df$education))],
#                                                     levels(df$education)[length(levels(df$education))], NA)
# # combine all columns, omit NAs, add column to df
# df$education.new <- factor(c(na.omit(c(t(df.ints)))))   
# # empty small df to extract numbers for the respective re-estimated categories (numbers are based on the original df.9 and df #creation)
# df.factors.1 <- data.frame(matrix(NA, length(levels(df$education.new)), 2))
# # for each remaining category, extract its name and match it with its respective number, add by rows
# for (w in 1:length(levels(df$education.new))){
#   df.factors.1[w,] <- c(unique(df$education.num[df$education == levels(df$education.new)[w]]),
#                        levels(df$education.new)[w])
# }
# # empty long df to set up numbers for re-estimated categories for entire data set
# df.factors.2 <- data.frame(matrix(NA, nrow(df), nrow(df.factors.1)))
# # assign numbers, add by columns
# for (n in 1:nrow(df.factors.1)){
#   df.factors.2[,n] <- ifelse(df$education.new == df.factors.1[n,2], df.factors.1[n,1], NA)
# }
# # combine all columns, omit NAs, add column to df
# df$education.new.num <- as.numeric(na.omit(c(t(df.factors.2))))
# # refactor levels in the order of the numbers
# df$education.new <- factor(df$education.new, 
#                                      levels = unique(df$education.new[order(df$education.new.num)]))
# # plot and save distribution of original education categories
# ggplot(df, aes(x = education)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "darkred") + ylab("Percentages") + xlab("Education") + 
#    ggtitle("Distribution of Original Education Categories") + theme(plot.title = element_text(hjust = 0.5))
# ggsave(paste(wd, "/plots/barplot_orig.pdf", sep = ""), height=7, width=7, units='in')
# # plot and save distribution of re-estimated education categories
# ggplot(df, aes(x = education.new)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "darkred") + ylab("Percentages") + xlab("Education") +
#    ggtitle("Distribution of OPM Education Categories") + theme(plot.title = element_text(hjust = 0.5))
# ggsave(paste(wd, "/plots/barplot_new.pdf", sep = ""), height=7, width=7, units='in')
# saveRDS(df, file = paste(wd, "/data/anes_education.rds", sep = ""))
 
