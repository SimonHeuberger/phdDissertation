
# The table I created in opm_test_model is created with a placebo treatment, i.e. there is no real treatment. That means Group T2 should be zero. The table is not evidence that the OP method is better
# This repeat the estimations for the table 100 times and visualizes the distribution of the Group T2 coefficients. This will show which category estimation is closer to zero, which is the true value of that coefficient


library(blockTools)
library(ggplot2)
library(here)

test.ols.trump <- readRDS(here("data", "anes_education.rds")) 

set.seed(123) # set.seed() needs to be above sample() here -- it can be anywhere before the loop in the .R for all observations as I don't sample there

test.ols.trump <- test.ols.trump[sample(nrow(test.ols.trump), 100), ]
test.ols.trump$id <- 1:nrow(test.ols.trump)
test.ols.trump$rep <- ifelse(test.ols.trump$pid == "Republican", 1, 0)
test.ols.trump$dem <- ifelse(test.ols.trump$pid == "Democrat", 1, 0)
test.ols.trump$male <- ifelse(test.ols.trump$gender == "Male", 1, 0)
test.ols.trump$white <- ifelse(test.ols.trump$race == "White", 1, 0)
test.ols.trump$black <- ifelse(test.ols.trump$race == "African-American", 1, 0)
test.ols.trump$hisp <- ifelse(test.ols.trump$race == "Hispanic", 1, 0)
test.ols.trump$income.num <- as.numeric(test.ols.trump$income)
test.ols.trump$education.num <- as.numeric(test.ols.trump$education.num)
test.ols.trump$education.new.num <- as.numeric(test.ols.trump$education.new.num)

test.ols.trump.orig <- subset(test.ols.trump, select = c(id, feel.trump, dem, rep, male, white, black, hisp, income.num, age, education, education.num))
test.ols.trump.new <- subset(test.ols.trump, select = c(id, feel.trump, dem, rep, male, white, black, hisp, income.num, age, education.new, education.new.num))

test.ols.trump.new$education = test.ols.trump.new$education.new # so that it's all named equally
test.ols.trump.new$education.num = test.ols.trump.new$education.new.num 

list.ols.trump <- list(test.ols.trump.orig, test.ols.trump.new)
list.list.assgn <- list.list.lm.out <- list.list.gt2.coeff <-  rep(list(list()), length(list.ols.trump))
repeats <- 100

pb <- txtProgressBar(min = 1, max = repeats, style = 3) # creates the percentage progress bar across the sampled numbers

for(x in 1:length(list.ols.trump)){
  for(i in 1:repeats){
    setTxtProgressBar(pb, i) # loads the percentage progress bar into the loop
    list.list.assgn[[x]][[i]] <- assignment(block(list.ols.trump[[x]], n.tr = 2,
                                                  id.vars = c("id"), block.vars = c("education.num")))
    list.ols.trump[[x]]$treat1 <- ifelse(list.ols.trump[[x]]$id %in% which(list.ols.trump[[x]]$id %in% 
                                                                             unname(list.list.assgn[[x]][[i]]$assg[[1]][,1])), "treat1", NA)
    list.ols.trump[[x]]$treat2 <- ifelse(list.ols.trump[[x]]$id %in% which(list.ols.trump[[x]]$id %in% 
                                                                             unname(list.list.assgn[[x]][[i]]$assg[[1]][,2])), "treat2", NA)
    list.ols.trump[[x]]$group <- factor(c(na.omit(c(t(list.ols.trump[[x]][,c("treat1", "treat2")])))))
    list.list.lm.out[[x]][[i]] <- lm(feel.trump ~ group + dem + rep + male + white + black + hisp + income.num + age, data = list.ols.trump[[x]])
    list.list.gt2.coeff[[x]][[i]] <- unname(list.list.lm.out[[x]][[i]]$coefficients[2])
  }  
}

gt2.orig <- unlist(list.list.gt2.coeff[[1]], recursive = FALSE)
gt2.new <- unlist(list.list.gt2.coeff[[2]], recursive = FALSE)

gt2.orig.df <- data.frame(cbind(gt2.orig, rep("original", length(gt2.orig))))
gt2.new.df <- data.frame(cbind(gt2.new, rep("ordinal probit", length(gt2.new))))
colnames(gt2.orig.df) <- colnames(gt2.new.df) <- c("coefficient", "categories")
gt2.df <- rbind(gt2.orig.df, gt2.new.df)
gt2.df$coefficient <- as.numeric(as.character(gt2.df$coefficient))

pdf(here("plots", paste0("ols_trump_gt2_coeff_100_obs_", repeats, "_runs.pdf")))
ggplot(gt2.df, aes(x=coefficient, fill=categories)) + geom_density(alpha=0.2, aes(y=..density..), position="identity") + xlab("Regression Coefficients for Placebo Treatment Group") + ylab("Density") + theme(legend.title=element_blank()) + theme(legend.position = c(0.85, 0.75)) + ggtitle("Distribution of Placebo Treatment Coefficients By Education Model") + theme(plot.title = element_text(hjust = 0.5))
dev.off()


