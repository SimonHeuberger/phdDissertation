

library(blockTools)
library(here)

test.ols.trump <- readRDS(here("data", "anes_education.rds")) 

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

#repeats <- 100
repeats <- 1000

pb <- txtProgressBar(min = 1, max = repeats, style = 3) # creates the percentage progress bar across the sampled numbers

set.seed(123)

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

saveRDS(list.list.gt2.coeff, file = here("data", paste0("list_list_test_ols_trump_gt2_coeff_all_obs_", repeats, "_runs.rds")))


