
# drag the two list.df files over to Jeff before running
# loading code below is adjusted (no need for wd and subfolder paths)

library(blockTools)
library(here)

test.ols.trump.orig <- test.ols.trump.new <- readRDS(here("data", "anes_education.rds"))

test.ols.trump.new$education = test.ols.trump.new$education.new # so that I can run the rest of the code easily in a loop
list.test.ols.trump <- list(test.ols.trump.orig, test.ols.trump.new)
listing.temp <- list()

pb <- txtProgressBar(min = 1, max = length(list.test.ols.trump), style = 3) # creates the percentage progress bar across the sampled numbers

for (i in 1:length(list.test.ols.trump)){
  setTxtProgressBar(pb, i) # loads the percentage progress bar into the loop
  list.test.ols.trump[[i]]$education <- as.numeric(list.test.ols.trump[[i]]$education)
  list.test.ols.trump[[i]]$id <- 1:nrow(list.test.ols.trump[[i]])
  listing.temp[[i]] <- assignment(block(list.test.ols.trump[[i]], n.tr = 2, id.vars = c("id"), block.vars = c("education")), seed = 123)
  list.test.ols.trump[[i]]$treat1 <- ifelse(list.test.ols.trump[[i]]$id %in% which(list.test.ols.trump[[i]]$id %in% unname(listing.temp[[i]]$assg[[1]][,1])), "treat1", NA)
  list.test.ols.trump[[i]]$treat2 <- ifelse(list.test.ols.trump[[i]]$id %in% which(list.test.ols.trump[[i]]$id %in% unname(listing.temp[[i]]$assg[[1]][,2])), "treat2", NA)
  list.test.ols.trump[[i]]$group <- factor(c(na.omit(c(t(list.test.ols.trump[[i]][,c("treat1", "treat2")])))))
  list.test.ols.trump[[i]]$rep <- ifelse(list.test.ols.trump[[i]]$pid == "Republican", 1, 0)
  list.test.ols.trump[[i]]$dem <- ifelse(list.test.ols.trump[[i]]$pid == "Democrat", 1, 0)
  list.test.ols.trump[[i]]$male <- ifelse(list.test.ols.trump[[i]]$gender == "Male", 1, 0)
  list.test.ols.trump[[i]]$white <- ifelse(list.test.ols.trump[[i]]$race == "White", 1, 0)
  list.test.ols.trump[[i]]$black <- ifelse(list.test.ols.trump[[i]]$race == "African-American", 1, 0)
  list.test.ols.trump[[i]]$hisp <- ifelse(list.test.ols.trump[[i]]$race == "Hispanic", 1, 0)
  list.test.ols.trump[[i]]$income.num <- as.numeric(list.test.ols.trump[[i]]$income)
  saveRDS(list.test.ols.trump[[i]], file = here("data", paste0("list_test_ols_trump[[", i, "]].rds")))
  }

