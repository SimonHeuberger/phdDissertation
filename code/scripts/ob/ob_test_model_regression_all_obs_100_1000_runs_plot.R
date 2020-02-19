
library(ggplot2)
library(here)

#repeats <- 100
repeats <- 1000

list.list.gt2.coeff <- readRDS(here("data", paste0("list_list_test_ols_trump_gt2_coeff_all_obs_", repeats, "_runs.rds")))

gt2.orig <- unlist(list.list.gt2.coeff[[1]], recursive = FALSE)
gt2.new <- unlist(list.list.gt2.coeff[[2]], recursive = FALSE)

gt2.orig.df <- data.frame(cbind(gt2.orig, rep("original", length(gt2.orig))))
gt2.new.df <- data.frame(cbind(gt2.new, rep("ordinal probit", length(gt2.new))))
colnames(gt2.orig.df) <- colnames(gt2.new.df) <- c("coefficient", "categories")
gt2.df <- rbind(gt2.orig.df, gt2.new.df)
gt2.df$coefficient <- as.numeric(as.character(gt2.df$coefficient))

pdf(here("plots", paste0("ols_trump_gt2_coeff_all_obs_", repeats, "_runs.pdf")))
ggplot(gt2.df, aes(x=coefficient, fill=categories)) + geom_density(alpha=0.2, aes(y=..density..), position="identity") + xlab("Regression Coefficients for Placebo Treatment Group") + ylab("Density") + theme(legend.title=element_blank()) + theme(legend.position = c(0.85, 0.75)) + ggtitle("Distribution of Placebo Treatment Coefficients By Education Model") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

tapply(gt2.df$coefficient, gt2.df$categories, mean)
tapply(gt2.df$coefficient, gt2.df$categories, median)

