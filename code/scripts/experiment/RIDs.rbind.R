
# combine all unique RIDs and send to Lucid for final invoicing
# won't run without the saved RIDs.op and RIDs.an .csv files (creation code commented out in clean.an.R and clean.op.R)

library(here)

op.RIDs <- read.csv(here("data", "experiment", "RIDs.op.csv"), as.is = TRUE)
an.RIDs <- read.csv(here("data", "experiment", "RIDs.an.csv"), as.is = TRUE)

all.RIDs <- rbind(op.RIDs, an.RIDs)
nrow(all.RIDs)

write.csv(all.RIDs, file = here("data", "experiment", "RIDs.all.csv"), row.names = FALSE)


