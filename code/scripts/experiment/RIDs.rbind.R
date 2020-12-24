
library(here)

op.RIDs <- read.csv(here("data", "experiment", "RIDs.op.csv"), as.is = TRUE)
an.RIDs <- read.csv(here("data", "experiment", "RIDs.an.csv"), as.is = TRUE)

all.RIDs <- rbind(op.RIDs, an.RIDs)
nrow(all.RIDs)

write.csv(all.RIDs, file = here("data", "experiment", "RIDs.all.csv"), row.names = FALSE)


