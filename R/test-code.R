## test functions
library(tidyverse)

dat <- read_csv("../infectious_disease_predictability/Data/CHLAMYDIA_Cases_2006-2014_20160707103149.csv", skip=2, na = "-")
x_all <- dat$DELAWARE
## fix NAs with mean imputation
na_idx <- which(is.na(x_all))
x_all[na_idx] <- (x_all[na_idx - 1]+x_all[na_idx + 1])/2


forecast_from_one_time(x_fit = x_all[1:300],
                       x_i = get_delay_vector(x_all, i= 350, tau=1, m=5),
                       T=3, tau=1, m=5, k=10)
