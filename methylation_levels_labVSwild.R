library(brms)
epiage_methylation 
model <- brm(
  Meth_level ~ Source + Body_mass_g + (1 | Animal_ID) + (1 | Gene/Position), # Meth_level is a column with methylation proportion for a CpG site within a gene indicated in column Gene at position indicated in column Position
  data = epiage_methylation,
  family = zero_inflated_beta,
  warmup = 1000, iter = 4000, 
  chains = 4, cores = 1,
  control = list(adapt_delta = 0.98, max_treedepth = 13),
  init = 0
)