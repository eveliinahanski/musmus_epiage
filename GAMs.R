set.seed(5463)
library(mgcv)

data
linear_model <- gam(Epigenetic_age_Clock2 ~ Body_mass_g, data = data)
smoothed_model <- gam(Epigenetic_age_Clock2 ~ s(Body_mass_g), data = data)
linear_with_intercept <- gam(Epigenetic_age_Clock2 ~ Body_mass_g + Source, data = data)
smoothed_with_intercept <- gam(Epigenetic_age_Clock2 ~ s(Body_mass_g) + Source, data = data)
linear_with_interaction <- gam(Epigenetic_age_Clock2 ~ Body_mass_g * Source, data = data)
smoothed_with_interaction <- gam(Epigenetic_age_Clock2 ~ s(Body_mass_g, by = Source), data = data)