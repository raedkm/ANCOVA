# Assumptions and diagnostics


## Assumptions of a regression
# 1.Independence
# 2.Measurement scale
# 3.Linearity
# 4.Constant variance
# 5.Normality
# 6.Measurement error


# Regression diagnostics


patridge <- read_csv("Input/PARTRIDG_BIGSTUDY.CSV")

glimpse(patridge)

plot(patridge)


partridge_model <- lm(Partridge ~ Hedgerow, data = patridge)

plot(fitted(partridge_model))


