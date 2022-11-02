# Assumptions and diagnostics





# Regression diagnostics
library("tidyverse")




patridge <- read_csv("Input/PARTRIDG_BIGSTUDY.CSV")

glimpse(patridge)

plot(patridge)



# Regression assumptions --------------------------------------------------

## Assumptions of a regression

# 1.Independence
# 2.Measurement scale
# 3.Linearity
# 4.Constant variance
# 5.Normality
# 6.Measurement error


partridge_model <- lm(Partridge ~ Hedgerow, data = patridge)

plot(fitted(partridge_model))


# Checking the linearity assumption

plt_data <- data.frame(Fitted = fitted(partridge_model), 
                       Resids =  resid(partridge_model))
                       
ggplot(plt_data, aes(x = Fitted, y = Resids)) + 
  geom_point() + 
  xlab("Fitted values") + ylab("Residuals")



# Checking the normality assumption

mod_resids <- resid(partridge_model)
mod_resids <- mod_resids / sd(mod_resids)


resid_order <- order(mod_resids)
resid_order


all_resids <- qqnorm(mod_resids, plot.it = FALSE)
all_resids <- as.data.frame(all_resids)


head(all_resids, 10)


ggplot(all_resids, aes(x = x, y = y)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1) +
  xlab("Theoretical Value") + ylab("Standardised Residual")


# Checking the constant variance assumption

# extract the residuals
sqrt_abs_resids <- resid(partridge_model)
# step 1. standardise them
sqrt_abs_resids <- sqrt_abs_resids / sd(sqrt_abs_resids)
# step 2. find their absolute value
sqrt_abs_resids <- abs(sqrt_abs_resids)
# step 3. square root these
sqrt_abs_resids <- sqrt(sqrt_abs_resids)

plt_data <- 
  data.frame(Fitted = fitted(partridge_model), Resids = sqrt_abs_resids)

ggplot(plt_data, aes(x = Fitted, y = Resids)) + 
  geom_point() + 
  xlab("Fitted values") + ylab("Square root of absolute residuals")








# ANOVA assumptions -------------------------------------------------------




# 1.Independence
# 2.Measurement scale
# 3.Equal variance
# 4.Normality




