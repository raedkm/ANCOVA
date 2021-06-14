

library("tidyverse")

corn_crake <- read.csv(file = "Input/CORN_CRAKE.CSV")

glimpse(corn_crake)

ggplot(corn_crake, aes(x = Supplement, y = WeightGain)) +
  geom_boxplot()

corncrake_model <- lm(WeightGain ~ Supplement, data = corn_crake)

anova(corncrake_model)


corncrake_stats <- 
  corn_crake %>% 
  group_by(Supplement) %>% 
  summarise(Mean = mean(WeightGain), SE = sd(WeightGain)/sqrt(n()))



ggplot(data = corncrake_stats, 
       aes(x = Supplement, y = Mean, ymin = Mean - SE, ymax = Mean + SE)) + 
  # this adds the means
  geom_point(colour = "blue", size = 3) + 
  # this adds the error bars
  geom_errorbar(width = 0.1, colour = "blue") + 
  # controlling the appearance
  scale_y_continuous(limits = c(0, 30)) + 
  # use sensible labels
  xlab("Supplement treatment") + ylab("Weight gain (g)") +
  # flip x and y axes
  coord_flip() +
  # use a more professional theme
  theme_bw()


# Change order to make "None" first in the plot
corncrake_stats <- 
  corncrake_stats %>% 
  mutate(Supplement = factor(Supplement, levels = c("None", "Sizefast", "Linseed", "Allvit", "Earlybird")))



# creating a bar plot
ggplot(data = corncrake_stats, 
       aes(x = Supplement, y = Mean, ymin = Mean - SE, ymax = Mean + SE)) + 
  # this adds the means
  geom_col(fill = "lightgrey", colour = "grey") + 
  # this adds the error bars
  geom_errorbar(width = 0.1, colour = "black") + 
  # controlling the appearance
  scale_y_continuous(limits = c(0, 30)) + 
  # use sensible labels
  xlab("Supplement treatment") + ylab("Weight gain (g)") + 
  # use a more professional theme
  theme_bw()
