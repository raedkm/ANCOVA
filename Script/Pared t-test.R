
library("tidyverse")

glycolipid <- read.csv(file = "Input/GLYCOLIPID.CSV")


glimpse(glycolipid)





# Creating a difference column to do a t.test manually
glycolipid_diffs <- glycolipid %>%
  group_by(Patient) %>%
  summarise(Difference = diff(Glycolipid))

glycolipid_diffs

# Check for normal distribution
ggplot(glycolipid_diff, aes(x = Difference)) +
  geom_dotplot() + 
  theme_grey(base_size = 22)





#tstest
t.test(glycolipid_diff$Difference)

# Express: Individual patients had significantly lower serum glycolipid 
# concentrations when treated with Drug B than when treated 
# with Drug A (t = 2.62, d.f. = 7, p < 0.05).



# using the built in paired t.test 
t.test(Glycolipid ~ Drug, data = glycolipid, paired = T)
