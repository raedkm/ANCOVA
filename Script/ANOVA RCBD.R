# ANOVA for randomised block designs

library("tidyverse")

damsels <- read_csv("Input/DAMSELS.CSV")

glimpse(damsels)


# model fitting
damsels.model <- lm(Midge ~ Species + Block, data = damsels)

anova(damsels.model)


#The p-value gives the probability that the differences 
#between the set of means for each term in the model, or a more 
#extreme difference, could have arisen through sampling 
#variation under the null hypothesis of no difference.
#Here, there is a significant effect of block (p < 0.001), 
#which says that the density of midge larvae varies across 
#the lake. It looks like blocking was a good idea—there is a 
#lot of spatial (nuisance) variation in midge larvae density. 
#Of course what we actually care about is the damselfly 
#species effect. This main effect term is not significant (p > 0.05),
#so we conclude that there is no difference in the impact of the 
#predatory larvae of three damselfly species.




# Creating a one-way design to see what the effect is
damsels.oneway <- lm(Midge ~ Species, data = damsels)

anova(damsels.oneway)


#see the degrees of freedom for the residuals (compare it ot the previous model)
#The degrees of freedom are higher, this is good because this means
#we have more power to detect a significant difference among treatment groups
#However, the error sum of squares is also much higher when we ignore the block effect
#The F ratio is lower then the previous model, we have accounted for much less noise compared
#to when we included the block 
#In summary, designing a blocked experiment will likely icrease the power