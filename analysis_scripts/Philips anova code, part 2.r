# Philip's demonstration of ANOVA in R issues, using web size data set
# Part 2: All for your information.
#   how to compute correct analysis by hand
#   shows how various simple analyses are wrong
#   and how to use Anova() in car.


---------------------------------------
# do the relevant comparisons "by hand"

# fit the default ANOVA model
websize.lm1 <- lm(websize ~ Island + Native + Island:Native, data=transplant)

# first extract residual sd and error df from the lm summary
sigma <- summary(websize.lm1)$sigma
df <- summary(websize.lm1)$df[2]

# then calculate estimate and se for each comparison

# interaction is difference of differences
intEst <- (cellMean[1,1] - cellMean[1,2]) - (cellMean[2,1] - cellMean[2,2])
intSe <- sigma*sqrt(sum(1^1/cellN))

# estimate island averages = marginal means, rows are islands
$ the cell means approach defines the Saipan Island mean as the average
#   of the Saipan / No and Saipan / Yes cell means
#   NOT the average of all obs from Saipan

island <- apply(cellMean, 1, mean)
#  average for each row

native <- apply(cellMean, 2, mean)
#  average for each column

# and the se for each marginal mean
islandSe <- sigma*sqrt(apply(0.5^2/cellN, 1, sum))
nativeSe <- sigma*sqrt(apply(0.5^2/cellN, 2, sum))

# the 0.5 is because each marginal mean is the average of two cells
# NOTE: if the design is larger, the 0.5 changes to 1/# cells
#   e.g.: 3 islands: each native level (No or Yes) is average of 3 islands
#    so 0.5 becomes 0.33333333
#   and 4 "native" levels, each island is average of 4 cells,
#    so 0.5 becomes 0.25

# difference between island averages, rows are islands
mainIsland <- (cellMean[1,1] + cellMean[1,2])/2 - (cellMean[2,1] + cellMean[2,2])/2

#  or:
mainIsland <- island[1] - island[2]
names(mainIsland) <- NULL    # which gets rid of misleading label

# and it's se
mainIslandSe <- cellSd*sqrt(sum(0.5^2/cellN))

# or:
mainIslandSe <- sqrt(sum(islandSe^2))

# difference between treatment averages, cols are treatments
mainNative <- (cellMean[1,1] + cellMean[2,1])/2 - (cellMean[1,2] + cellMean[2,2])/2
mainNativeSe <- cellSd*sqrt(sum(0.5^2/cellN))

# collect estimate and se to do all three tests

ests <- c(Island=mainIsland, Native=mainNative, Int=intEst)
ses <- c(mainIslandSe, mainNativeSe, intSe)

ts <- ests/ses

# pvalues are twice the upper tail prob of | t |
pvals <- 2*pt(abs(ts), df, lower=F)

# and print the results nicely
cbind(Est = ests, se=ses, T=ts, p=round(pvals, 4))

-----------------------------------------
--------------------------------
# Now compare to various other approaches, most of which DON'T WORK!

# default ANOVA

anova(websize.lm1)
# interaction ok, main effects are not - because these tests are sequential
# match SAS type I SS

--------------------------------

# using type III Anova in car library
library(car)

Anova(websize.lm1, type=3)
# I don't know what these represent

# refitting the anova with an orthogonal contrast set
#  i.e. contr.sum or contr.helmert

contrasts(transplant$Island) <- contr.sum(2)
contrasts(transplant$Native) <- contr.sum(2)
# set the contrasts for each factor to "sum to zero" with 2 levels 
#   the number is the number of levels
# these have values of -1, 0, or +1 for each level

websize.lm2 <- lm(websize ~ Island + Native + Island:Native, data=transplant)
summary(websize.lm2)

Anova(websize.lm2, type=3)
# now the type III tests are the correct tests

anova(websize.lm2)
# but you still have to use Anova in the car library

------------------------------

# what about the functions in lmerTest?

library(lmerTest)
lmerTest::anova(websize.lm1)

# gives same as stats::anova(websize.lm1)
# trying lmerTest::anova(websize.lm1, type=3)
#   gives an error when object is not an lmer result

