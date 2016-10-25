# Philip's demonstration of ANOVA in R issues, using web size data set
# Part 1: use of lsmeans package: my recommended approach
# If you are knowledgeable and careful, 
#   Anova() in the car package will provide correct answers.
# Part 2 shows how to get "by hand" correct analyses,
#   shows that various "simple" approaches are wrong and
#   explains how to correctly use Anova() in car

# none of these issues relevant when data are balanced (equal for all cells)
#   (or the infrequent proportional sample sizes)

# In general, data are unbalanced, for many reasons, so have to know how to 
#   get correct analyses from unbalanced data

# my use of "correct analysis" = an analysis that answers:
#   is there an interaction
#   what are the differences between levels of A, averaged over B
#   what are the differences between levels of B, averaged over A

# some folks call my perspective "cell means analysis"
# i.e. what is done to an observation is the combination of factor levels
#   so the analysis should start there

---------------------------------------
# properties of the data set

# Is the data set unbalanced?

# data set is saved in the transplant data frame, created by Spider_WebSize_Analysis.R

transplant <- read.csv('data/working/transplant.csv', as.is=T)

cellN <- with(transplant, table(Island, Native))
cellN

# for reference, what are the cell means?
cellMean <- with(transplant, tapply(websize, list(Island, Native), mean))
cellMean

--------------------------------------
# Use of lsmeans package

transplant$Island <- factor(transplant$Island)
transplant$Native<- factor(transplant$Native)

websize.lm1 <- lm(websize ~ Island + Native + Island:Native, data=transplant)
# need to fit a linear model first
# this defines the factors and the cells in the model

library(lsmeans)

websize.grid1 <- ref.grid(websize.lm1)
# start by creating a reference grid: essentially the cell structure
# the grid object contains the model and the data

websize.grid1

summary(websize.grid1)
#  gives cell means for each combination of factor levels

--------------
# computing lsmeans

lsmeans(websize.grid1, "Island")
lsmeans(websize.grid1, "Native")
# Specify the grid and the desired means
#   get estimates, with se and ci's
# The note about interactions is to remind you that simple effects may not equal the 
#   main effect

# tests of differences:
# use pairs() after saving the lsmeans result
websize.isl <- lsmeans(websize.grid1, "Island")
pairs(websize.isl)

# lots of other types of comparisons are possible
contrast(websize.isl, 'trt.vs.ctrl1')

# the full list of comparisons is given by
#   ls("package:lsmeans", pat=".lsmc")
# pairs() is a shortcut for pairwise contrasts

# can calculate both the lsmeans and contrasts at once
lsmeans(websize.grid1, "Island", contr="pairwise")
lsmeans(websize.grid1, "Native", contr="pairwise")
# pairwise differences are in the $contrasts part of the result

# cld() gives a nice graphic of the group differences
#   but a necessary library, multcompview, is not available for R 3.3

#  to get the overall F test:
# save the pairwise differences and do a joint test that all = 0

pairs.isl <- pairs(websize.isl)
test(pairs.isl, joint=T)
#  test that all pairwise differences = 0, which is the overall F test
# General advice: check df1 = numerator df.
#   make sure it is what you expect

# Warning: If you test the lsmeans = 0, you get a different test
test(websize.isl, joint=T)

# tests Saipan mean = 0 and Guan mean = 0 (2 df), prob. not what you wanted.

# and how to test the interaction
# compute pairwise differences within each level of one factor

int.isl <- pairs(websize.grid1, by='Island')
int.isl
#  No-Yes results for each Island

int.isl2 <- update(int.isl, by=NULL)
int.isl2
#  convert to a table with 2 rows (from a list of two contrasts)

test(pairs(int.isl2), joint=T)
# which we can then compare using a joint test


----------------------------------
# demonstration of factorials with more than 2 levels

transplant2 <- subset(transplant, Site %in% c('forbi','ladt','nblas'))
# remove sites without both Native levels

transplant2$Site <- factor(transplant2$Site)
# and convert to a factor

# if Site was already a factor, need to fix the levels
# (Subset has only 3 sites, but levels still the original 5)
transplant2$Site <- factor(as.character(transplant2$Site))
# this is one way to reset the levels

websize.lm3 <- lm(websize ~ Site + Native + Site:Native, data=transplant2)
websize.grid3 <- ref.grid(websize.lm3)

websize.site <- lsmeans(websize.grid3, "Site")
summary(websize.site)

# differences between each pair of sites

pairs(websize.site)
# pairwise comparisons, using Tukey mult. comp. adjustment

pairs(websize.site, adjust="none")
# if you really want no adjustment

# overall F test
test(pairs(websize.site), joint=T)

websize.sitenative <- lsmeans(websize.grid3, "Native")
test(pairs(websize.sitenative), joint=T)


# matches anova using contr.sum contrasts and Anova in car()
# this is described at the end of part 2
contrasts(transplant2$Site) <- contr.sum(3)
contrasts(transplant2$Native) <- contr.sum(2)
websize.lm4 <- lm(websize ~ Site*Native, data=transplant2)
Anova(websize.lm4, type=3)

# and the interaction of Site * Native
int.site <- pairs(websize.grid3, by="Site")
int.site2 <- update(int.site, by=NULL)
test(pairs(int.site2), joint=T)

-------------------------------
# why you needed to omit two sites with missing cells

transplant$Site <- factor(transplant$Site)
websize.lm5 <- lm(websize ~ Site + Native + Site:Native,
  data=transplant)

websize.grid5 <- ref.grid(websize.lm5)
websize.grid5

# aside: anova() appears to give tests
anova(websize.grid5)

# BUT meaningless!!!  
# notice that df for interaction is not what you expect
#   that's a signature of missing cells

# lsmeans is more aware of the missing cell issue
lsmeans(websize.grid5, "Site")
lsmeans(websize.grid5, "Native")
#  Notice the NA's for non-estimable marginal means

full.site <- lsmeans(websize.grid5, "Site")
test(pairs(full.site), joint=T)
# and the test is NA 
# SAS telss you the marginal means are non-estimable 
#   but gives you type III tests anyway


