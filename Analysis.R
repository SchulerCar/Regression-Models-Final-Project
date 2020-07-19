rm(list=ls())
library(tidyverse)
data("mtcars")
mtcars <- as_tibble(mtcars)
mtcars

# Look for variables that are cross-correlated
library(GGally)
ggpairs(mtcars, lower = list(continuous = wrap("smooth", method = "lm", size=0.1)))


# Correlation matrix
data(mtcars)
corr <- round(cor(mtcars), 1)
library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)


# Coerce some variables into factors
mtcarsF<-mtcars
mtcarsF$cyl <-  factor(mtcarsF$cyl)
mtcarsF$vs <-   factor(mtcarsF$vs,labels = c("V block","S block"))
mtcarsF$am <-   factor(mtcarsF$am,labels = c("Automatic","Manual"))
mtcarsF$gear <- factor(mtcarsF$gear)
mtcarsF$carb <- factor(mtcarsF$carb)
mtcarsF

ggplot(mtcarsF, aes(x=am,y=mpg)) +
        geom_boxplot(fill="lightblue") +
        labs(x="Transmission Type", y="MPG")

ggplot(mtcarsF, aes(x=am,y=mpg)) +
        geom_boxplot(aes(fill=factor(cyl))) +
        labs(x="Transmission Type", y="MPG", fill = "# cylinders")

#Create weight quantiles
wtMedian <- median(mtcarsF$wt)
lightWeight=paste("LIGHT,  weight<",wtMedian)
heavyWeight=paste("HEAVY, weight>",wtMedian)
mtcarsF <- mtcarsF %>% mutate(wtQ=factor(ntile(wt,2),labels=c(lightWeight,heavyWeight)))
rownames(mtcarsF)<-rownames(mtcars)

ggplot(mtcarsF, aes(x=am,y=mpg)) +
        geom_boxplot(aes(fill=factor(wtQ))) +
        labs(x="Transmission Type", y="MPG", fill="Weight")

mtcarsF <- mtcarsF %>% select(-wtQ)
baseModel <- lm(data=mtcarsF, mpg ~ .)
optimalModel <- step(baseModel, direction= "both")

newModel <- lm(data=mtcarsF, mpg ~ cyl + hp + wt + am + wt*am)
summary(newModel)
