#setwd()

library(MASS)
library(car)
library(tibble)
library(MVN)
library(randomForest)
library(caret)
library(tidyr)

set.seed(27)
################################################################################
### Read in data ###############################################################
### read in training dataset with full measurements and species known
seeds_full <- read.csv(file ="BW_full_measurements.csv", sep = ";")       
seeds <- seeds_full
# delete unused columns
seeds[1] <- NULL
seeds[3:6] <- NULL
seeds[4:5] <- NULL
seeds[12:18] <- NULL
seeds$Species <- substring(seeds$Tag,1,4)
seeds$Species <- as.factor(seeds$Species)
seeds$Site <- substring(seeds$Tag,6,9)
seeds$Site <- as.factor(seeds$Site)
seeds[1] <- NULL

### read in data to be classified (measurements there, but species unknown)
scans_full <- read.csv(file ="Measurements.csv")  
scans <- scans_full
# delete unused columns to end up with the same columns that are in the training set
scans[1] <- NULL
scans[3:6] <- NULL
scans[4:5] <- NULL
scans[12:18] <- NULL
scans$Site <- substring(scans$Label,5,10)
scans$Site <- as.factor(scans$Site)
reference <- as.data.frame(scans$Label)
reference <- rownames_to_column(reference)

levels(scans$Site) <- levels(seeds$Site)

################################################################################
### Random Forest ##############################################################
# train Random Forest
rf <- randomForest(Species~., data=seeds, proximity=TRUE)

# use Random Forest to predict unknown species
pred <- as.data.frame(predict(rf, scans))
pred <- rownames_to_column(pred)

################################################################################
### Scan summary and evaluation ################################################
### prepare dataframe
pred <- merge(pred, reference, "rowname")
predictions <- pred
predictions$rowname <- NULL
names(predictions) <- c('pred_species', 'Tag')
predictions <- as.data.frame(table(predictions))
predictions <- pivot_wider(predictions, names_from = pred_species, values_from = Freq)

# this copes with the two scans per seed-load. If more or less scans are used, adjust here
m2 <- subset(predictions, as.logical(grepl("_m2_", predictions$Tag)))
m1 <- subset(predictions, as.logical(grepl("_m1_", predictions$Tag)))
m2$Tag <- sub('_m2_', '_', m2$Tag)   
m1$Tag <- sub('_m1_', '_', m1$Tag)   

# compute metric of how similar both 'equal' scans were classified (can be modified for different metrics)
accuracies <- merge(m1, m2, "Tag")
    # absolute difference
accuracies$discrepancy <- ((accuracies$ABAM.x - accuracies$ABAM.y)^2) +         
  ((accuracies$CANO.x - accuracies$CANO.y)^2) + ((accuracies$PSME.x - accuracies$PSME.y)^2) + 
  ((accuracies$THPL.x - accuracies$THPL.y)^2) + ((accuracies$TSHE.x - accuracies$TSHE.y)^2) + 
  ((accuracies$TSME.x - accuracies$TSME.y)^2)
    # difference relative to number of seeds on scan
accuracies$sum <- apply(accuracies[,c(2:13)], 1, sum)
accuracies$sum <- floor(accuracies$sum/2)
accuracies$relative_dicrepancy <- accuracies$discrepancy/accuracies$sum
accuracies$sum <- NULL

# come up with one prediction per mix (can be modified to other metrics)
accuracies$ABAM <- floor((accuracies$ABAM.x + accuracies$ABAM.y)/2)            
accuracies$CANO <- floor((accuracies$CANO.x + accuracies$CANO.y)/2)
accuracies$PSME <- floor((accuracies$PSME.x + accuracies$PSME.y)/2)
accuracies$THPL <- floor((accuracies$THPL.x + accuracies$THPL.y)/2)
accuracies$TSHE <- floor((accuracies$TSHE.x + accuracies$TSHE.y)/2)
accuracies$TSME <- floor((accuracies$TSME.x + accuracies$TSME.y)/2)

# summary per seed trap (number of seeds from each species per seed trap)
summary <- subset(accuracies, select= c('Tag', 'ABAM', 'CANO', 'PSME', 'THPL', 
                                        'TSHE', 'TSME', 'discrepancy'))
summary$Tag <- substring(summary$Tag, 1, 28)
summary <- aggregate(. ~ Tag, data = summary, FUN = sum)                       

