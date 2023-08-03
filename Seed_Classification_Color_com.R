# setwd()

library(MASS)
library(car)
library(stringr)
library(MVN)
library(tibble)
library(dplyr)
library(mvoutlier)
library(klaR)
library(rpart)
library(heplots)
library(randomForest)
library(caret)

set.seed(27)

################################################################################ 
### Read in data ###############################################################
### Training data (species known)
colours_full <- read.csv() # csv-file containing the output of 'Wholw_Color_Macro_com.ijm' with known species
measures_full <- read.csv() # csv-file containing the output of 'Measurements_from_padded_color_com.ijm'
colours <- colours_full
measures <- measures_full
measures[1] <- NULL
colours[1] <- NULL

# prepping measures table; delete unused columns
# substring numbers may have to be adjusted according to scan (tif-file) naming sceme
measures$Species <- substring(measures$Tag,1,4)
measures$Species <- as.factor(measures$Species)
measures$Site <- substring(measures$Tag,6,9)
measures$Site <- as.factor(measures$Site)
measures$Skew <- NULL
measures$Kurt <- NULL

# prepping colour table
colours[8:9] <- NULL
colours$C_Space <- str_sub(colours$Label, -3, -1)
colours$Help <- rep(c("R", "G", "B", "L", "A", "B", "H", "S", "B"), times = )# insert number of length(colours)/9
cols <- c( 'C_Space' , 'Help')
colours$Layer <- apply( colours[ , cols ] , 1 , paste , collapse = "_" )
colours$Tag <- str_sub(colours$Label,1,nchar(colours$Label)-4)
colours <- unique(colours)
# subsets for restructuring
sub_RGB_R <- subset(colours, Layer == "RGB_R")
sub_RGB_G <- subset(colours, Layer == "RGB_G")
sub_RGB_B <- subset(colours, Layer == "RGB_B")
sub_HSB_H <- subset(colours, Layer == "HSB_H")
sub_HSB_S <- subset(colours, Layer == "HSB_S")
sub_HSB_B <- subset(colours, Layer == "HSB_B")
sub_LAB_L <- subset(colours, Layer == "LAB_L")
sub_LAB_A <- subset(colours, Layer == "LAB_A")
sub_LAB_B <- subset(colours, Layer == "LAB_B")
# rename columns
colnames(sub_RGB_R) <- paste(colnames(sub_RGB_R),"RGB_R",sep="_")
colnames(sub_RGB_G) <- paste(colnames(sub_RGB_G),"RGB_G",sep="_")
colnames(sub_RGB_B) <- paste(colnames(sub_RGB_B),"RGB_B",sep="_")
colnames(sub_HSB_H) <- paste(colnames(sub_HSB_H),"HSB_H",sep="_")
colnames(sub_HSB_S) <- paste(colnames(sub_HSB_S),"HSB_S",sep="_")
colnames(sub_HSB_B) <- paste(colnames(sub_HSB_B),"HSB_B",sep="_")
colnames(sub_LAB_L) <- paste(colnames(sub_LAB_L),"LAB_L",sep="_")
colnames(sub_LAB_A) <- paste(colnames(sub_LAB_A),"LAB_A",sep="_")
colnames(sub_LAB_B) <- paste(colnames(sub_LAB_B),"LAB_B",sep="_")
# delete unnecessary columns
sub_RGB_R[1:2] <- NULL
sub_RGB_G[1:2] <- NULL
sub_RGB_B[1:2] <- NULL
sub_HSB_H[1:2] <- NULL
sub_HSB_S[1:2] <- NULL
sub_HSB_B[1:2] <- NULL
sub_LAB_L[1:2] <- NULL
sub_LAB_A[1:2] <- NULL
sub_LAB_B[1:2] <- NULL
sub_RGB_R[5:8] <- NULL
sub_RGB_G[5:8] <- NULL
sub_RGB_B[5:8] <- NULL
sub_HSB_H[5:8] <- NULL
sub_HSB_S[5:8] <- NULL
sub_HSB_B[5:8] <- NULL
sub_LAB_L[5:8] <- NULL
sub_LAB_A[5:8] <- NULL
sub_LAB_B[5:8] <- NULL
# merge them together 
colour <- NULL
colour <- merge(sub_RGB_R, sub_RGB_G, by.x = "Tag_RGB_R", by.y = "Tag_RGB_G", indicator = TRUE)
colour <- merge(colour, sub_RGB_B, by.x = "Tag_RGB_R", by.y = "Tag_RGB_B")
colour <- merge(colour, sub_HSB_H, by.x = "Tag_RGB_R", by.y = "Tag_HSB_H")
colour <- merge(colour, sub_HSB_S, by.x = "Tag_RGB_R", by.y = "Tag_HSB_S")
colour <- merge(colour, sub_HSB_B, by.x = "Tag_RGB_R", by.y = "Tag_HSB_B")
colour <- merge(colour, sub_LAB_L, by.x = "Tag_RGB_R", by.y = "Tag_LAB_L")
colour <- merge(colour, sub_LAB_A, by.x = "Tag_RGB_R", by.y = "Tag_LAB_A")
colour <- merge(colour, sub_LAB_B, by.x = "Tag_RGB_R", by.y = "Tag_LAB_B")
# rename column tag            
colnames(colour)[1] <- "Tag"
# Combine color and measurement dataframe into one dataframe
seed_c <- merge(colour, measures, by = "Tag") 

### Data to be classified (species not known)
colours_scans <- read.csv() # csv-file containing the output of 'Wholw_Color_Macro_com.ijm'
measures_scans <- read.csv() # csv-file containing the output of 'Measurements_from_padded_color_com.ijm'
measures_scans[1] <- NULL
colours_scans[1] <- NULL

# prepping measures table; delete unused columns
measures_scans$Site <- substring(measures_scans$Tag,1,4)
measures_scans$Site <- as.factor(measures_scans$Site)
measures_scans$Skew <- NULL
measures_scans$Kurt <- NULL

# prepping colour table
colours_scans[8:9] <- NULL
colours_scans$C_Space <- str_sub(colours_scans$Label, -3, -1)
colours_scans$Help <- rep(c("R", "G", "B", "L", "A", "B", "H", "S", "B"), times = )# insert number of length(colours_scans)/9 
cols <- c( 'C_Space' , 'Help')
colours_scans$Layer <- apply( colours_scans[ , cols ] , 1 , paste , collapse = "_" )
colours_scans$Tag <- str_sub(colours_scans$Label,1,nchar(colours_scans$Label)-4)
colours_scans <- unique(colours_scans)
# subsets for restructuring
sub_RGB_R <- subset(colours_scans, Layer == "RGB_R")
sub_RGB_G <- subset(colours_scans, Layer == "RGB_G")
sub_RGB_B <- subset(colours_scans, Layer == "RGB_B")
sub_HSB_H <- subset(colours_scans, Layer == "HSB_H")
sub_HSB_S <- subset(colours_scans, Layer == "HSB_S")
sub_HSB_B <- subset(colours_scans, Layer == "HSB_B")
sub_LAB_L <- subset(colours_scans, Layer == "LAB_L")
sub_LAB_A <- subset(colours_scans, Layer == "LAB_A")
sub_LAB_B <- subset(colours_scans, Layer == "LAB_B")
# rename columns
colnames(sub_RGB_R) <- paste(colnames(sub_RGB_R),"RGB_R",sep="_")
colnames(sub_RGB_G) <- paste(colnames(sub_RGB_G),"RGB_G",sep="_")
colnames(sub_RGB_B) <- paste(colnames(sub_RGB_B),"RGB_B",sep="_")
colnames(sub_HSB_H) <- paste(colnames(sub_HSB_H),"HSB_H",sep="_")
colnames(sub_HSB_S) <- paste(colnames(sub_HSB_S),"HSB_S",sep="_")
colnames(sub_HSB_B) <- paste(colnames(sub_HSB_B),"HSB_B",sep="_")
colnames(sub_LAB_L) <- paste(colnames(sub_LAB_L),"LAB_L",sep="_")
colnames(sub_LAB_A) <- paste(colnames(sub_LAB_A),"LAB_A",sep="_")
colnames(sub_LAB_B) <- paste(colnames(sub_LAB_B),"LAB_B",sep="_")
# delete unnecessary columns
sub_RGB_R[1:2] <- NULL
sub_RGB_G[1:2] <- NULL
sub_RGB_B[1:2] <- NULL
sub_HSB_H[1:2] <- NULL
sub_HSB_S[1:2] <- NULL
sub_HSB_B[1:2] <- NULL
sub_LAB_L[1:2] <- NULL
sub_LAB_A[1:2] <- NULL
sub_LAB_B[1:2] <- NULL
sub_RGB_R[5:8] <- NULL
sub_RGB_G[5:8] <- NULL
sub_RGB_B[5:8] <- NULL
sub_HSB_H[5:8] <- NULL
sub_HSB_S[5:8] <- NULL
sub_HSB_B[5:8] <- NULL
sub_LAB_L[5:8] <- NULL
sub_LAB_A[5:8] <- NULL
sub_LAB_B[5:8] <- NULL
# merge them together 
colour_scans <- NULL
colour_scans <- merge(sub_RGB_R, sub_RGB_G, by.x = "Tag_RGB_R", by.y = "Tag_RGB_G", indicator = TRUE)
colour_scans <- merge(colour_scans, sub_RGB_B, by.x = "Tag_RGB_R", by.y = "Tag_RGB_B")
colour_scans <- merge(colour_scans, sub_HSB_H, by.x = "Tag_RGB_R", by.y = "Tag_HSB_H")
colour_scans <- merge(colour_scans, sub_HSB_S, by.x = "Tag_RGB_R", by.y = "Tag_HSB_S")
colour_scans <- merge(colour_scans, sub_HSB_B, by.x = "Tag_RGB_R", by.y = "Tag_HSB_B")
colour_scans <- merge(colour_scans, sub_LAB_L, by.x = "Tag_RGB_R", by.y = "Tag_LAB_L")
colour_scans <- merge(colour_scans, sub_LAB_A, by.x = "Tag_RGB_R", by.y = "Tag_LAB_A")
colour_scans <- merge(colour_scans, sub_LAB_B, by.x = "Tag_RGB_R", by.y = "Tag_LAB_B")
# rename column tag            
colnames(colour_scans)[1] <- "Tag"
# Combine color and measurement dataframe into one dataframe
seed_scans <- merge(colour_scans, measures_scans, by = "Tag")
reference <- as.data.frame(seed_scans$Tag)
reference <- rownames_to_column(reference)

################################################################################
### Random Forest ##############################################################
# train Random Forest
rf <- randomForest(Species~., data=seed_c, proximity=TRUE)

# use Random Forest to predict unknown species
pred <- as.data.frame(predict(rf, seed_scans))
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

# compute metric of how similar both 'equal' scans were classified
accuracies <- merge(m1, m2, "Tag")
accuracies$discrepancy <- ((accuracies$ABAM.x - accuracies$ABAM.y)^2) +         
  ((accuracies$CANO.x - accuracies$CANO.y)^2) + ((accuracies$PSME.x - accuracies$PSME.y)^2) + 
  ((accuracies$THPL.x - accuracies$THPL.y)^2) + ((accuracies$TSHE.x - accuracies$TSHE.y)^2) + 
  ((accuracies$TSME.x - accuracies$TSME.y)^2)

# come up with one prediction per mix
accuracies$ABAM <- floor((accuracies$ABAM.x + accuracies$ABAM.y)/2)            
accuracies$CANO <- floor((accuracies$CANO.x + accuracies$CANO.y)/2)
accuracies$PSME <- floor((accuracies$PSME.x + accuracies$PSME.y)/2)
accuracies$THPL <- floor((accuracies$THPL.x + accuracies$THPL.y)/2)
accuracies$TSHE <- floor((accuracies$TSHE.x + accuracies$TSHE.y)/2)
accuracies$TSME <- floor((accuracies$TSME.x + accuracies$TSME.y)/2)

# summary per seed trap (number of seeds from each species per seed trap)
summary <- subset(accuracies, select= c('Tag', 'ABAM', 'CANO', 'PSME', 'THPL', 
                                        'TSHE', 'TSME', 'discrepancy'))
names(summary)[names(summary) == 'Tag'] <- 'Trap'
summary$Trap <- substring(summary$Trap, 1, 6)
summary <- aggregate(. ~ Trap, data = summary, FUN = sum)                       


