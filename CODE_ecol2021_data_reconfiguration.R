# Welcome to the survivors of the tornado that is my code -Sarah
#the usual
rm(list=ls())
getwd()
setwd("C:/Users/sarah/OneDrive/Documents/R/Fall2021")
getwd()

#package loading
library(dplyr)
library(ggplot2)

data <- read.csv("EcolSoy_v6.csv", header = TRUE, as.is = TRUE)
# na.strings = c('smth you want to be recog as NA', 'smth#2')   
# sets certain values to be read as NA

#some ways to look at the data
class(data) #checking if data frame
colnames(data)
str(data) #gives classes and some values for each column
data %>% summarise_all(n_distinct) #gives number of unique values in each column

###################################################################
###### #####       Making subsetted datasets     ###### ##### #####

#getting rid of some of these
exclude <- c(1:6,8,62,63,65,67,69:77)
bin_dat <- data[,-exclude] #saving to a new data frame of soon-to-be binary data
metadat <- data[,exclude] #saving other columns to a different data frame

#checking results
colnames(data) 
colnames(bin_dat)
colnames(metadat)

#######################################################################
###### ##### Changing bin_dat values to right format ###### ##### #####

#want to get all of the columns to 1s and 0s as well as numeric classes

str(bin_dat) #identifying which ones I need to change

#changing NA and other empty values to 0
bin_dat[] [is.na(bin_dat[])] <- 0
bin_dat[] [bin_dat == ""] <- 0
str(bin_dat) #checking

#changing words to 0 or 1
bin_dat[] [bin_dat == "yes"] <- 1
bin_dat[] [bin_dat == "no"] <- 0

str(bin_dat)#checking
bin_dat %>% summarise_all(n_distinct) 
  #told us that exp_isSBA and value_isBEST columns are wrong
  #exp_num_var and value_best_num_var are supposed to have more than 2 unique vals

#checking wtf is wrong
unique(bin_dat$value_isBEST)
  #there is a value called "no " that slipped past
unique(bin_dat$exp_isSBA)
  #only one value: 0
  #checked the original data: forgot to put 1s in this column wherever 1s appeared
  #in either the exp_captured_SBA and/or exp_SBA_abundance columns

#fixing problem 1
bin_dat$value_isBEST[bin_dat$value_isBEST == "no "] <- 0
bin_dat$value_isBEST <- as.numeric(bin_dat$value_isBEST)
unique(bin_dat$value_isBEST) 
class(bin_dat$value_isBEST) #checking     #is guud

#fixing problem 2 (this is harder)
bin_dat$exp_isSBA #the problem child
bin_dat$exp_captured_SBA
bin_dat$exp_SBA_abundance


nrow(bin_dat) #getting number of rows to use for i
colnames(bin_dat) #getting col # IDs for the three columns  --> 16, 17, 18

#replaces the value in the column of interest with 1 if either of the conditions = 1
bin_dat$exp_isSBA[(bin_dat$exp_captured_SBA | bin_dat$exp_SBA_abundance) == 1] <- 1
unique(bin_dat$exp_isSBA) #check

#looking for other errors
str(bin_dat)

bin_dat$value_best_num_var <- as.numeric(bin_dat$value_best_num_var)
unique(bin_dat$value_best_num_var) 
class(bin_dat$value_best_num_var) #checking     #is guud

#looking for other errors
str(bin_dat)



#######################################################################
###### #####     Transposing shit                    ###### ##### #####

bin_dat.t <- t(bin_dat) #transposing shit

rownames(bin_dat.t) #checking

#making it a dataframe again cuz transpose made it was a matrix
bin_dat.t <- as.data.frame(bin_dat.t)
class(bin_dat.t) #checking
str(bin_dat.t) #checking
tail(bin_dat.t)


###############################################################
#################    rowsums               ####################
###############################################################

#making the total sums column
bin_dat.t$totals <- rowSums(bin_dat.t)


bin_dat.t$totals #checking



###### 
# column1 <- c("p1","p2","p3","p4","p5")
# column2 <- c("R","","R","R","")
# column3 <- c("","B","","","B")
# column4 <- c("D","","D","","")
# test <- data.frame(column1,column2,column3,column4)
# colnames(test)[c(1:4)] <- c("pol_nbr","r1","r2","r3")
# test
# 
# test$tot_cols <- rowSums(test[, -1] != "") 
# 
# 



###############################################################
#################    remaking a chunky dataset           ####################

#listing
str(metadat)
colnames(metadat)

#making columns to discard
#discarded spatial scale bc we neglected some of the scales bc they were hidden
#within the papers
metadat_discard <- c("confidence_interv", "notes", "ParCorrDep", "spatial_scale")

metadat <- metadat[,-metadat_discard] #saving to a new data frame 
#unary operator error - bc some have empty values


#changing NA and other empty values to NA
metadat[] [metadat == ""] <- "NA"
metadat[] [is.na(metadat[])] <- "NA"
str(metadat) #checking

#trying again
metadat <- metadat[,-metadat_discard] #saving to a new data frame 
#didn't work. doing it another way

metadat.smol = subset(metadat, select = 
                        -c("confidence_interv", "notes", "ParCorrDep", "spatial_scale"))
#same error. another way!

colnames(metadat)
metadat.smol <- metadat[1:562, c(1:16)]
str(metadat.smol)
#worked. yay


#reversing the transposing 
bin_dat.trev <- t(bin_dat.t)
bin_dat.trev <- as.data.frame(bin_dat.trev) #coverting to data frame again cuz transpose
class(bin_dat.trev)

#looking at the data to see what to do next
colnames(bin_dat.trev)
colnames(metadat.smol)
  #need to add a blank row to metadat.smol so the dimensions are the same as the other set

metadat.smol[563,] <- "NA"

#also need to make a common column to merge the two datasets by, i'll do ID
metadat.smol$ID <- c(1:563)
bin_dat.trev$ID <- c(1:563)

#merging the two dataframes
metameta <- merge(metadat.smol, bin_dat.trev, by="ID")


###############################################################
#################    subsetting part dos            ####################

str(bin_dat.t) #listing shit
rownames(bin_dat.t) #listing the rownames to select which ones to sacrifice
ncol(bin_dat.t) #figuring out the # of the totals column

#making a variable that has the columns we want, written out this way for reference
#even though it was a pain
exclude_more <- c("exp_num_var","value_best_num_var","signif","value_isBEST",
                  "exp_isNA","exp_isTIME","exp_isSCALE","exp_isFIELD",
                  "exp_isSBA","exp_isPRED", "exp_isPREDTYPE",
                  "exp_isBORDERTYPE",
                  "exp_isBORDERTYPE_grass","exp_isBORDERTYPE_crops","exp_isBORDERTYPE_forest",
                  "exp_border_buckthorn", "exp_bordertype_alfalfa","exp_bordertype_corn",
                  "exp_bordertype_canola","exp_bordertype_cereals","exp_bordertype_soybean",
                  "exp_bordertype_richness","exp_bordertype_generalizedcrops",
                  "exp_bordertype_grass","exp_bordertype_prairie",
                  "exp_SOYPRED","exp_FUNGUS","exp_isOTHER")

#subsetting just the totals from the above groups
tots <- bin_dat.t[exclude_more,563]

#making a new dataset that has the exclude_more rows and just column 563 (totals)
bin_dat.t.tots <- data.frame(exclude_more, tots)
head(bin_dat.t.tots) #checking
colnames(bin_dat.t.tots)
rownames(bin_dat.t.tots)

#colname is fucked up
colnames(bin_dat.t.tots) <- c("variable", "total")
colnames(bin_dat.t.tots)


##############################################################
#################    data grabbing / visualization          ####################

#dataframes
metameta
bin_dat.t
bin_dat.t.tots

getwd()
write.csv(metameta, "C:/Users/sarah/OneDrive/Documents/R/Fall2021/metameta.csv" )
write.csv(bin_dat.t, "C:/Users/sarah/OneDrive/Documents/R/Fall2021/exp_pres_abs_with_totals.csv" )
write.csv(bin_dat.t.tots, "C:/Users/sarah/OneDrive/Documents/R/Fall2021/exp_totals.csv" )




