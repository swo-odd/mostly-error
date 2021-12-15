rm(list=ls())
getwd()
setwd("C:/Users/sarah/OneDrive/Documents/R/Fall2021")
getwd()

library(dplyr)

data <- read.csv("EcolSoy_v6.csv", header = TRUE, as.is = TRUE)
  #na.strings = c('smth you want to be recog as NA', 'smth#2')   sets certain values to be read as NA

head(data) #checking

dat <- data #copy to fuck up

colnames(data) #looking at colnames to see what to get rid of 

#removing extraneous columns (for now) to make a smaller dataset
dat = subset(data, select = 
               -c(notes,ParCorrDep, 
                  delta_i, w_i, value_R2, value_R2_type, likelihood, 
                  confidence_interv, spatial_scale, 
                  exp_border, exp_isNA))

head(dat) #checking

###################################################
###### replacing values with other ones ##### #####

colnames(dat) #checking which columns to replace empty values with 0

#replacing empty values in columns 9-59, 62, and 66 with 0
dat[c(9:59, 62, 65, 66)][is.na(dat[c(9:59, 62,65, 66)])] <- 0

head(dat)  #check
unique(dat$value_isBEST)
unique(dat$p_val)
#didn't work on column 62 or 65

class(dat[,62]) #checkign if it's bc of different class type
class(dat[,65])
class(dat[,66]) #col 62 is character while 66 is numeric

#trying something else for those columns
dat$value_isBEST[dat$value_isBEST == ""] <- 0
dat$p_val[dat$p_val == ""] <- "NA"

unique(dat$value_isBEST) #checking
unique(dat$p_val)
unique(dat$signif)
head(dat) #checking

#replacing col 62's words with numbers
dat$value_isBEST[dat$value_isBEST == "yes"] <- 1
dat$value_isBEST[dat$value_isBEST == "no"] <- 0
dat$value_isBEST[dat$value_isBEST == "no "] <- 0

unique(dat$value_isBEST) #checking
head(dat) #checking


#trying to make all the binary columns into numerics

#making into a numeric test
as.numeric(dat$exp_num_var) #realized needed 0s instead of NA
dat$exp_num_var [is.na(dat$exp_num_var)] <- 0
class(dat$exp_num_var)
unique(dat$exp_num_var)

str(dat) #checking

#changing remaining ones I want numeric into numeric
dat$value_isBEST <- as.numeric(dat$value_isBEST)
dat$value_best_num_var <- as.numeric(dat$value_best_num_var)

str(dat) #checking

######################################################
###### making a transposed version for use ##### #####
dat.t <- t(dat)

#checking
head(dat)
head(dat.t)
rownames(dat)
rownames(dat.t)
colnames(dat)
colnames(dat.t)

###############################################################
#################    rowsums               ####################
###############################################################
#made a copy to fuck with
dat.t.trash <- dat.t
#showing the data
dim(dat.t.trash)
nrow(dat.t.trash) #rows are the categories(?)
ncol(dat.t.trash) #columns are the individual models
str(dat.t.trash)
rownames(dat.t.trash)

#exclude the non-numeric rows
exclude <- c(1:6,8,60, 61, 63,65)

dat.t.trash <- dat.t.trash[-exclude,]

nrow(dat.t.trash) #checking
rownames(dat.t.trash)

#making it a dataframe again cuz for some reason it was a matrix
dat.t.trash <- as.data.frame(dat.t.trash)
class(dat.t.trash) #checking

class(dat.t.trash[1,])











?as.data.frame

#making any remainin nonnumeric numeric
dat.t.trash[55,]<- as.numeric(dat.t.trash[55,])

#making the total sums column
dat.t.trash$total_present <- rowSums(dat.t.trash)



dat.t.trash <- as.data.frame(dat.t.trash)
class(dat.t.trash)







dat.t.trash[1,]<- as.numeric(dat.t.trash[1,])
dat.t.trash[1,]






rownames(dat.t.trash$exp_num_var)
dat.t.trash$


class(dat.t.trash$exp_num_var)

?data


?str


###example
column1 <- c("p1","p2","p3","p4","p5")
column2 <- c("R","","R","R","")
column3 <- c("","B","","","B")
column4 <- c("D","","D","","")
test <- data.frame(column1,column2,column3,column4)
colnames(test)[c(1:4)] <- c("pol_nbr","r1","r2","r3")
test

test$tot_cols <- rowSums(test[, -1] != "") 

test

#mine
dat.t.trash$total_present <- rowSums(dat.t.trash[-exclude ,])




?rowSums













ifelse(class(dat.t) == "numeric", rowSums(dat.t), NA)

i <- c(1:66)
for(i in dat.t[i , ]) { 
  if(class(dat.t[i, ]) = "numeric") {rowSums(dat.t[i,])} 
  else {NA}
}



dat.t$rowsums <- for(i in dat.t[i , ]) {ifelse(class(dat.t) == "numeric", rowSums(dat.t), NA)}

       
       
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
rowSums(x); colSums(x)
dimnames(x)[[1]] <- letters[1:8]
rowSums(x); colSums(x); rowMeans(x); colMeans(x)
x[] <- as.integer(x)
rowSums(x); colSums(x)
x[] <- x < 3
rowSums(x); colSums(x)
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
x[3, ] <- NA; x[4, 2] <- NA
rowSums(x); colSums(x); rowMeans(x); colMeans(x)
rowSums(x, na.rm = TRUE); colSums(x, na.rm = TRUE)
rowMeans(x, na.rm = TRUE); colMeans(x, na.rm = TRUE)


rowSums

count(dat.t[, class == "numeric" | length == 1 ])

?rowSums

i <- c(1:66)
for(i in dat.t[i , ]) { 
  if(class(dat.t[i, ]) = "numeric") {rowSums(dat.t[i,])} 
  else {NA}
  }
  
if(class(dat.t[i, ]) = "numeric") {rowSums(dat.t[i,])} else {NA}
  
  rowSums(class(dat.t) == "numeric" | dat.t > 0)
  
# Filter the OTU table to keep ONLY OTUs in at least 5% of samples 
# otu_filt3 > 0 sets the values greater than 0 to TRUE 


# rowSums(otu_filt3 > 0) sums the TRUEs in each row (number of samples OTU is in)


# ncol(otu_filt3) find the number of columns 
# (0.05*ncol(otu_filt3)) multiple that by 0.05 
# otu_filt3[rowSums(otu_filt3 > 0) > (0.05*ncol(otu_filt3)),] keep only 
# rows where occurance is greater than 5% of samples 
featuretable_filt <- featuretable_filt[rowSums(featuretable_filt > 0) > (0.05*ncol(featuretable_filt)),] 
head(featuretable_filt)    


}

rownames
dat.t$rowsums <- ifelse(class(dat.t) == "numeric", rowsums(dat.t), NA)





table(dat.t)
dat.t$rowsums <- if(dat.t
                    
                    
                    rowSums(dat.t)

dat.t$rowsums <- rowSums(dat.t if(class(dat.t[,i]) == "numeric")
dat.t$rowsums <- rowSums(dat.t)


###############################################################
#################    come back to this     ####################
###############################################################

#tried findin unique values for all columns, didnt work
unique(dat)
unique(dat[,1:66])

#tried this dplyr way
dat %>% summarise_all(n_distinct)

#all the yes or no columns that we want have 2 values like we want
#except exp_isSBA

unique(dat$exp_isSBA) #checking
# yeah I fucked up. forgot to make columns
# exp_captured_SBA and exp_SBA_abundance be reflected
# in exp_isSBA

#trying to fix that

######################################################################
############  data to tabulate  #############
######################################################################

colnames(dat)

unique(dat$paper_ID) #10 studies
dat %>% summarise_all(n_distinct) # getting data on how many types of models existed
unique(dat$model_type) #types of models here
unique(dat$value_type)
dat$value_type

list(dat$value_type == "NA") #need to thinkn aboout whether to keep the null versions or not

######################################################################
############  Subsetting for checking landscape effects  #############
######################################################################

dat_by_landscape_overall <- group_by(dat, exp_isBORDERTYPE)

head(dat_by_landscape_overall)
colnames(dat_by_landscape_overall)

ggplot(dat, x = "exp_isBORDERTYPE", y = )

data$new <- rowSums(data)

library(vegan)
dat.t <- t(dat)
head(dat)
head(dat.t)

rownames(dat)
rownames(dat.t)

colnames(dat)
colnames(dat.t)

barplot(dat$exp_num_var)

?barplot


keep_interest <- c("p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Prevotellaceae.g__Prevotella",
                   "p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae.g__Bacteroides",
                   "p__Firmicutes.c__Clostridia.o__Clostridiales.f__Clostridiaceae.g__Clostridium")

#Making subsetted otu_tables for each
otu_significant <- otu_table[otu_table$Taxa %in% keep_significant, ]
otu_significantsugar <- otu_table[otu_table$Taxa %in% keep_significantsugar, ]
otu_significantaspartame <- otu_table[otu_table$Taxa %in% keep_significantaspartame, ]
otu_interest <- otu_table[otu_table$Taxa %in% keep_interest, ]

colnames(otu_significant)
#Plotting these subsets
library(ggplot2)

#All significant OTUS
ggplot(otu_significant, aes(x=Aspartame_Diet, y=RelativeAbundance, fill=Taxa)) +  
  geom_bar(stat ="Identity") + facet_grid(.~Sugar_Diet) + 
  xlab("Aspartame Presence") + ylab("Relative Abundance") +
  scale_fill_discrete(name = "Bacterial Taxa")
#Significant sugar otus
ggplot(otu_significantsugar, aes(x=Sugar_Diet, y=RelativeAbundance, fill=Taxa)) +  
  geom_bar(stat ="Identity") + 
  xlab("Sugar Diet") + ylab("Relative Abundance") +
  scale_fill_discrete(name = "Bacterial Taxa")
#Significant aspartame otus
ggplot(otu_significantaspartame, aes(x=Aspartame_Diet, y=RelativeAbundance, fill=Taxa)) +  
  geom_bar(stat ="Identity") + 
  xlab("Aspartame Presence") + ylab("Relative Abundance") +
  scale_fill_discrete(name = "Bacterial Taxa")
#OTUs of interest
ggplot(otu_interest, aes(x=Aspartame_Diet, y=RelativeAbundance, fill=Taxa)) +  
  geom_bar(stat ="Identity", identity = "fill") + facet_grid(.~Sugar_Diet) + 
  xlab("Aspartame Presence") + ylab("Relative Abundance") +
  scale_fill_discrete(name = "Bacterial Taxa")

######################################################################
############  Subsetting by value type                   #############
###################################################################### 

library(dplyr)

dat_by_val <-mutate(dat,
                 value= case_when(
                   TOTAL_SUGARS_G_AVE >=77 ~"HIGH SUGAR",
                   TOTAL_SUGARS_G_AVE <77 ~"LOW SUGAR",
                   TRUE ~ NA_character_))

mapping <-mutate(mapping,
                 Aspartame_Diet= case_when(
                   ASPARTAME_MG_AVE >0 ~"YES",
                   ASPARTAME_MG_AVE <=0 ~"NO",
                   TRUE ~ NA_character_))

library(ggplot2)

ggplot(dat, aes(x=Aspartame_Diet, y=RelativeAbundance, fill=Taxa)) +  
  geom_bar(stat ="Identity", position="fill") + facet_grid(.~Sugar_Diet) + 
  xlab("Aspartame Presence") + ylab("Relative Abundance") +
  scale_fill_discrete(name = "Bacterial Taxa")


ggplot(dat, aes(x=Aspartame_Diet, y=RelativeAbundance, fill=Taxa)) +  
  geom_bar(stat ="Identity", position="fill") + facet_grid(.~Sugar_Diet) + 
  xlab("Aspartame Presence") + ylab("Relative Abundance") +
  scale_fill_discrete(name = "Bacterial Taxa")

plot_by_valuetype

hist(dat, exp_num_var)

class(dat$exp_num_var)

dat

dat$value_isBEST[dat$value_isBEST == "no "] <- 0
#plotting
xx.bs = ggplot() + 
  #NMDS grouping data by bray
  geom_point(data = data.scores, size = 4, 
             aes(x=NMDS1, y=NMDS2, shape = family, color = origin)) +
  #species points
  geom_point(data = species.scores, 
             aes(x=NMDS1, y=NMDS2), alpha=0.5) +
  #species points labels
  geom_text(data= species.scores, aes(x=NMDS1, y=NMDS2, label=species),
            alpha = 0.25, size = 2.5, nudge_y = 0.03, check_overlap = FALSE) +
  #hull groupings by family
  geom_polygon(data=data.scores, 
               aes(x=NMDS1, y=NMDS2, fill=origin, group = origin), 
               alpha = 0.15, show.legend = FALSE) +
  #labels
  labs(x = "NMDS1", colour = "Origin", y = "NMDS2", shape = "Family",
       title = "NMDS using Bray-Curtis Dissimilarity")



dat_landtype









