##########################
### ZOO800: HOMEWORK 3 ###
##########################

# Author: Rebekkah L. LaBlue (w/ support from Aubrey Wendorff)
# Project: Intro coding in R: syntax, variables, data stractures, loops
# Due: September 22, 2025

################
### PACKAGES ###
################

library(dplyr)
library(tidyverse)
library(magrittr)
library(LakeMetabolizer)

######################
### PART 1: BASICS ###
######################

### --- TEMPERATURE --- ###

temp_C <- 18.5 # assign value
temp_F <- temp_C * 9/5 + 32 # create new value mathematically from base above

Temp_result <- paste("The water temperature is", temp_C, "°C and", temp_F, "°F.") # embed numeric values in summary chr string
print(Temp_result)

###############################
### PART 2: VECTORS, ARRAYS ###
###############################

### --- FISH --- ###

species_counts <- c(Bluegill = 12, Bass = 7, Sunfish = 21, Carp = 3) # assign associated chr and num values to/within vector

sum(species_counts) # sum num values across vector
names(which.max(species_counts)) # find vector's chr index with largest num value (which.max) and return its name (names)

### --- CHLORYPHYLL --- ###

Chlorophyll_matrix <- matrix( 
  c(2,3,4,  17,12,9,  36,45,49), # values grouped by dimensions
  nrow = 3, ncol = 3, byrow = TRUE, # create matrix of set dimensions, fill across rows not columns
  dimnames = list(c("Surface", "Mid", "Bottom"), # name rows
                  c("Day 1", "Day 2", "Day 3"))) # name columns

rowMeans(Chlorophyll_matrix) # calculate num row means of matrix, which correspond to each depth across 3 sample days

###########################
### PART 3: DATA FRAMES ###
###########################

### --- LAKES --- ###

Lakes_df <- data.frame( # df, columns names = associated values
  lake = c("Mendota", "Wingra", "Monona", "Waubesa", "Kegonsa"),
  temp_C = c(22.4, 25.1, 23.7, 24.6, 26.0),
  DO_mgL = c(8.3, 6.7, 7.5, 7.9, 6.2)
)

colMeans(Lakes_df[, c(2,3)]) # use indexing to take means of only num columns 2,3 

### --- BONUS --- ###
equalize_water <- function(temp_C) { # define function to perform pckg LakeMetabolizer analysis
  o2.at.sat.base(temp_C) # .base takes temp as argument without needing time series data
}

Lakes_df <- Lakes_df %>% # update df with new columns
  mutate(
    eq_conc = equalize_water(temp_C), # new column for results of function
    DO_perc = (DO_mgL / eq_conc) * 100 # new column for mathmatical transformation of values
  ) %>% 
  arrange( # order appearance of lakes/rows in df from smallest (top) to largest DO_perc value
    DO_perc
  )

#########################
### PART 4: FOR LOOPS ###
#########################

### --- SQUARE ROOTS --- ###

numbers <- c(1:10) # numbers is a vector of values 1 through 10
for (number in numbers) { # for every individual index/element in 10-part vector numbers
  print(sqrt(number)) # take square root of each number, show results
}

### --- EXPONENTIAL GROWTH --- ###

N0 <- 10 # assign values
r <- 0.3
t <- (1:10)

pop <- vector("numeric", length = 10) # create vector pop, which will contain 10 num elements
for (timestep in t) { # for each index/element/timestep in 10-part vector t
  pop[timestep] <- (N0 * exp(r * timestep)) # pop filled by the value for each timestep (10 total) resulting from the exp growth equation
}

### --- PHOSPHORUS --- ###

phosphorus <- list( # create list, automate values fill (4 each) with random number generation runif and bounding (min, max)
  lake1 = c(runif(4, min = 5, max = 40)),
  lake2 = c(runif(4, min = 5, max = 40)),
  lake3 = c(runif(4, min = 5, max = 40)),
  lake4 = c(runif(4, min = 5, max = 40)),
  lake5 = c(runif(4, min = 5, max = 40))
)

lake_means <- vector("numeric", length = length(phosphorus)) # create vector lake_means, which will contain num elements for as many elements are in phosphorus (5)

for (lake in seq_along(phosphorus)) { # for each index/lake in phosphorus (seq_along NOT length to loop across each element in list)
  lake_means[lake] <- mean(phosphorus[[lake]]) # lake_means filled by values for each lake, which will be result of taking the mean for [[each num vector]] within the [list] 
  print(lake_means[lake]) # show means for each lake stored in lake_means
}

###############################
### PART 5: APPLY FUNCTIONS ###
###############################

### --- CHLORYPHYLL --- ###

apply(Chlorophyll_matrix, MARGIN = 1, FUN = "mean") # margin 1 = row, define function as mean
apply(Chlorophyll_matrix, MARGIN = 2, FUN = "mean") # margin 2 = column, define function as mean

### --- LAKES --- ####

apply(Lakes_df[, 2:5], MARGIN = 2, FUN = function(x) max(x) - min(x)) # create range function, apply across columns 2-5

### --- EXPONENTIAL GROWTH --- ###

pop_sapply <- sapply(t, function(timestep) N0 * exp(r * timestep)) # returns vector containing individual numeric values
pop_lapply <- lapply(t, function(timestep) N0 * exp(r * timestep)) # returns numbered list of vectors with associated numeric values
# ANSWER: sapply() is cleaner because the returned numeric vector can be passed into other functions, operations easily...
# ... lapply() would require first un-listing elements to use numeric values of vector only