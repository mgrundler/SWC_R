# What: Software Carpentry Workshop
# When: October 18th, 2016
# Who: Maggie Grundler
# Where: Hatcher Library, UM
# Why: To improve computer programming skills

######################################

# Packages necessary for this analysis:

library(RSQLite)

conn <- dbConnect(SQLite(), dbname='~/Desktop/survey.sqlite')

tables <- dbListTables(conn)

class(tables)

surveys <- dbGetQuery(conn, 'SELECT * FROM surveys')
head(surveys)
summary(surveys)

# Joining tables
surveys <- dbGetQuery(conn, 'SELECT * FROM surveys 
                      JOIN species ON surveys.species_id = species.species_id
                      JOIN plots ON surveys.plot_id = plots.plot_id;')
names(surveys)

surveys <- read.csv('~/Desktop/ecology.csv')
class(surveys)
sapply(surveys, class) # Call class for every column in surveys

# Always disconnect as soon as possible to protect hard drive

dbDisconnect(conn)
rm(conn) # Help clean up workspace by removing variables

##################
# Practice with dataframes and data types; dataframe is a collection of vectors (list); vectors can only hold one type of data
df <- data.frame(
  x1 = c(TRUE, FALSE, TRUE),
  x2 = c(1, 'red', 2))

class(df$x1)
typeof(df$x1)

x3 <- c(1, 'b', 2) # will convert everything to a string

# Use lists to store different data types together 

list(1:10, c(TRUE, FALSE))

str(surveys) # show structure of data
# Different ways to access data
class(surveys$year) # vector of integers
head(surveys$year)
class(surveys['year']) # single column dataframe
head(surveys['year'])
class(surveys[,'year']) # vector of integers
class(surveys[,4]) # vector of integers
class(surveys[['year']]) # vector of integers; list is a collection of vectors

##############

# Factors; human readable but stored as integers
surveys$sex
levels(surveys$sex)
nlevels(surveys$sex)

spice <- factor(c('low', 'medium', 'low', 'high'))
levels(spice)
max(spice) # Not meaningful

spice <- factor(c('low', 'medium', 'low', 'high'), levels=c('low', 'medium', 'high'))
levels(spice)
max(spice) # Not meaningful

spice <- factor(c('low', 'medium', 'low', 'high'), levels=c('low', 'medium', 'high'), ordered=TRUE)
levels(spice)
max(spice) # Meaningful!

spice <- ordered(spice, levels=c('high', 'medium', 'low'))
max(spice) # Reorder factor levels

###############
# Analysis tools

# Tabulation

tabulation <- table(surveys$taxa)
barplot(tabulation)

# Reorder by frequency of taxa in dataset

taxa <- ordered(surveys$taxa, levels=c('Rodent', 'Bird', 'Rabbit', 'Reptile')) # manual ordering
barplot(table(taxa))

surveys[order(surveys$weight),] # sort entire dataframe by ordered weight
order(surveys$weight) # order single column producing indices
sort(surveys$weight) # another way to order single column producing values

# With ordered factors, a baseline will be the first level; 
# exercise caution when assigning levels of factors, to avoid mistakes in downstream analysis 
# (i.e. baselines in linear regressions)

# Cross-tabulation

# How many records exist for each taxa in each year?

table(surveys$year, surveys$taxa) # Good method for quality control; check that data makes sense across columns
with(surveys, table(year, taxa)) # Same result, more readable; avoid repetitive typing

#############
# Questions

# Median weight of each rodent species between 1980 and 1990?

rodents <- surveys[taxa=='Rodent',]
rodents_8090 <- rodents[rodents$year %in% c(1980:1990),]
rodents_8090 <- rodents_8090[order(rodents_8090$year),]



library(dplyr) # data manipulation tasks with dataframes; can connect to databases

output <- select(rodents_8090, year, taxa, weight)
filter(surveys, taxa == 'Rodent') # another way to subset data; can specify which package - dplyr::filter()

filter(select(surveys, year, taxa, weight), taxa == 'Rodent') # nest functions for brevity

# Pipes

filtered <- surveys %>% 
  filter(taxa == 'Rodent') %>%
  select(year, taxa, weight)

# Subset to years 1980:1990

filtered <- surveys %>% 
  filter(taxa == 'Rodent', year %in% c(1980:1990)) %>% # can use & instead of , for same function, check equal operation using all.equal() function
  select(year, taxa, weight)

##### 
# Calculate medians

weights <- filter(rodents_8090, weight != 'NA') # eliminate NA weights

usp <- unique(weights$species)
medians <- matrix(NA, nrow=length(usp), ncol=1)
for (i in 1:length(usp)){
  x <- weights[weights$species == usp[i],]
  medians[i,1] <- median(x$weight)
}
rownames(medians) <- usp

# NO WEIGHT RECORDED FOR HARRISI, SPECIES DROPPED FOR MEDIANS MATRIX

#####

# Create new columns based on values in existing columns
 surveys %>%
  mutate(weight_kg = weight / 1000) %>%
  head() # pipe output into head function; or tail

# Split, Apply, Combine workflow
# Answer median question using plyr instead of base R!
  surveys %>%
    filter(!is.na(weight), taxa=='Rodent', year %in% 1980:1990) %>% # invert logical vector for correct result
    group_by(species_id) %>%
    summarize(med_weight = median(weight)) %>%
    print(n=18)
    
# Alternatively

surveys_complete <- surveys %>%
  filter(!is.na(weight),
         species_id != '',
         !is.na(hindfoot_length),
         sex != '',
         taxa == 'Rodent') 
  
common_species <- surveys_complete %>%
  group_by(species_id) %>%
  tally() %>% # count how many times each species appears
  filter(n >= 50) %>%
  select(species_id)

common_surveys <- surveys_complete %>%
  filter(species_id %in% common_species$species_id)

write.csv(common_surveys, file = '~/Desktop/surveys_complete.csv', row.names=FALSE)

#############
## ggplot2 ##
#############

library(ggplot2)

ggplot(data = common_surveys, aes(x=weight, y=hindfoot_length, color=species_id)) + geom_point()
