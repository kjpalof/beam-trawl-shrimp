#K.Palof ADF&G 
#3-7-16
#beam trawl exploration

############################################################################
#######################   data and libraries ################################
############################################################################
beam <-read.csv("Data Dump 1969-2015 trawl shrimp D6,7,8.csv")
beam

library(plyr)
library(dplyr)
library(ggplot2)

############################################################################
#######################   data subset and explore ################################
############################################################################

condition <- c(961, 962) # can change this to pull just the species I want as a subset

period1 <-c(5,6)
period2 <-c(7,8)
period3 <-c(9,10)
period4 <-c(11, 12, 1,2)

##
beam %>%
  mutate(DISTRICT =factor(DISTRICT), SPECIES_CODE = factor(SPECIES_CODE), 
         sell_date = as.Date(SELL_DATE, format = "%m/%d/%Y")) %>%
  mutate(sell_month = as.numeric(format(sell_date, "%m")))  %>%
  mutate(period108 = ifelse(sell_month %in% period1, 1, ifelse(sell_month %in% period2, 2, 
                                                               ifelse(sell_month %in% period3, 3, 
                                                                      ifelse(sell_month %in% period4, 4,0)))))-> beam

# summarizes pounds caught by year by district, also includes the number of unique ADFG numbers that fished
beam %>%
  mutate(DISTRICT =factor(DISTRICT), SPECIES_CODE = factor(SPECIES_CODE), 
             +          sell_date = as.Date(SELL_DATE, format = "%m/%d/%Y")) %>%
  mutate(sell_month = as.numeric(format(sell_date, "%m")))  %>%
  filter(SPECIES_CODE %in% condition) %>% #need tp pull out only pink and sidestrip
  group_by(YEAR, SEASON_REF, DISTRICT, SPECIES_CODE, sell_month) %>%  # group by District - 106 and 108 are areas of concern
  summarise(no_adfgno = length(unique(ADFG_NO)), POUNDS = sum(POUNDS))   -> beam2

# need to summarize things by session - or season - make sure I group by these also!!!!!!!!
ggplot(beam2, aes(YEAR, POUNDS, color = DISTRICT)) +geom_point() + geom_smooth()

### just district 108
beam2 %>%
  filter(DISTRICT == 108) %>%
  -> beam108
ggplot(beam108, aes(YEAR, POUNDS, color = SPECIES_CODE)) +geom_point() + geom_smooth()

beam108 %>%
  group_by(YEAR) %>%  # view both shrimp species together
  summarise(POUNDS =sum(POUNDS)) ->beam108all
#ggplot(beam108all, aes(YEAR, POUNDS)) +geom_point() + geom_smooth()
ggplot(beam108all, aes(YEAR, POUNDS)) +geom_point() + geom_line()+geom_smooth()

### just district 106
beam2 %>%
  filter(DISTRICT == 106) -> beam106
ggplot(beam106, aes(YEAR, POUNDS, color = SPECIES_CODE)) +geom_point() + geom_smooth()
beam106 %>%
  group_by(YEAR) %>%
  summarise(POUNDS =sum(POUNDS)) ->beam106all
ggplot(beam106all, aes(YEAR, POUNDS)) +geom_point() + geom_line()+geom_smooth()

################ calculate mean harvest over different baseline years ##########




####################

plot(beam2)
plot(beam2$no_adfgno, beam2$POUNDS)
plot(beam2$YEAR, beam2$no_adfgno)
plot(beambyyear$YEAR, beambyyear$POUNDS)

plot(beam3$YEAR, beam3$POUNDS, by =beam3$DISTRICT)

beam %>%
  group_by(YEAR) %>%
  summarise(no_adfgno = length(unique(ADFG_NO)), POUNDS = sum(POUNDS))   -> beambyyear

#beam2 %>%
#  group_by(YEAR) %>%
#  summarise(POUNDS = sum(POUNDS)) ->beambyyear 

plot(beambyyear)



############################################################################
#######################   random extra code ################################
############################################################################
beam %>%
  filter(SPECIES_CODE == 962) -> test2

beam$ADFG_NO
unique(beam$ADFG_NO)
length(unique(beam$ADFG_NO))


#changes input to date and extracts the month
test <- as.Date(beam$SELL_DATE, format = "%m/%d/%Y")
format(test, "%m") # extracts the month from the above vector of dates