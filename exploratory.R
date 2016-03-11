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
# summarizes pounds caught by year by district, also includes the number of unique ADFG numbers that fished
# also includes the sell_month so that the harvest period or session can be calculated
# only includes pink and sidestrip via the "condition" vector used (see above)
beam %>%
  mutate(DISTRICT =factor(DISTRICT), SPECIES_CODE = factor(SPECIES_CODE), 
         sell_date = as.Date(SELL_DATE, format = "%m/%d/%Y")) %>%
  mutate(sell_month = as.numeric(format(sell_date, "%m")))  %>%
  filter(SPECIES_CODE %in% condition) %>% #need tp pull out only pink and sidestrip
  group_by(YEAR, SEASON, DISTRICT, SPECIES_CODE, sell_month) %>%  # group by District - 106 and 108 are areas of concern
  summarise(no_adfgno = length(unique(ADFG_NO)), POUNDS = sum(POUNDS))   -> beam2

# need to summarize things by session - or season - make sure I group by these also!!!!!!!!
ggplot(beam2, aes(YEAR, POUNDS, color = DISTRICT)) +geom_point() + geom_smooth()

### just district 108 ##################
#########################################
# add in harvest periods see the periods vectors above
beam2 %>%
  filter(DISTRICT == 108) %>%
  mutate(period108 = ifelse(sell_month %in% period1, 1, ifelse(sell_month %in% period2, 2, 
                                                               ifelse(sell_month %in% period3, 3, 
                                                                      ifelse(sell_month %in% period4, 4,0))))) -> beam108
glimpse(beam108)
ggplot(beam108, aes(SEASON, POUNDS, color = SPECIES_CODE)) +geom_point() + geom_smooth()
ggplot(beam108, aes(YEAR, POUNDS, color = SPECIES_CODE)) +geom_point() + geom_smooth()

# just to graph all shrimp together as one point per period
beam108 %>%
  group_by(SEASON, period108) %>%  # view both shrimp species together
  summarise(POUNDS =sum(POUNDS)) ->beam108period

beam108 %>%
  group_by(YEAR) %>%  # view both shrimp species together
  summarise(POUNDS =sum(POUNDS)) ->beam108year

beam108 %>%
  group_by(SEASON) %>%  # view both shrimp species together
  summarise(POUNDS =sum(POUNDS)) ->beam108season

#ggplot(beam108all, aes(YEAR, POUNDS)) +geom_point() + geom_smooth()
ggplot(beam108season, aes(SEASON, POUNDS)) +geom_point() +geom_line()

### just district 106
beam2 %>%
  filter(DISTRICT == 106) %>%
  mutate(period106 = ifelse(sell_month %in% period1, 1, ifelse(sell_month %in% period2, 2, 
                                                               ifelse(sell_month %in% period3, 3, 
                                                                      ifelse(sell_month %in% period4, 3,0)))))-> beam106
ggplot(beam106, aes(YEAR, POUNDS, color = SPECIES_CODE)) +geom_point() + geom_smooth()
beam106 %>%
  group_by(SEASON, period106) %>%
  summarise(POUNDS =sum(POUNDS)) ->beam106all
ggplot(beam106all, aes(SEASON, POUNDS, color = period106)) +geom_point() 
###################################################################################
################ calculate mean harvest over different baseline years ##########
#####################################################################################

# use beam108period and beam 106
glimpse(beam108period)
levels(beam108period$SEASON)
ggplot(beam108year, aes(YEAR, POUNDS)) +geom_point() + geom_smooth() # to get an idea of which years to use

#encompasses 
years1 <- c("May1982 - Apr83", "May1983 - Apr84", "May1984 - Apr85","May1985 - Apr86", "May1986 - Apr87", 
            "May1987 - Apr88", "May1988 - Apr89", "May1989 - Apr90", "May1990 - Apr91", "May1991 - Apr92",
            "May1992 - Apr93", "May1993 - Apr94", "May1994 - Apr95", "May1995 - Apr96")
years2 <- c("May1986 - Apr87", "May1987 - Apr88", "May1988 - Apr89", "May1989 - Apr90", "May1990 - Apr91", 
            "May1991 - Apr92","May1992 - Apr93", "May1993 - Apr94", "May1994 - Apr95", "May1995 - Apr96", 
            "May1996 - Apr97", "May1997 - Apr98", "May1998 - Apr99", "May1999 - Apr00", "May2000 - Apr01",
            "May2001 - Apr02", "May2002 - Apr03","May2003 - Apr04",)
years3 <- 
#Years 1
##### 
beam108 %>%
  filter(SEASON %in% years1) %>%
  group_by(SEASON, period108) %>%
  summarise(POUNDS = sum(POUNDS))  -> beam8year1 # pounds by season
beam8year1 %>%
  group_by(period108) %>%
  summarize (year1_lbs = mean(POUNDS))->year1mean

beam8year1 %>%
  group_by(SEASON) %>%
  summarise(POUNDS = sum(POUNDS)) -> beam8year1_graph

ggplot(beam8year1_graph, aes(SEASON, POUNDS )) +geom_point() 
#####

#Year 2
#####
beam108 %>%
  filter(SEASON %in% years2) %>%
  group_by(SEASON, period108) %>%
  summarise(POUNDS = sum(POUNDS))  -> beam8year2 # pounds by season
beam8year2 %>%
  group_by(period108) %>%
  summarize (year2_lbs = mean(POUNDS))->year2mean

beam8year2 %>%
  group_by(SEASON) %>%
  summarise(POUNDS = sum(POUNDS)) -> beam8year2_graph

ggplot(beam8year2_graph, aes(SEASON, POUNDS )) +geom_point() 
#Years 3
#####


#####


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

#beam %>%
#  mutate(DISTRICT =factor(DISTRICT), SPECIES_CODE = factor(SPECIES_CODE), 
#         sell_date = as.Date(SELL_DATE, format = "%m/%d/%Y")) %>%
#  mutate(sell_month = as.numeric(format(sell_date, "%m")))  -> beam
#mutate(period108 = ifelse(sell_month %in% period1, 1, ifelse(sell_month %in% period2, 2, 
#                                                             ifelse(sell_month %in% period3, 3, 
#                                                                    ifelse(sell_month %in% period4, 4,0)))))