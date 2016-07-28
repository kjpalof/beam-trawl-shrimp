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
ggplot(beam108season, aes(SEASON, POUNDS)) +geom_point() +geom_smooth()

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

beam106  %>%
  group_by(SEASON) %>%  # view both shrimp species together
  summarise(POUNDS =sum(POUNDS)) ->beam106season

beam106  %>%
  group_by(YEAR) %>%  # view both shrimp species together
  summarise(POUNDS =sum(POUNDS)) ->beam106year

###################################################################################
################ calculate mean harvest over different baseline years ##########
#####################################################################################

# use beam108period and beam 106
glimpse(beam108period)
levels(beam108period$SEASON)
ggplot(beam108year, aes(YEAR, POUNDS)) +geom_point() + geom_smooth() +ggtitle("District 108 (lb) Harvest by year")# to get an idea of which years to use
ggplot(beam106year, aes(YEAR, POUNDS)) +geom_point() + geom_smooth() +ggtitle("District 106 (lb) Harvest by year")# to get an idea of which years to use

#encompasses 
years1 <- c("May1982 - Apr83", "May1983 - Apr84", "May1984 - Apr85","May1985 - Apr86", "May1986 - Apr87", 
            "May1987 - Apr88", "May1988 - Apr89", "May1989 - Apr90", "May1990 - Apr91", "May1991 - Apr92",
            "May1992 - Apr93", "May1993 - Apr94", "May1994 - Apr95", "May1995 - Apr96")
years2 <- c("May1986 - Apr87", "May1987 - Apr88", "May1988 - Apr89", "May1989 - Apr90", "May1990 - Apr91", 
            "May1991 - Apr92","May1992 - Apr93", "May1993 - Apr94", "May1994 - Apr95", "May1995 - Apr96", 
            "May1996 - Apr97", "May1997 - Apr98", "May1998 - Apr99", "May1999 - Apr00", "May2000 - Apr01",
            "May2001 - Apr02", "May2002 - Apr03","May2003 - Apr04","May2004 - Apr05")
years3 <- c("May1980 - Apr81", "May1981 - Apr82", "May1982 - Apr83", "May1983 - Apr84", "May1984 - Apr85",
            "May1985 - Apr86", "May1986 - Apr87", "May1987 - Apr88", "May1988 - Apr89", "May1989 - Apr90",
            "May1990 - Apr91", "May1991 - Apr92", "May1992 - Apr93", "May1993 - Apr94", "May1994 - Apr95",
            "May1995 - Apr96", "May1996 - Apr97", "May1997 - Apr98", "May1998 - Apr99", "May1999 - Apr00",
            "May2000 - Apr01")
years4 <- c("May1973 - Apr74", "May1974 - Apr75","May1975 - Apr76", "May1976 - Apr77", "May1977 - Apr78", "May1978 - Apr79",
           "May1979 - Apr80", "May1980 - Apr81", "May1981 - Apr82", "May1982 - Apr83", "May1983 - Apr84", "May1984 - Apr85",
           "May1985 - Apr86", "May1986 - Apr87", "May1987 - Apr88", "May1988 - Apr89", "May1989 - Apr90", "May1990 - Apr91",
           "May1991 - Apr92")
#Years 1
##### 
#108 district 
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

#median instead of mean
beam8year1 %>%
  group_by(period108) %>%
  summarize (year1_lbs = median(POUNDS))->year1median
###############################
#106 district 
beam106 %>%
  filter(SEASON %in% years1) %>%
  group_by(SEASON, period106) %>%
  summarise(POUNDS = sum(POUNDS))  -> beam6year1 # pounds by season
beam6year1 %>%
  group_by(period106) %>%
  summarize (year1_lbs = mean(POUNDS))->year1mean6

beam6year1 %>%
  group_by(SEASON) %>%
  summarise(POUNDS = sum(POUNDS)) -> beam6year1_graph

ggplot(beam6year1_graph, aes(SEASON, POUNDS )) +geom_point() 

#median instead of mean
beam6year1 %>%
  group_by(period106) %>%
  summarize (year1_lbs = median(POUNDS))->year1median6

#####

#Year 2
#####
#district 108
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

#median instead of mean
beam8year2 %>%
  group_by(period108) %>%
  summarize (year2_lbs = median(POUNDS))->year2median
####
#district 106
beam106 %>%
  filter(SEASON %in% years2) %>%
  group_by(SEASON, period106) %>%
  summarise(POUNDS = sum(POUNDS))  -> beam6year2 # pounds by season
beam6year2 %>%
  group_by(period106) %>%
  summarize (year2_lbs = mean(POUNDS))->year2mean6

beam6year2 %>%
  group_by(SEASON) %>%
  summarise(POUNDS = sum(POUNDS)) -> beam6year2_graph

ggplot(beam6year2_graph, aes(SEASON, POUNDS )) +geom_point() 

#median instead of mean
beam6year2 %>%
  group_by(period106) %>%
  summarize (year2_lbs = median(POUNDS))->year2median6
#####
#Years 3
#####
#108 district 
beam108 %>%
  filter(SEASON %in% years3) %>%
  group_by(SEASON, period108) %>%
  summarise(POUNDS = sum(POUNDS))  -> beam8year3 # pounds by season
beam8year3 %>%
  group_by(period108) %>%
  summarize (year1_lbs = mean(POUNDS))->year3mean

beam8year3 %>%
  group_by(SEASON) %>%
  summarise(POUNDS = sum(POUNDS)) -> beam8year3_graph

ggplot(beam8year3_graph, aes(SEASON, POUNDS )) +geom_point() 

#median instead of mean
beam8year3 %>%
  group_by(period108) %>%
  summarize (year3_lbs = median(POUNDS))->year3median
### ###
#106 district 
beam106 %>%
  filter(SEASON %in% years3) %>%
  group_by(SEASON, period106) %>%
  summarise(POUNDS = sum(POUNDS))  -> beam6year3 # pounds by season
beam6year3 %>%
  group_by(period106) %>%
  summarize (year3_lbs = mean(POUNDS))->year3mean6

beam6year3 %>%
  group_by(SEASON) %>%
  summarise(POUNDS = sum(POUNDS)) -> beam6year3_graph

ggplot(beam6year3_graph, aes(SEASON, POUNDS )) +geom_point() 

#median instead of mean
beam6year3 %>%
  group_by(period106) %>%
  summarize (year3_lbs = median(POUNDS))->year3median6

#106 district
#Years 4  increasing trend
beam106 %>%
  filter(SEASON %in% years4) %>%
  group_by(SEASON, period106) %>%
  summarise(POUNDS = sum(POUNDS))  -> beam6year4 # pounds by season
beam6year4 %>%
  group_by(period106) %>%
  summarize (year4_lbs = mean(POUNDS))->year4mean6

beam6year4 %>%
  group_by(SEASON) %>%
  summarise(POUNDS = sum(POUNDS)) -> beam6year4_graph

ggplot(beam6year4_graph, aes(SEASON, POUNDS )) +geom_point() 

#median instead of mean
beam6year4 %>%
  group_by(period106) %>%
  summarize (year4_lbs = median(POUNDS))->year4median6

#####
library(scales)
#graphing
#####
ggplot(beam108year, aes(YEAR, POUNDS)) +geom_point() + geom_smooth() +ggtitle("District 108 (lb) Harvest by year")+
  scale_y_continuous(labels = comma)      # to get an idea of which years to use
ggplot(beam106year, aes(YEAR, POUNDS)) +geom_point() + geom_smooth() +ggtitle("District 106 (lb) Harvest by year")# to get an idea of which years to use

ggplot(graph2_beam106, aes(YEAR, POUNDS)) +geom_point() +geom_line() +geom_smooth() +geom_point(aes(YEAR,YR4_POUNDS, color=YEAR4, size=4)) +ggtitle("District 106 (lb) Harvest by year")


# 106
years4_by_year <- c(1973:1992)
beam106year %>%
  filter(YEAR %in% years4_by_year) %>%
  group_by(YEAR) %>%
  summarise(YR4_POUNDS = sum(POUNDS))  -> beam6years4_graph2 # pounds by season

# neeed to combine beam106year and beam6years4_graph2
#both have same YEAR - use this to merge.
beam6years4_graph2 %>%
  right_join(beam106year, by ="YEAR") %>%
  mutate(YEAR4 = ifelse(YR4_POUNDS >= 1, "BASE",0)) -> graph2_beam106

  #%>% mutate(YEAR4 = as.factor(YEAR4))-> graph2_beam106

#####
### ### ### ### ### 
#M1 - median catch over the most recent 3 yrs.
#####
glimpse(beam108)
M1 <- c("May2013 - Apr14", "May2014 - Apr15","May2015 - Apr16")

beam108 %>%
  filter(SEASON %in% M1) %>%
  group_by(SEASON, period108) %>%
  summarise(POUNDS = sum(POUNDS)) %>%
  group_by(period108) %>%
  summarize(M1_108_lbs = median(POUNDS))-> M1_108median_lbs
beam106 %>%
  filter(SEASON %in% M1) %>%
  group_by(SEASON, period106) %>%
  summarise(POUNDS = sum(POUNDS)) %>%
  group_by(period106) %>%
  summarize(M1_106_lbs = median(POUNDS))-> M1_106median_lbs
#####

#M2 - median catch over the most recent 10 yrs.
#####
M2 <- c("May2006 - Apr07", "May2007 - Apr08", "May2008 - Apr09","May2009 - Apr10","May2010 - Apr11", 
        "May2011 - Apr12", "May2012 - Apr13","May2013 - Apr14", "May2014 - Apr15","May2015 - Apr16")

beam108 %>%
  filter(SEASON %in% M2) %>%
  group_by(SEASON, period108) %>%
  summarise(POUNDS = sum(POUNDS)) %>%
  group_by(period108) %>%
  summarize(M2_108_lbs = median(POUNDS))-> M2_108median_lbs
M2_108median_lbs

beam106 %>%
  filter(SEASON %in% M2) %>%
  group_by(SEASON, period106) %>%
  summarise(POUNDS = sum(POUNDS)) %>%
  group_by(period106) %>%
  summarize(M2_106_lbs = median(POUNDS))-> M2_106median_lbs
M2_106median_lbs
#####

#M3 - 3rd highest catch of the most recent 10 yrs.
#####
# for 108 looks like this is May2014-Apr15
beam108 %>%
  filter(SEASON == "May2014 - Apr15") %>%
  group_by(period108) %>%
  summarise(POUNDS=sum(POUNDS))

# for 106 looks like this is May2005-Apr06
beam106 %>%
  filter(SEASON == "May2005 - Apr06") %>%
  group_by(period106) %>%
  summarise(POUNDS=sum(POUNDS))
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