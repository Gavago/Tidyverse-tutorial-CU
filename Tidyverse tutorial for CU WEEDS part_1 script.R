# MY overall goal : -----
# to see how socio-ecology and energetics relate to immune activation
# need 2 data frames for subj-month and subj-2.month observations, that include data on
# rainfall*
# food availability
# life history variables
# biomarkers
# social behavior
# *part 1 focus on rainfall


# Simple example of readr, tidyr, dplyr: Rainfall -----


# See what original rainfall data look like 
# not tidy: cols are not different variables, are rainfall values per month

# from personal repository, demonstrate read_csv
# add col_types arg to read_csv to prevent "col type guessing" message
raw_rain<-read_csv("/Users/nic/Documents/NYCEP/Juvenile data and field/Data/Rainfall/rainfall.csv", col_types = cols())
raw_rain

# csv from direct raw text in github
library(RCurl)
x<-read.csv(text=getURL("https://raw.githubusercontent.com/Gavago/Tidyverse-tutorial-CU/master/rainfall.csv"),stringsAsFactors = F, header=T)
x
raw_rain <- x %>% as_tibble()
raw_rain

# Note: in read_csv example, x is automatically a tibble data frame, shows dim and vector class, 
# no strings as factors (useful for joining/merging), tibble prints only to fill the screen

# what I want:
# final df with
# row = average mm rain per 1 and 2 month periods
# 2 cols: time period & rain
# simple. tidy.


# Before getting started
# want to write months in a special way (i.e. as factor, lower case, and abbreviated)
# so build month.abb.fun function before running pipe 
# input will be character name of original cols
# output is factor w ordered levels
month.abb.fun<- function (x) { 
  fac.mo <- match(x,month.name) %>% #convert month to month number
    month.abb[.] %>% #to abbrev
    tolower(.) %>% #make lowercase
    factor(., levels= unique(.)) #make factor
  return(fac.mo)
}

# reference df to add new "period" variable/labels
pn<-data.frame(per.num=1:4, period=c("p1","p2","p3","p4"))

rdf <- raw_rain %>%
  gather(key = month , value = rainfall, -day) %>% #makes wide data long
  filter(month %in% month.name[c(8:12,1:3)]) %>% #isolate rows, my study period only from August 2015 to Mar 2016
  mutate(month.name = month.abb.fun(month)) %>% #reformat months
  mutate(per.num = ceiling(as.numeric(month.name)/2)) %>% #create 2 month period numbers
  merge(.,pn, by="per.num", all.x=T) %>% #add period labels, merge() instead of join() is personal preference
  as_tibble() #merge breaks tibble format, must redo :-\


# finish rain DFs: create month and 2 month period dfs
RAIN_per <- rdf %>%
  group_by(period) %>%
  summarise(avg.rain= mean(rainfall,na.rm=T))

RAIN_mo <- rdf %>%
  group_by(month.name) %>%
  summarise(avg.rain= mean(rainfall,na.rm=T))

RAIN_per
RAIN_mo

# uglier(!) alternative to group_by() and summarise() in base
aggregate(rdf$rainfall, by=list(rdf$period), mean, na.rm=T)
aggregate(rdf$rainfall, by=list(rdf$month), mean, na.rm=T)


# Other tidy and dplyr examples on rainfall ----
# example tidyr::spread() - takes long data and makes it wide
rain_long <-raw_rain %>%
  gather(key = month , value = rainfall, -day)
rain_long
rain_long %>%
  spread(month,rainfall)
rain_long %>%
  spread(day,rainfall)


# examples dplyr::select(contains()) and pull()
rdf %>%
  select(contains("rain")) %>%
  names()

rdf %>%
  select(starts_with("rain")) %>%
  names()

rdf %>%
  select(ends_with("fall")) %>%
  names()

# base alternatives of select() with grepl() and regex
names(rdf)[grepl("rain",names(rdf))]
names(rdf)[grepl("^rain",names(rdf))]
names(rdf)[grepl("fall$",names(rdf))]


# pull() return the numeric vector, rather than the list/data.frame element
rdf %>%
  select(contains("rain")) %>%
  pull()

# example filter specific rows with filter(grepl())
rdf %>%
  filter(grepl("Aug",month))

rdf %>%
  filter(grepl("ember$",month))

rdf %>%
  filter(grepl("^A",month))

# example renaming columns w rename()
x <- rdf %>%
  rename(rain=rainfall)

rdf %>%
  rename(rain= select(.,contains("rain")))

# alternative to rename() in base
colnames(rdf)[grepl("rain",colnames(rdf))] <- "rain"

# example arrange()
rdf %>%
  arrange(month) #alphabeteical, not chrono

mo.levels<-month.name[c(1:3,8:12)] #re-order levels, now arranges chronologically
rdf %>%
  mutate(month = factor(month, levels=mo.levels)) %>%
  as_tibble() %>%
  arrange(month) 

# alternative to arrange in base() == sort() and order()

# example distinct()
rdf %>%
  distinct(month) %>%
  pull()

# alternative to distinct in base
unique(rdf$month)

# example unite() & separate()
rdf %>%
  unite(month_day, month, day)

rdf %>%
  unite(month_day, month, day) %>%
  separate(month_day, c("month", "day"))
