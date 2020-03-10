###################################
#########Seminar Code##############


#install libraries

install.packages('sf')
install.packages('dplyr')
install.packages('tidyr')
install.packages('ggplot2')
install.packages('data.table')
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)


#set work directory

setwd('C:\\Users\\einav\\Desktop\\Seminar_Einav_Grinberg\\FinalData')


#add violence data

violence_1 = read.csv('questionnaires\\violence_data.csv', stringsAsFactors = F)
violence_2 = read.csv('questionnaires\\violence_data2.csv', stringsAsFactors = F)

#organize violence columns

move.col <- function(df, move_this, next_to_this, before = FALSE) {
  if (before==FALSE)
    df[,c(match(setdiff(names(df)[1:which(names(df)==next_to_this)],move_this),names(df)),
          match(move_this,names(df)),
          match(setdiff(names(df)[which(names(df)==next_to_this):ncol(df)],c(next_to_this,move_this)),names(df)))]
  else
    df[,c(match(setdiff(names(df)[1:(which(names(df)==next_to_this))],c(next_to_this,move_this)),names(df)),
          match(move_this,names(df)),
          match(setdiff(names(df)[(which(names(df)==next_to_this)):ncol(df)],move_this),names(df)))]
}

violence_1 = move.col(violence_1, 'momage', 'Date.of.Interview')

a  = colnames(violence_2[,60:63])
b = colnames(violence_2[,155:156])
c = colnames(violence_2 [,77:80])

violence_2 = setcolorder(violence_2, c(a, b, c))

rm(a,b,c)

#check the NA's in every dataset

sum(is.na(violence_1))
sum(is.na(violence_2))

#remove coord NA's

sum(is.na(violence_1[,112:113]))
violence_1 = violence_1 %>%
  drop_na(Longitude, Latitude)
sum(is.na(violence_2[,157:158]))
violence_2 = violence_2 %>%
  drop_na(longitude, latitude)

#st geometry for violence data.frames

violence_1 = st_as_sf(violence_1, coords = c('Longitude','Latitude'), crs = 4326)
violence_2 = st_as_sf(violence_2, coords = c('longitude','latitude'), crs = 4326)

#save violence data to shapefiles and remove out of bounds points from the data in ArcMap

st_write(violence_1, 'shapefiles\\violence_1.shp', delete_dsn = TRUE)
st_write(violence_2, 'shapefiles\\violence_2.shp', delete_dsn = TRUE)

#read new violence shapefiles

violence_data1 = st_read('shapefiles/violence_data1.shp')
violence_data2 = st_read('shapefiles/violence_data2.shp')

#change the field names to the new shapfiles

names(violence_data1) = names(violence_1)
names(violence_data2) = names(violence_2)

#keep only relevant questions

v1 = violence_data1 %>%
  select(momage,
         seen.knife.ever, seen.shot.ever, ever.been.shoved, ever.been.attacked.knife, ever.been.shot.at,
         since.pushed, since.ever.kicked, since.ever.hit.hurt.body, since.ever.choked,
         since.ever.force.sex, since.ever.physical.attack, not.go.anywhere.alone,
         not.go.out.after.dark, keep.doors.locked, not.talk.to.strangers, know.neighbors)


v2 = violence_data2 %>%
  select(momage, heard.gun.ever, during.fear.force.sex, during.fear.physical.attack)


#save the chosen question layers for spatial join and select by location with the block data (data.shp) in ArcMap

st_write(v1, 'shapefiles\\v1.shp', delete_dsn = TRUE)
st_write(v2, 'shapefiles\\v2.shp', delete_dsn = TRUE)


########statistics##########

#read the joined layer 

dat_v = read_sf('shapefiles/data_vf.shp', stringsAsFactors = FALSE)

#change the question names

x = names(v2)
x = x[2:4]
names(dat_v)[134:136] = x
y = names(v1)
y = y[2:42]
names(dat_v)[139:154] = y

#check variables normal distribution with shapiro

shapiro.test(dat_v$Mean)
shapiro.test(dat_v$BLACK)
shapiro.test(dat_v$inc_blw_p)
shapiro.test(dat_v$WHITE)
shapiro.test(dat_v$HISPANI)
shapiro.test(dat_v$edu_ttl)
shapiro.test(dat_v$CRIME_T)
shapiro.test(dat_v$CRMCYAS)
shapiro.test(dat_v$CRMCYRA)
shapiro.test(dat_v$CRMCYMU)
shapiro.test(dat_v$POP_SQM)
shapiro.test(dat_v$ndvi)
shapiro.test(dat_v$LAN)
shapiro.test(dat_v$POLICE_)

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$BLACK)#mean - 284.3

#filter to 2 groups of high and low Black population by mean

high_b = dat_v %>%
  filter(BLACK >= 284.3)

low_b = dat_v %>%
  filter(BLACK < 284.3)

#t.test

black_res = t.test(high_b$Mean, low_b$Mean)
black_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$inc_blw_p)#mean - 57.92

#filter to 2 groups of high and low population below the poverty line by mean

high_incblwp = dat_v %>%
  filter(inc_blw_p >= 57.92)

low_incblwp = dat_v %>%
  filter(inc_blw_p < 57.92)

#t.test

incblwp_res = t.test(high_incblwp$Mean, low_incblwp$Mean)
incblwp_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$WHITE)#mean - 820.5

#filter to 2 groups of high and low White population by mean

high_w = dat_v %>%
  filter(WHITE >= 820.5)

low_w = dat_v %>%
  filter(WHITE < 820.5)

#t.test

w_res = t.test(high_w$Mean, low_w$Mean)
w_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$HISPANI)#mean - 188.3

#filter to 2 groups of high and low Hispanic population by mean

high_his = dat_v %>%
  filter(HISPANI >= 188.3)

low_his = dat_v %>%
  filter(HISPANI < 188.3)

#t.test

his_res = t.test(high_his$Mean, low_his$Mean)
his_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$edu_ttl)#mean - 984.2

#filter to 2 groups of high and low total education by mean

high_edu = dat_v %>%
  filter((edu_ttl >= 984.2))

low_edu = dat_v %>%
  filter((edu_ttl < 984.2))

#t.test

edu_res = t.test(high_edu$Mean, low_edu$Mean)
edu_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$CRIME_T)#mean - 1225

#filter to 2 groups of high and low total crimes by mean

high_crimettl = dat_v %>%
  filter((CRIME_T >= 1225))

low_crimettl = dat_v %>%
  filter((CRIME_T < 1225))

#t.test

crimettl_res = t.test(high_crimettl$Mean, low_crimettl$Mean)
crimettl_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$CRMCYAS)#mean - 205.3

#filter to 2 groups of high and low assault crimes by mean

high_crimeas = dat_v %>%
  filter((CRMCYAS >= 205.3))

low_crimeas = dat_v %>%
  filter((CRMCYAS < 205.3))

#t.test

crimeas_res = t.test(high_crimeas$Mean, low_crimeas$Mean)
crimeas_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$CRMCYRA)#mean - 135.4

#filter to 2 groups of high and low rape crimes by mean

high_crimera = dat_v %>%
  filter((CRMCYRA >= 135.4))

low_crimera = dat_v %>%
  filter((CRMCYRA < 135.4))

#t.test

crimera_res = t.test(high_crimera$Mean, low_crimera$Mean)
crimera_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$CRMCYMU)#mean - 145.7

#filter to 2 groups of high and low murder crimes by mean

high_crimemu = dat_v %>%
  filter((CRMCYMU >= 145.7))

low_crimemu = dat_v %>%
  filter((CRMCYMU < 145.7))

#t.test

crimemu_res = t.test(high_crimemu$Mean, low_crimemu$Mean)
crimemu_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$POP_SQM)#mean - 15448
 
#filter to 2 groups of high and low population density by mean

high_dense = dat_v %>%
  filter((POP_SQM >= 15448))

low_dense = dat_v %>%
  filter((POP_SQM < 15448))

#t.test

desne_res = t.test(high_dense$Mean, low_dense$Mean)
desne_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$ndvi)#mean - 0.58550

#filter to 2 groups of high and low ndvi by mean

high_ndvi = dat_v %>%
  filter((ndvi >= 0.58550))

low_ndvi = dat_v %>%
  filter((ndvi < 0.58550))

#t.test

ndvi_res = t.test(high_ndvi$Mean, low_ndvi$Mean)
ndvi_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$LAN[dat_v$LAN > 0])#mean - 38.4070

#filter to 2 groups of high and low areas with light at night by mean

high_lan = dat_v %>%
  filter((LAN >= 38.4070), (LAN !=0))

low_lan = dat_v %>%
  filter((LAN < 38.4070), (LAN !=0))

#t.test

lan_res = t.test(high_lan$Mean, low_lan$Mean)
lan_res

#calculate summary for socio-demogrpahic and crime data

summary(dat_v$POLICE_) #mean - 0.9197

#filter to 2 groups of high and low amount of police departments by mean

high_police = dat_v %>%
  filter((POLICE_ >= 0.9197))

low_police = dat_v %>%
  filter((POLICE_ < 0.9197))

#t.test

police_res = t.test(high_police$Mean, low_police$Mean)
police_res

#create a dat_v point shapefile for presenting safety index in Arcmap

point_v = st_centroid(dat_v)
st_write(point_v,"shapefiles\\point_vf.shp", delete_dsn = T)

#save dat_v for creating maps with Arcmap

st_write(dat_v, "shapefiles\\data_vf.shp", delete_dsn = T)

#plot the murder crimes and safety index 

ggplot(data = dat_v) +
  geom_smooth(mapping = aes(x = Mean, y = CRMCYMU), method = 'loess')+
  labs(x = 'Safety Index', y ='Murder Crimes', title = "Murder Crimes vs Safety Index")


#add new column categorizing the sense of security to 2 groups by the security index divided by 2

max(dat_v$Mean) / 2
dat_v$category <- ifelse(dat_v$Mean >= 0.2894737  , 
                               "Low Sense of Security", "High Sense of Security")

#plot of the categorized safety index distribution in each county

ggplot(dat_v, aes(x = C_Name, y = Mean, fill = category))+
  geom_bar(stat = 'identity', width = 0.5)+
  labs(title = 'High and Low Sense of Security by County',
       x = 'County Name', y = 'Categorized Safety Index Distribution')+
  theme(axis.text.x = element_text(angle=65, vjust=0.6, size = 10),plot.title = element_text(hjust = 0.5))

#install cartography and zoom packages for question maps

install.packages('cartography')
library(cartography)
install.packages('zoom')
library(zoom)

#read all the counties data for background

counties = read_sf('shapefiles/counties_v.shp')


#plot the question answers

par(bg = '#d1e5f0')
plot(st_geometry(counties), col = '#d9d9d9')
zoomplot.zoom(fact = 3)
choroLayer(dat_v, var = "heard.gun.ever", 
           breaks = c(0, 0.5, 1),  
            lwd = 0.1, legend.pos = "topright", col = c('#2166ac','#b2182b'),
           legend.title.txt = "0 - No, 1 - Yes", 
           legend.values.rnd = 2, add = T)
layoutLayer(title = "Have you ever heard a gun shot?", postitle = 'center',
            frame = FALSE, col = NA, scale = 'auto', coltitle = "black", 
            north = FALSE, south = FALSE )


plot(st_geometry(counties), col = '#d9d9d9')
zoomplot.zoom(fact = 3)
choroLayer(dat_v, var = "ever.been.shoved", 
           breaks = c(0, 0.5, 1),  
            lwd = 0.1, legend.pos = "topright", col = c('#2166ac','#b2182b'),
           legend.title.txt = "0 - No, 1 - Yes", 
           legend.values.rnd = 2, add = T)
layoutLayer(title = "Have you ever been shoved, kicked, or punched in an argument?", postitle = 'center',
            frame = FALSE, col = NA, scale = 'auto', coltitle = "black", 
            north = FALSE, south = FALSE )


plot(st_geometry(counties), col = '#d9d9d9')
zoomplot.zoom(fact = 3)
choroLayer(dat_v, var = "not.go.out.after.dark", 
           breaks = c(0, 0.5, 1),  
           lwd = 0.1, legend.pos = "topright", col = c('#2166ac','#b2182b'),
           legend.title.txt = "0 - No, 1 - Yes", 
           legend.values.rnd = 2, add = T)
layoutLayer(title = "In order to feel safe in your neighborhood do you try to not go out after dark?", postitle = 'right',
            frame = FALSE, col = NA, scale = 'auto', coltitle = "black", 
            north = FALSE, south = FALSE )


plot(st_geometry(counties), col = '#d9d9d9')
zoomplot.zoom(fact = 3)
choroLayer(dat_v, var = "keep.doors.locked", 
           breaks = c(0, 0.5, 1),  
            lwd = 0.1, legend.pos = "topright", col = c('#2166ac','#b2182b'),
           legend.title.txt = "0 - No, 1 - Yes", 
           legend.values.rnd = 2, add = T)
layoutLayer(title = "In order to feel safe in your neighborhood do you lock the doors all the time?", postitle = 'right',
            frame = FALSE, col = NA, scale = 'auto', coltitle = "black", 
            north = FALSE, south = FALSE )


plot(st_geometry(counties), col = '#d9d9d9')
zoomplot.zoom(fact = 3)
choroLayer(dat_v, var = "know.neighbors", 
           breaks = c(0, 0.5, 1),  
           lwd = 0.1, legend.pos = "topright", col = c('#2166ac','#b2182b'),
           legend.title.txt = "0 - No, 1 - Yes", 
           legend.values.rnd = 2, add = T)
layoutLayer(title = "In order to feel safe do you make sure you know your neighbors?", postitle = 'right',
            frame = FALSE, col = NA, scale = 'auto', coltitle = "black", 
            north = FALSE, south = FALSE )


