# Data Visualisation Assignment 2
# Name: Andrew Gibbons
# Student id: D20125638
# Programme code: TU256
# Class code: Data Visualisation SPEC9995: 2020-21
# Part time, post graduate.
# 12th April 2021


# E3003 for population
# VSD30 for suicides

if(!require(csodata))install.packages('csodata')
if(!require(dplyr))install.packages('dplyr')
if(!require(ggplot2))install.packages('ggplot2')
if(!require(reshape2))install.packages('reshape2')


library('dplyr')
library('ggplot2')
library('reshape2')
# R library for downloading datasets from the CSO.
# https://cran.r-project.org/web/packages/csodata/vignettes/quick_start_guide.html
library('csodata') 

library('RColorBrewer')

####### setup for all graphs and maps

suicides <- cso_get_data('VSD30')

suicides <- dplyr::select(suicides, -Statistic)

shp <- cso_get_geo('Administrative Counties')

pop <- cso_get_data('E3003')
allAgespop <- dplyr::filter(pop, Single.Year.of.Age == 'All ages')
allAgespop <- dplyr::filter(allAgespop, County.and.City != 'State')
allAgespop <- dplyr::filter(allAgespop, Sex != 'Both sexes')
allAgespop <- dplyr::select(allAgespop, -Single.Year.of.Age, -Statistic)

allAgespop <- dplyr::rename(allAgespop, Area.of.Residence = County.and.City)


########### setup for maps ================

mapCounties <- c( 
 "Limerick City"    ,           "Limerick County"   ,         
 "North Tipperary"     ,        "South Tipperary"     ,       
 "Waterford City"     ,         "Waterford County"   ,        
 "Galway City"         ,        "Galway County"   ,           
 "Leitrim County"     ,         "Mayo County"    ,            
 "Roscommon County"   ,         "Sligo County"    ,           
 "Cavan County"      ,          "Donegal County"  ,           
 "Monaghan County"   ,          "Carlow County"   ,           
 "Dublin City"       ,          "South Dublin"     ,          
 "Fingal"             ,         "D\xfan Laoghaire-Rathdown", # fix this one.
 "Kildare County"     ,         "Kilkenny County"   ,         
 "Laois County"        ,        "Longford County"  ,          
 "Louth County"        ,        "Meath County"     ,          
 "Offaly County"       ,        "Westmeath County"   ,        
 "Wexford County"      ,        "Wicklow County"   ,          
 "Clare County"        ,        "Cork City"       ,           
 "Cork County"         ,        "Kerry County" ) # 34 values

mapSuicideCounties <- c( # in replacment order.
'Limerick City and County', 'Limerick City and County',
'Tipperary', 'Tipperary',
'Waterford City and County','Waterford City and County',
'Galway City', 'Galway County', 
'Leitrim', 'Mayo','Roscommon','Sligo','Cavan','Donegal',
'Monaghan','Carlow',
'Dublin City', 'South Dublin' , 'Fingal' , 'Dún Laoghaire-Rathdown'  ,
'Kildare' ,  'Kilkenny' , 'Laois'  , 'Longford'  , 'Louth'     ,
'Meath', 'Offaly',              
'Westmeath',            'Wexford'      ,            
'Wicklow' ,                  'Clare'   ,                 
'Cork City' , 'Cork County',  'Kerry'   )    # 34 values                
               



########## suicides for 2016 ####################

suicides2016 <- dplyr::select(suicides,Area.of.Residence,Sex,`2016`)

## remove the state. probably not that necessary.
suicides2016 <- dplyr::filter(suicides2016, `Area.of.Residence` != 'State')

# remove both sexes.
suicides2016 <- dplyr::filter(suicides2016, Sex != 'Both sexes')

# remove north and south tipp
suicides2016 <- dplyr::filter(suicides2016, `Area.of.Residence` != 'North Tipperary')
suicides2016 <- dplyr::filter(suicides2016, `Area.of.Residence` != 'South Tipperary')

# fix Dún Laoghaire-Rathdown to fix the spelling

levels <- levels(suicides2016$Area.of.Residence)
levels[length(levels) + 1] <- "Dún Laoghaire-Rathdown"
suicides2016$Area.of.Residence <- factor(suicides2016$Area.of.Residence, levels = levels)
suicides2016$Area.of.Residence[(suicides2016$Area.of.Residence == 'Dun Laoghaire-Rathdown')] <- 'Dún Laoghaire-Rathdown'

# fix Waterford County / Waterford City by merging them 

levels <- levels(suicides2016$Area.of.Residence)
levels[length(levels) + 1] <- "Waterford City and County"
suicides2016$Area.of.Residence <- factor(suicides2016$Area.of.Residence, levels = levels)

tmp <- dplyr::filter(suicides2016, stringr::str_detect(Area.of.Residence,'Waterford'))

tmpImport <- data.frame(Area.of.Residence= 'Waterford City and County',Sex= 'Male', s = as.numeric(sum(tmp[tmp$Sex == 'Male',3])))
names(tmpImport) <- c("Area.of.Residence",  "Sex" , toString("2016"))
suicides2016 <- rbind(suicides2016,tmpImport)

tmpImport <- data.frame(Area.of.Residence= 'Waterford City and County',Sex= 'Female', s = as.numeric(sum(tmp[tmp$Sex == 'Female',3])))
names(tmpImport) <- c("Area.of.Residence",  "Sex" , toString("2016"))
suicides2016 <- rbind(suicides2016,tmpImport)

# fix Limerick County / Limerick City by merging them
levels <- levels(suicides2016$Area.of.Residence)
levels[length(levels) + 1] <- "Limerick City and County"
suicides2016$Area.of.Residence <- factor(suicides2016$Area.of.Residence, levels = levels)
tmp <- dplyr::filter(suicides2016, stringr::str_detect(Area.of.Residence,'Limerick'))

tmpImport <- data.frame(Area.of.Residence= 'Limerick City and County',Sex= 'Male', s = as.numeric(sum(tmp[tmp$Sex == 'Male',3])))
names(tmpImport) <- c("Area.of.Residence",  "Sex" , toString("2016"))
suicides2016 <- rbind(suicides2016,tmpImport)

tmpImport <- data.frame(Area.of.Residence= 'Limerick City and County',Sex= 'Female', s = as.numeric(sum(tmp[tmp$Sex == 'Female',3])))
names(tmpImport) <- c("Area.of.Residence",  "Sex" , toString("2016"))
suicides2016 <- rbind(suicides2016,tmpImport)

# the population for these areas.

## select only the 2016 years by subtracting the 2011 year.
allAgespop2016 <- dplyr::select(allAgespop, -`2011`) 

totals <- merge(allAgespop2016, suicides2016, by = c('Area.of.Residence','Sex'))

totals <- dplyr::mutate(totals, per100k = (`2016.y` / `2016.x` * 100000))

totalsCast <- reshape2::dcast(totals, Area.of.Residence ~ Sex)
# result        Area.of.Residence      Male   Female

totalsCast <- totalsCast %>% dplyr::mutate(malePos = Male)

totalsCast <- totalsCast %>% dplyr::select(-Male)

totalsCast <- totalsCast %>% dplyr::mutate( Male = malePos * -1)

totalsCast <- totalsCast %>% dplyr::mutate(femalePos = Female)

totalsCast <- totalsCast %>% dplyr::mutate(TotalPos = (femalePos + malePos))

forGraph <- reshape2::melt(totalsCast,value.name='per100k',id = c('Area.of.Residence','malePos','femalePos','TotalPos'))

gg <- ggplot(data = forGraph, aes(x = reorder(Area.of.Residence,malePos),y= per100k, fill = as.factor(variable)))   + geom_bar(stat='identity',position = 'stack') + coord_flip() + labs(title = "Male Suicide by County (Ireland 2016)",   x="County", y = "Suicides per 100k population") + scale_fill_manual(name = "Sex",labels=c('Male' = 'Male','Female' = 'Female'),values=c('Male' = 'blue','Female' = 'orange')) + theme(legend.position = c(0.2, 0.2))
ggsave(plot=gg,"2016Suicides_Males.png", width=5.2, height= 7.4)

gg <- ggplot(data = forGraph, aes(x = reorder(Area.of.Residence,femalePos),y= per100k, fill = as.factor(variable))) + geom_bar(stat='identity',position = 'stack') + coord_flip() + labs(title = "Female Suicide by County (Ireland 2016)", x="County", y = "Suicides per 100k population") + scale_fill_manual(name = "Sex",labels=c('Male' = 'Male','Female' = 'Female'),values=c('Male' = 'blue','Female' = 'orange')) + theme(legend.position = c(0.15, 0.2))
ggsave(plot=gg,"2016Suicides_Females.png", width=5.2, height= 7.4)

gg <- ggplot(data = forGraph, aes(x = reorder(Area.of.Residence,TotalPos),y= per100k, fill = as.factor(variable))) + geom_bar(stat='identity',position = 'stack') + coord_flip() + labs(title = "Suicides by County (Ireland 2016)", x="County", y = "Suicides per 100k population") + scale_fill_manual(name = "Sex",labels=c('Male' = 'Male','Female' = 'Female'),values=c('Male' = 'blue','Female' = 'orange')) + theme(legend.position = c(0.15, 0.2))
ggsave(plot=gg,"2016Suicides_Totals.png", width=5.2, height= 7.4)

## additional - maps.
# method. 
# add new column to map.
# for each county name, add the value from the suicide dataframe to the map dataframe.
# display.

addSuicides <- function(map, suicides, mapCounties, suicideCounties){

   # for the males

    maleSuicides <- dplyr::filter(suicides, variable == 'Male')    

  ## critically, the map names index must match up with the suicide names index.
  for( i in 1:length(mapCounties)) 
    {
    map[(map$COUNTYNAME == mapCounties[i]),23] <- maleSuicides[(maleSuicides$Area.of.Residence == mapSuicideCounties[i]),6] 
    }

    femaleSuicides <- dplyr::filter(suicides, variable == 'Female')    

  for( i in 1:length(mapCounties))
   { 
   map[(map$COUNTYNAME == mapCounties[i]),22] <- femaleSuicides[(femaleSuicides$Area.of.Residence == mapSuicideCounties[i]),6]    
   }

 return (map)
}



shp$suicidesMale <- 0
shp$suicidesFemale <- 0

suicidemap <- addSuicides(shp,forGraph,mapCounties,mapSuicideCounties)

t <- tm_shape(suicidemap) + tm_fill(col="suicidesMale", palette = viridisLite::viridis(20),colorNA = "grey50", legend.reverse = TRUE,style="cont",title='Male Suicides 2016') + tm_borders(col = "black", lwd = 1)

tmap_save(tm = t, 'suicideMapIrelandMale2016.png')

t <- tm_shape(suicidemap) + tm_fill(col="suicidesFemale", palette = viridisLite::viridis(20),colorNA = "grey50", legend.reverse = TRUE,style="cont",title='Female Suicides 2016') + tm_borders(col = "black", lwd = 1)

tmap_save(tm = t, 'suicideMapIrelandFemale2016.png')


 



#  2011 suicides #######################################################################

suicides2011 <- dplyr::select(suicides,Area.of.Residence,Sex,`2011`)

## remove the state. probably not that necessary.
suicides2011 <- dplyr::filter(suicides2011, `Area.of.Residence` != 'State')

# remove both sexes.
suicides2011 <- dplyr::filter(suicides2011, Sex != 'Both sexes')

# fix north and south Tipperary into one Tipperary

tmp <- dplyr::filter(suicides2011, stringr::str_detect(Area.of.Residence,'Tipp'))
# initialize
tmp[(tmp$Area.of.Residence == 'Tipperary'),3] = 0
tmp[(tmp$Sex == 'Male' & tmp$Area.of.Residence == 'Tipperary'),3] = as.numeric(sum(tmp[tmp$Sex == 'Male',3]))
tmp[(tmp$Sex == 'Female' & tmp$Area.of.Residence == 'Tipperary'),3] = as.numeric(sum(tmp[tmp$Sex == 'Female',3]))

# initialize dataframe
suicides2011[(suicides2011$Area.of.Residence == 'Tipperary'),3] = 0
suicides2011[(suicides2011$Area.of.Residence == 'Tipperary' & suicides2011$Sex == 'Male'),3] = as.numeric(sum(tmp[tmp$Sex == 'Male'& tmp$Area.of.Residence == 'Tipperary',3]))
suicides2011[(suicides2011$Area.of.Residence == 'Tipperary' & suicides2011$Sex == 'Female'),3] = as.numeric(sum(tmp[tmp$Sex == 'Female'& tmp$Area.of.Residence == 'Tipperary',3]))

# check
suicides2011[suicides2011$Area.of.Residence == 'Tipperary',]

suicides2011 <- dplyr::filter(suicides2011, `Area.of.Residence` != 'North Tipperary')
suicides2011 <- dplyr::filter(suicides2011, `Area.of.Residence` != 'South Tipperary')

# fix Dún Laoghaire-Rathdown to fix the spelling

levels <- levels(suicides2011$Area.of.Residence)
levels[length(levels) + 1] <- "Dún Laoghaire-Rathdown"
suicides2011$Area.of.Residence <- factor(suicides2011$Area.of.Residence, levels = levels)
suicides2011$Area.of.Residence[(suicides2011$Area.of.Residence == 'Dun Laoghaire-Rathdown')] <- 'Dún Laoghaire-Rathdown'

# fix Waterford County / Waterford City by merging them 

levels <- levels(suicides2011$Area.of.Residence)
levels[length(levels) + 1] <- "Waterford City and County"
suicides2011$Area.of.Residence <- factor(suicides2011$Area.of.Residence, levels = levels)

tmp <- dplyr::filter(suicides2011, stringr::str_detect(Area.of.Residence,'Waterford'))

tmpImport <- data.frame(Area.of.Residence= 'Waterford City and County',Sex= 'Male', s = as.numeric(sum(tmp[tmp$Sex == 'Male',3])))
names(tmpImport) <- c("Area.of.Residence",  "Sex" , toString("2011"))
suicides2011 <- rbind(suicides2011,tmpImport)

tmpImport <- data.frame(Area.of.Residence= 'Waterford City and County',Sex= 'Female', s = as.numeric(sum(tmp[tmp$Sex == 'Female',3])))
names(tmpImport) <- c("Area.of.Residence",  "Sex" , toString("2011"))
suicides2011 <- rbind(suicides2011,tmpImport)

# fix Limerick County / Limerick City by merging them
levels <- levels(suicides2011$Area.of.Residence)
levels[length(levels) + 1] <- "Limerick City and County"
suicides2011$Area.of.Residence <- factor(suicides2011$Area.of.Residence, levels = levels)
tmp <- dplyr::filter(suicides2011, stringr::str_detect(Area.of.Residence,'Limerick'))

tmpImport <- data.frame(Area.of.Residence= 'Limerick City and County',Sex= 'Male', s = as.numeric(sum(tmp[tmp$Sex == 'Male',3])))
names(tmpImport) <- c("Area.of.Residence",  "Sex" , toString("2011"))
suicides2011 <- rbind(suicides2011,tmpImport)

tmpImport <- data.frame(Area.of.Residence= 'Limerick City and County',Sex= 'Female', s = as.numeric(sum(tmp[tmp$Sex == 'Female',3])))
names(tmpImport) <- c("Area.of.Residence",  "Sex" , toString("2011"))
suicides2011 <- rbind(suicides2011,tmpImport)

# the population for these areas.
## remove the '2016' column to leave the 2011 column
pop2011 <- dplyr::select(allAgespop, -`2016`)

totals2011 <- merge(pop2011, suicides2011, by = c('Area.of.Residence','Sex'))

totals2011 <- dplyr::mutate(totals2011, per100k = (`2011.y` / `2011.x` * 100000))

totalsCast2011 <- reshape2::dcast(totals2011, Area.of.Residence ~ Sex,value.var = 'per100k')
# result        Area.of.Residence      Male   Female

totalsCast2011 <- totalsCast2011 %>% dplyr::mutate(malePos = Male)

totalsCast2011 <- totalsCast2011 %>% dplyr::select(-Male)

totalsCast2011 <- totalsCast2011 %>% dplyr::mutate( Male = malePos * -1)

totalsCast2011 <- totalsCast2011 %>% dplyr::mutate(femalePos = Female)

totalsCast2011 <- totalsCast2011 %>% dplyr::mutate(TotalPos = (femalePos + malePos))

forGraph2011 <- reshape2::melt(totalsCast2011,value.name='per100k',id = c('Area.of.Residence','malePos','femalePos','TotalPos'))

gg <- ggplot(data = forGraph2011, aes(x = reorder(Area.of.Residence,malePos),y= per100k, fill = as.factor(variable)))   + geom_bar(stat='identity',position = 'stack') + coord_flip() + labs(title = "Male Suicides by County (Ireland 2011)",   x="County", y = "Suicides per 100k population") + scale_fill_manual(name = "Sex",labels=c('Male' = 'Male','Female' = 'Female'),values=c('Male' = 'blue','Female' = 'orange')) + theme(legend.position = c(0.15, 0.2))
ggsave(plot=gg,"2011Suicides_Males.png", width=5.2, height= 7.4)

gg <- ggplot(data = forGraph2011, aes(x = reorder(Area.of.Residence,femalePos),y= per100k, fill = as.factor(variable))) + geom_bar(stat='identity',position = 'stack') + coord_flip() + labs(title = "Female Suicides by County (Ireland 2011)", x="County", y = "Suicides per 100k population") + scale_fill_manual(name = "Sex",labels=c('Male' = 'Male','Female' = 'Female'),values=c('Male' = 'blue','Female' = 'orange')) + theme(legend.position = c(0.15, 0.48))
ggsave(plot=gg,"2011Suicides_Females.png", width=5.2, height= 7.4)

gg <- ggplot(data = forGraph2011, aes(x = reorder(Area.of.Residence,TotalPos),y= per100k, fill = as.factor(variable))) + geom_bar(stat='identity',position = 'stack') + coord_flip() + labs(title = "Suicides by County (Ireland 2011)", x="County", y = "Suicides per 100k population") + scale_fill_manual(name = "Sex",labels=c('Male' = 'Male','Female' = 'Female'),values=c('Male' = 'blue','Female' = 'orange')) + theme(legend.position = c(0.15, 0.2))
ggsave(plot=gg,"2011Suicides_Totals.png", width=5.2, height= 7.4)

##### suicide trends by top suicide counties for Males #########################################

suicidesByArea <- dplyr::filter(suicides, Area.of.Residence == 'Wexford' | Area.of.Residence == 'Galway City' | Area.of.Residence == 'Cork County' | Area.of.Residence == 'Limerick City and County' | Area.of.Residence == 'Carlow' | Area.of.Residence == 'Monaghan' | Area.of.Residence == 'Cork City' | Area.of.Residence == 'Cavan' | Area.of.Residence == 'Offaly' | Area.of.Residence == 'Kerry')

suicidesByArea <- dplyr::filter(suicidesByArea, Sex != 'Both sexes')

suicidesByArea <- dplyr::select(suicidesByArea,-Statistic)

# for males
suicidesByAreaMale <- dplyr::filter(suicidesByArea, Sex == 'Male')

suicidesByAreaMale <- dplyr::select(suicidesByAreaMale, -Sex)

forGraphByArea <- reshape2::melt(suicidesByAreaMale, value.name = 'Year', id = c('Area.of.Residence','transparent'))

# set the graph legend in order
forGraphByArea$Area.of.Residence <- factor(forGraphByArea$Area.of.Residence, levels = c('Cork County','Wexford','Kerry','Cork City','Cavan','Galway City','Offaly','Monaghan','Limerick City and County','Carlow'))

# graph for males
gg <- ggplot(data = forGraphByArea, aes(x = variable, y = Year, color = Area.of.Residence)) + geom_smooth(method = loess ,aes(group=Area.of.Residence),size=2,se = FALSE) + labs(title = "Male suicide trends per top county (Ireland 2008-2019)", x="Year of Suicide", y = "Total Suicides") + theme(legend.position = c('0.1','0.7') ) +
scale_color_manual(name = "County",labels=c('Cork County' = 'Cork County','Wexford' = 'Wexford','Cork City' = 'Cork City','Cavan' = 'Cavan','Galway City' = 'Galway City','Offaly' = 'Offaly','Monaghan' = 'Monaghan','Limerick City and County' = 'Limerick City and County','Carlow' = 'Carlow','Kerry' = 'Kerry'), values=c('Cork County' = '#ff7f00','Wexford' = '#33a02c','Cork City' = '#ccebc5','Cavan' = 'blue','Galway City' = '#ccebc5','Offaly' = '#ccebc5','Monaghan' = '#ccebc5','Limerick City and County' = '#a6cee3','Carlow' = '#ccebc5', 'Kerry' = '#e31a1c')) + scale_x_discrete(guide = guide_axis(angle = 30))
    
ggsave(plot=gg,"2008-2019_suicidesMale.png", width=5.2, height= 7.4)

##### suicide trends by top suicide counties for Females #########################################

suicidesByAreaFemale <- dplyr::filter(suicides, Area.of.Residence == 'Leitrim' | Area.of.Residence == 'Galway County' | Area.of.Residence == 'Galway City' | Area.of.Residence == 'Laois' | Area.of.Residence == 'Cavan' | Area.of.Residence == 'Cork City' | Area.of.Residence == 'Westmeath' | Area.of.Residence == 'Kerry')

suicidesByAreaFemale <- dplyr::filter(suicidesByAreaFemale, Sex != 'Both sexes')

# for Females
suicidesByAreaFemale <- dplyr::filter(suicidesByAreaFemale, Sex == 'Female')
suicidesByAreaFemale <- dplyr::select(suicidesByAreaFemale, -Sex,-Statistic)

forGraphByArea <- reshape2::melt(suicidesByAreaFemale, value.name = 'Year', id = c('Area.of.Residence'))

# set the graph legend in order of height on graph
forGraphByArea$Area.of.Residence <- factor(forGraphByArea$Area.of.Residence, levels = c('Kerry','Galway County','Galway City','Cork City','Cavan','Westmeath','Laois','Leitrim'))

# graph for females
gg <- ggplot(data = forGraphByArea, aes(x = variable, y = Year, color = Area.of.Residence)) + geom_smooth(method = loess ,aes(group=Area.of.Residence),size=2,se = FALSE) + labs(title = "Female suicide trends per top county (Ireland 2008-2019)", x="Year of Suicide", y = "Total Suicides") + theme(legend.position = c('0.3','0.7') ) + 
scale_color_manual(name = "County",labels=c('Leitrim' = 'Leitrim','Galway County' = 'Galway County','Galway City' = 'Galway City','Cork City' = 'Cork City','Westmeath'='Westmeath','Laois' = 'Laois','Cavan' = 'Cavan', 'Kerry' = 'Kerry'), values=c('Kerry' = 'black', 'Galway County' = '#ff7f00','Westmeath' = '#ccebc5','Laois' = '#ccebc5','Cork City'= '#ccebc5','Cavan' = '#e31a1c','Galway City' = 'blue','Leitrim' = '#ccebc5')) + scale_x_discrete(guide = guide_axis(angle = 30))

ggsave(plot=gg,"2008-2019_suicidesFemales.png", width=5.2, height= 7.4)


############## population trends by top suicide counties Male and Female #######################

popByArea <- dplyr::filter(allAgespop, Sex == 'Male' | Sex == 'Female')
## all the counties that are in the top 6 for 2011 and 2016, both sexes.
popByArea <- dplyr::filter(popByArea, Area.of.Residence == 'Wexford' | Area.of.Residence == 'Galway City'| Area.of.Residence == 'Cork County' | 
Area.of.Residence == 'Limerick City and County' | Area.of.Residence == 'Carlow' | Area.of.Residence == 'Monaghan' | Area.of.Residence == 'Cork City' | 
Area.of.Residence == 'Cavan' | Area.of.Residence == 'Offaly' | Area.of.Residence == 'Leitrim' | Area.of.Residence == 'Kerry'| Area.of.Residence == 'Galway County' | 
Area.of.Residence == 'Galway City' | Area.of.Residence == 'Laois' | Area.of.Residence == 'Cork City' | Area.of.Residence == 'Westmeath' | Area.of.Residence == 'Kerry')

popByAreaMelt <- reshape2::melt(popByArea, value.name = 'Year', id = c('Area.of.Residence','Sex'))

gg <- ggplot(data = popByAreaMelt, aes(x = variable, y = Year, fill = Sex)) + geom_bar(aes(group=Sex),stat='identity',position = 'dodge') +  facet_wrap(. ~ reorder(Area.of.Residence,-Year)) + 
scale_fill_manual(name = "Sex",labels=c('Male' = 'Male','Female' = 'Female'),values=c('Male' = 'blue','Female' = 'orange')) +
labs(title = "Ireland 2011 and 2016 populations for top suicide Areas", x="Year", y = "Population total")
ggsave(plot=gg,"2011-2016_PopulationAreas.png", width=10.4, height= 14.8)


################## suicides for all counties. 2008 to 2019 ######################################

suicidesAll <- dplyr::filter(suicides, `Area.of.Residence` != 'State')
suicidesAll <- dplyr::filter(suicidesAll, `Area.of.Residence` != 'Leinster')
suicidesAll <- dplyr::filter(suicidesAll, `Area.of.Residence` != 'Border')
suicidesAll <- dplyr::filter(suicidesAll, `Area.of.Residence` != 'Midland')
suicidesAll <- dplyr::filter(suicidesAll, `Area.of.Residence` != 'Munster')
suicidesAll <- dplyr::filter(suicidesAll, `Area.of.Residence` != 'Ulster (part of)')
suicidesAll <- dplyr::filter(suicidesAll, `Area.of.Residence` != 'West')
suicidesAll <- dplyr::filter(suicidesAll, `Area.of.Residence` != 'Mid-East')
suicidesAll <- dplyr::filter(suicidesAll, `Area.of.Residence` != 'Mid-West')
suicidesAll <- dplyr::filter(suicidesAll, `Area.of.Residence` != 'South-East')
suicidesAll <- dplyr::filter(suicidesAll, `Area.of.Residence` != 'South-West')
suicidesAll <- dplyr::filter(suicidesAll, Sex != 'Both sexes')

forGraphAll <- reshape2::melt(suicidesAll, value.name = 'Year', id = c('Area.of.Residence','Sex'))

# for males only
suicidesAllMale <- dplyr::filter(suicidesAll, Sex == 'Male')
forGraphAll <- reshape2::melt(suicidesAllMale, value.name = 'Year', id = c('Area.of.Residence','Sex'))


gg <- ggplot(data = forGraphAll, aes(x = variable, y = Year, color = Area.of.Residence)) + geom_line(size=1,aes(group=Area.of.Residence))+labs(title = "Ireland all counties Male suicides", x="Year", y = "suicides total")
ggsave(plot=gg,"2008-2019_suicidesMaleAllCounties.png", width=5.2, height= 7.4)



