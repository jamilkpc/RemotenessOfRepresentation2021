library(ggplot2)
library(MASS)
library(dplyr)
library(tidyr)
library(magrittr)

dfFull <- readRDS("/home/jamil/Downloads/JoD/NewData/PP-map.rds")

# Figure 1
library(scales)
cubeRootScale <- function(x){x^(1/3)}
cubeScale <- function(x){x^3}
cubeRootTrans = trans_new('cubeRoot',cubeRootScale,cubeScale)
perMillion = function(x){round(x/1000000)}
qog_plot = dfFull %>%
  filter(democracia==1) %>%
  mutate(wdiPop = wdiPop, 
         Country.Code = countrycode::countrycode(Country.Code,'iso3c','iso2c')) %>%
  select(Country.Code,nos,wdiPop) %>%
  drop_na
mm = 1000000
ggplot(qog_plot, aes(x=wdiPop, y=nos, label = 'o')) +
  scale_x_continuous(trans = cubeRootTrans,
                     breaks = c(0,mm,10*mm,50*mm,100*mm,200*mm,500*mm,1000*mm,1500*mm),
                     limits = c(0,1500000000),
                     labels = perMillion) +
  scale_y_continuous(limits = c(0,800)) +
  geom_text() +
  geom_abline(aes(slope = 1, intercept = 0, color = 'black')) +
  geom_abline(aes(slope = 0.9, intercept = 0, color = 'gray40')) +
  geom_abline(aes(slope = 0.5, intercept = 0, color = 'gray80')) +
  xlab("Population (In Millions) on Cube Root Scale") + ylab("Number of Representatives") + 
  scale_color_identity(labels=c("Cube Root Rule","90% Cube Root Rule", "50% Cube Root Rule"), guide="legend") +
  theme_linedraw()

# Figure 2
qog_map = dfFull %>% 
  select(Country.Code,proximityWDI,wdiPop,democracia) %>%
  drop_na %>%
  mutate(proxFactor = (democracia==1)*((proximityWDI>0) + (proximityWDI>0.7) + (proximityWDI>0.9) + (proximityWDI>1) + (proximityWDI>1.1)) ) %>% 
  rbind(c('GRL', 0.9952099, 5818553, TRUE, 3))
write.csv(qog_map, file = 'newMap.csv')

library(rworldmap)
matched <- joinCountryData2Map(qog_map, nameJoinColumn="Country.Code")
matched <- subset(matched, continent != "Antarctica")
matched$ADMIN=='Greenland'

colourPalette <- c('gray50',
                   'red3',
                   'orange',
                   'yellow',
                   'palegreen1',
                   'palegreen4')

par(mar=c(1, 0, 0, 0))
mapParams <- mapCountryData(matched,
                            nameColumnToPlot='proxFactor',
                            catMethod='categorical',
                            colourPalette=colourPalette,
                            addLegend=FALSE,
                            mapTitle='',
                            oceanCol="lightblue",
                            missingCountryCol='gray95')

#changing legendText
mapParams$legendText <- c('Autocracy',
                          'Very Remote (Below 70%)',
                          'Remote (70%-89%)',
                          'Slightly Remote (90%-99%)',
                          'Proximate (100%-109%)',
                          'Very proximate (Above 110%)')
#add legend
do.call(addMapLegendBoxes, c(mapParams, x='bottom', ncol = 2, title = 'Remoteness of Representation (Fulfillment of Cube Root Rule)'))


