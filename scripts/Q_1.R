###########################################################################################################
###############                   Spatial Epidemiology GROUP 4       ######################################
###########################################################################################################

## Question 1

# Install necessary packages
install.packages("sp")
install.packages("sf")
install.packages("dplyr")
install.packages("tmap")

# Load required libraries
library(sp)
library(tmap)
library(sf)
library(dplyr)
library(readxl)

# Read in data.xlsx
data <- read_excel("data/data.xlsx")
head(data)

# Read in shape file
municipality.shp <- st_read("data/gemeenten_304_2025.shp")
head(municipality.shp)

# Filter data for age under 45
data_under25 <- data %>% filter(age %in% c("0-24","25-44") )
head(data_under25)

# Indirect Standardization
#rate_g = y_gs/n_gs
rate <- data_under25 %>%
  group_by(age) %>% 
  summarise(Y_d = sum(diabetics), Y_c = sum(chronical_condition), N= sum(beneficaries)) %>%
  mutate(rate_d = Y_d/N, rate_c = Y_c/N)
head(rate)

# E_i = rate_gs * n_gi
data_under25 <- left_join(data_under25,rate,by ="age") %>%
  mutate(Ei_d = rate_d * beneficaries,Ei_c = rate_c *beneficaries)
head(data_under25)

# SMR = sum(y_gi)/sum(E_gi)
result <- data_under25 %>% group_by(municipality) %>% 
  summarize(Y_d = sum(diabetics),Y_c = sum(chronical_condition),E_d=sum(Ei_d),E_c = sum(Ei_c)) %>%
  mutate(SMR_d=Y_d/E_d, SMR_c = Y_c/ E_c ) 
head(result)

# Merge data files
map <- left_join(municipality.shp,result,by= c("NAAM" = "municipality"))
map_sf <- st_as_sf(map)

# Map of SMR of diabetics 
# lecture code (v3? this code does not work well for me for some reason)
tm_shape(map) + 
  tm_polygons(fill=c("SMR_d"), 
              fill.scale  = tm_scale(values = "-brewer.rd_yl_gn"),
              fill.legend = tm_legend(title = "SMR")) +
  tm_layout(legend.outside = T,legend.outside.position="bottom") 

#v4 code
tm_shape(map) + 
  tm_polygons(
    fill = "SMR_d", 
    fill.scale = tm_scale(values = "-brewer.rd_yl_gn"),
    fill.legend = tm_legend(
      title = "SMR",
      title.size = 0.9,
      text.size = 0.8,
      position = tm_pos_out(cell.h = "right", cell.v = "center") # Places legend outside on the right
    ))

# Map of SMR of diabetics for chronic condition
tm_shape(map) + 
  tm_polygons(
    fill = "SMR_c", 
    fill.scale = tm_scale(values = "-brewer.rd_yl_gn"),
    fill.legend = tm_legend(
      title = "SMR",
      title.size = 0.9,
      text.size = 0.8,
      position = tm_pos_out(cell.h = "right", cell.v = "center") # Places legend outside on the right
    ))

### Variance of SMR

## Add standard error
map$se_SMR_d <- sqrt(map$Y_d)/map$E_d
map$se_SMR_c <- sqrt(map$Y_c)/map$E_c
head(map)
map_sf <- st_as_sf(map)

# Map of SE_SMR for diabetics
tm_shape(map_sf) +
  tm_polygons(fill=c("SMR_d","se_SMR_d"), fill.scale  = tm_scale(values = "-brewer.rd_yl_gn"))

# Map of SE_SMR for chronic disease
tm_shape(map_sf) +
  tm_polygons(fill=c("SMR_c","se_SMR_c"), fill.scale  = tm_scale(values = "-brewer.rd_yl_gn"))

#### CI of SMR
# CI for diabetics
errfact_d<-exp(1.96*sqrt(1/map$Y_d)) 
map$LL_d <- map$SMR_d/errfact_d
map$UL_d <- map$SMR_d*errfact_d
# CI for chronic condition
errfact_c<-exp(1.96*sqrt(1/map$Y_c)) 
map$LL_c <- map$SMR_c/errfact_c
map$UL_c <- map$SMR_c*errfact_c

# Table with results
library(knitr) 
# diabetics
table_d<-data.frame(map[c("NAAM","Y_d","E_d","SMR_d","LL_d","UL_d")]) 
head(table_d)
# chronic condition
table_c<-data.frame(map[c("NAAM","Y_c","E_c","SMR_c","LL_c","UL_c")]) 
head(table_c)

# Area with increased risk 
# diabetics
kable(table_d[table_d$LL_d>1,],caption="Areas with SMR significantly above 1") 
# chronic conditions
kable(table_c[table_c$LL_c>1,],caption="Areas with SMR significantly above 1") 

# Map of areas with increase risk
# diabetics
map$risk_d<-(map$LL_d>1)
# chronic disease
map$risk_c<-(map$LL_c>1)

#diabetics
map_sf <- st_as_sf(map)
tm_shape(map_sf) + 
  tm_polygons(fill=c("risk_d"),fill.scale  = tm_scale(values = c("white", "grey")),
              fill.legend = tm_legend(title = "Risk Area"))

#chronic disease
map_sf <- st_as_sf(map)
tm_shape(map_sf) + 
  tm_polygons(fill=c("risk_c"),fill.scale  = tm_scale(values = c("white", "grey")),
              fill.legend = tm_legend(title = "Risk Area"))

# Catterpillar plot
library(ggplot2)
# diabetics
ggplot(data=table_d,aes(x=reorder(NAAM,SMR_d), y=SMR_d, colour = SMR_d)) +
  geom_errorbar(aes(ymin=LL_d, ymax=UL_d, group=NAAM), width=.1) +
  geom_point() +
  xlab("Minucipality")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1, size=6))+ 
  geom_hline(yintercept=1,col="grey")

#chronic condition
ggplot(data=table_c,aes(x=reorder(NAAM,SMR_c), y=SMR_c, colour = SMR_c)) +
  geom_errorbar(aes(ymin=LL_c, ymax=UL_c, group=NAAM), width=.1) +
  geom_point() +
  xlab("Minucipality")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1, size=6))+ 
  geom_hline(yintercept=1,col="grey")



