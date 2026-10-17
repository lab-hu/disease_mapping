################################################################################
######             SPATIAL EPIDEMIOLOGY HOMEWORK 1 - GROUP 4              ######
################################################################################

## Install necessary packages 
install.packages(
  "INLA",
  repos = c(
    CRAN = "https://cloud.r-project.org",
    INLA = "https://inla.r-inla-download.org/R/stable"
  ),
  type = "binary"
)
install.packages('spDataLarge',
                 repos='https://nowosad.github.io/drat/',
                 type='source') #to load spdep library
install.packages("knitr")

## Load required libraries
library(tmap)
library(sf)
library(dplyr)
library(readxl)
library(INLA)
library(knitr) 
library(spdep)

############################# ---- Question 1 ---- ########################
# Read in data.xlsx (Note: Create a "data" folder to store the data)
data <- read_excel("data/data.xlsx")
head(data)

# Check column names
names(data)

# Check missing values per column
colSums(is.na(data))

# Check age labels
unique(data$age)

## Read in shape file
municipality.shp <- st_read("data/gemeenten_304_2025.shp")
head(municipality.shp)

## Filter data for age under 45
dat <- data %>% filter(age %in% c("0-24","25-44") )
head(dat)

## Indirect Standardization
# rate_g = Y_gs/N_gs
rate <- dat %>%
  group_by(age) %>% 
  summarise(Y_d = sum(diabetics), Y_c = sum(chronical_condition), N= sum(beneficaries)) %>%
  mutate(rate_d = Y_d/N, rate_c = Y_c/N)
head(rate)

# E_i = Rate_gs * N_gi
dat <- left_join(dat,rate,by ="age") %>%
  mutate(Ei_d = rate_d * beneficaries,Ei_c = rate_c *beneficaries)
head(dat)

# SMR = sum(Y_gi)/sum(E_gi)
result <- dat %>% group_by(municipality) %>% 
  summarize(Y_d = sum(diabetics),Y_c = sum(chronical_condition),E_d=sum(Ei_d),E_c = sum(Ei_c)) %>%
  mutate(SMR_d=Y_d/E_d, SMR_c = Y_c/ E_c ) 
head(result)

## Merge data files
map <- left_join(municipality.shp,result, by= c("NAAM" = "municipality"))
map <- map[order(map$NAAM), ]

## Map of SMRs 
# Diabetics
tm_shape(map) + 
  tm_polygons(fill=c("SMR_d"), 
              fill.scale  = tm_scale(values = "-brewer.rd_yl_gn"),
              fill.legend = tm_legend(title = "Diabetes SMR")) +
  tm_layout(legend.outside = T,legend.outside.position="bottom") 
# Chronic disease 
tm_shape(map) + 
  tm_polygons(fill=c("SMR_c"), 
              fill.scale  = tm_scale(values = "-brewer.rd_yl_gn"),
              fill.legend = tm_legend(title = "Chronic diseases SMR")) +
  tm_layout(legend.outside = T,legend.outside.position="bottom") 

## Compute standard error (SE) of SMRs
map$se_SMR_d <- sqrt(map$Y_d)/map$E_d
map$se_SMR_c <- sqrt(map$Y_c)/map$E_c
head(map)

## Map of SE_SMR 
# Diabetics
tm_shape(map_sf) +
  tm_polygons(fill=c("SMR_d","se_SMR_d"), fill.scale  = tm_scale(values = "-brewer.rd_yl_gn"))
# Chronic diseases
tm_shape(map_sf) +
  tm_polygons(fill=c("SMR_c","se_SMR_c"), fill.scale  = tm_scale(values = "-brewer.rd_yl_gn"))

## CI of SMR
# Diabetics
errfact_d<-exp(1.96*sqrt(1/map$Y_d)) 
map$LL_d <- map$SMR_d/errfact_d
map$UL_d <- map$SMR_d*errfact_d
# Chronic diseases
errfact_c<-exp(1.96*sqrt(1/map$Y_c)) 
map$LL_c <- map$SMR_c/errfact_c
map$UL_c <- map$SMR_c*errfact_c

## Table with results
# Diabetics
table_d<-data.frame(map[c("NAAM","Y_d","E_d","SMR_d","LL_d","UL_d")]) 
head(table_d)
# Chronic diseases
table_c<-data.frame(map[c("NAAM","Y_c","E_c","SMR_c","LL_c","UL_c")]) 
head(table_c)

## Area with increased risk
# Diabetics
kable(table_d[table_d$LL_d>1,],caption="Areas with SMR significantly above 1") 
# chronic diseases
kable(table_c[table_c$LL_c>1,],caption="Areas with SMR significantly above 1") 

## Map of areas with increase risk
# Diabetics
map$risk_d<-(map$LL_d>1)
# Chronic diseases
map$risk_c<-(map$LL_c>1)

# Diabetics
map_sf <- st_as_sf(map)
tm_shape(map_sf) + 
  tm_polygons(fill=c("risk_d"),fill.scale  = tm_scale(values = c("white", "grey")),
              fill.legend = tm_legend(title = "Risk Area"))

# Chronic disease
map_sf <- st_as_sf(map)
tm_shape(map_sf) + 
  tm_polygons(fill=c("risk_c"),fill.scale  = tm_scale(values = c("white", "grey")),
              fill.legend = tm_legend(title = "Risk Area"))

## Catterpillar plot
library(ggplot2)
# Diabetics
ggplot(data=table_d,aes(x=reorder(NAAM,SMR_d), y=SMR_d, colour = SMR_d)) +
  geom_errorbar(aes(ymin=LL_d, ymax=UL_d, group=NAAM), width=.1) +
  geom_point() +
  xlab("Minucipality")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1, size=6))+ 
  geom_hline(yintercept=1,col="grey")
# Chronic diseases
ggplot(data=table_c,aes(x=reorder(NAAM,SMR_c), y=SMR_c, colour = SMR_c)) +
  geom_errorbar(aes(ymin=LL_c, ymax=UL_c, group=NAAM), width=.1) +
  geom_point() +
  xlab("Minucipality")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1, size=6))+ 
  geom_hline(yintercept=1,col="grey")

######################### ---- Question 2 ---- ##############################
map.shp <- map

## Prepare diabetics data
diabetics <- list(O=map.shp$Y_d,E=map.shp$E_d,N=nrow(map.shp))
ID_d <- 1:diabetics$N
data_d <- data.frame(O=diabetics$O, E=diabetics$E,region = ID_d)
head(data_d)

## Prepare chronic diseases data
chronic <- list(O=map.shp$Y_c,E=map.shp$E_c,N=nrow(map.shp))
ID_c <- 1:chronic$N
data_c <- data.frame(O=chronic$O, E=chronic$E,region = ID_c)
head(data_c)

################# Non-Spatial Model: Poisson Lognormal  #############

## Specification of model
formula_poi <- O ~ f(region, model='iid', param=c(2,0.5))

## Model specification
# Diabetes
model_poi_d <- inla(formula_poi, 
                    family="Poisson", 
                    data= data_d,
                    E = data_d$E,
                    control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE,return.marginals.predictor=TRUE))
# Chronic diseases
model_poi_c <- inla(formula_poi, 
                    family="Poisson", 
                    data= data_c,
                    E = data_c$E,
                    control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE,return.marginals.predictor=TRUE))

## Produce general summary
summary(model_poi_d)
summary(model_poi_c)

## Estimates of intercept and precision parameters
# Diabetes
model_poi_d$summary.fixed
model_poi_d$summary.hyperpar
# Chronic diseases
model_poi_c$summary.fixed
model_poi_c$summary.hyperpar

## Derive estimates of variance parameters
# Diabetes
variance_poi_d<-inla.tmarginal(function(x) 1/x,model_poi_d$marginals.hyperpar$"Precision for region")
inla.zmarginal(variance_poi_d)
# Chronic diseases
variance_poi_c<-inla.tmarginal(function(x) 1/x,model_poi_c$marginals.hyperpar$"Precision for region")
inla.zmarginal(variance_poi_c)

## Estimates of random effects
model_poi_d$summary.random$region
model_poi_c$summary.random$region

## Estimates of RR
model_poi_d$summary.fitted.values
model_poi_c$summary.fitted.values

## RR plot
# Diabetics
map.shp$theta_poi_d <-model_poi_d$summary.fitted.values$mean
tm_shape(map.shp) + 
  tm_polygons(c("theta_poi_d"), palette = "-RdYlGn", breaks=c(0,0.5,0.6,0.8,1.2,1.5,2,2.5),contrast=c(0,1)) + 
  tm_layout(legend.title.size = 1)
# Chronic diseases
map.shp$theta_poi_c <-model_poi_c$summary.fitted.values$mean
tm_shape(map.shp) + 
  tm_polygons(c("theta_poi_c"), palette = "-RdYlGn", breaks=c(0,0.5,0.6,0.8,1.2,1.5,2,2.5),contrast=c(0,1)) + 
  tm_layout(legend.title.size = 1)


######################### ---- SPATIAL MODEL:BYM2 ---- ###############

# Define adjacency matrix
nb <- poly2nb(map.shp, queen=TRUE)

add_nb <- function(x){
  queen_nb <- poly2nb(x, queen = TRUE)
  count = card(queen_nb)
  if(!any(count==0)){
    return(queen_nb)
  }
  # Get nearest neighbour index, use centroids:
  nnbs = knearneigh(st_coordinates(st_centroid(x)))$nn
  no_edges_from = which(count==0)
  for(i in no_edges_from){
    queen_nb[[i]] = nnbs[i]
    queen_nb[[nnbs[i]]] = c(queen_nb[[nnbs[i]]],i)
  }
  return(queen_nb)
}

nb.new <- add_nb(map.shp)

# Municipalities requiring an additional neighbour
map.shp$NAAM[card(nb)==0]

# Plot neighbourhood
plot(st_geometry(map.shp))
plot(nb.new,st_coordinates(st_centroid(st_geometry(map.shp))),add=TRUE, col="red")
plot(nb,st_coordinates(st_centroid(st_geometry(map.shp))),add=TRUE)

# Write and read INLA graph
nb2INLA("adj",nb.new)
g<-inla.read.graph(filename="adj")

#### Specification of model - BYM MODEL
formula_bym2 <- O ~ f(region, model='bym2', graph = g, scale.model=TRUE, param=c(2,0.5))

#### Model Diabetes
model_bym2_d <- inla(formula_bym2, 
                  family="Poisson", 
                  data=data_d,
                  E = data_d$E,
                  control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE,return.marginals.predictor=TRUE))

#### Model Chronic diseases
model_bym2_c <- inla(formula_bym2, 
                    family="Poisson", 
                    data=data_c,
                    E = data_c$E,
                    control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE,return.marginals.predictor=TRUE))

## Produce general summary 
summary(model_bym2_d)
summary(model_bym2_c)

## Estimates of intercept and precision parameters
# Diabetes
model_bym2_d$summary.fixed
model_bym2_d$summary.hyperpar
# Chronic diseases
model_bym2_c$summary.fixed
model_bym2_c$summary.hyperpar

## Derive estimate of variance parameters
# Diabetes
variance_bym2_d<-inla.tmarginal(function(x) 1/x,model_bym2_d$marginals.hyperpar$"Precision for region")
inla.zmarginal(variance_bym2_d)
# Chronic diseases
variance_bym2_c<-inla.tmarginal(function(x) 1/x,model_bym2_c$marginals.hyperpar$"Precision for region")
inla.zmarginal(variance_bym2_c)

## Estimates of random effects
# Diabetes
model_bym2_d$summary.random$region
# Chronic diseases
model_bym2_c$summary.random$region

## Estimates of RR
# Diabetes
model_bym2_d$summary.fitted.values
# Chronic diseases
model_bym2_c$summary.fitted.values

## Make map
# Diabetes
map.shp$theta_bym2_d <-model_bym2_d$summary.fitted.values$mean
tm_shape(map.shp) + 
  tm_polygons(c("theta_bym2_d"), palette = "-RdYlGn", breaks=c(0,0.5,0.6,0.8,1.2,1.5,2,2.5),contrast=c(0,1)) + 
  tm_layout(legend.title.size = 1)
# Chronic diseases
map.shp$theta_bym2_c <-model_bym2_c$summary.fitted.values$mean
tm_shape(map.shp) + 
  tm_polygons(c("theta_bym2_c"), palette = "-RdYlGn", breaks=c(0,0.5,0.6,0.8,1.2,1.5,2,2.5),contrast=c(0,1)) + 
  tm_layout(legend.title.size = 1)

############ ---- Non-Spatial vs. Spatial model comparison ---- ##############

# WAIC & DIC

# Diabetes: Non-spatial
c(model_poi_d$waic$waic, model_poi_d$waic$p.eff)
c(model_poi_d$dic$dic, model_poi_d$dic$p.eff)
# Diabetes: Spatial
c(model_bym2_d$waic$waic, model_bym2_d$waic$p.eff)
c(model_bym2_d$dic$dic, model_bym2_d$dic$p.eff)

# Chronic diseases: Non-spatial
c(model_poi_c$waic$waic, model_poi_c$waic$p.eff)
c(model_poi_c$dic$dic, model_poi_c$dic$p.eff)
# Chronic diseases: Spatial
c(model_bym2_c$waic$waic, model_bym2_c$waic$p.eff)
c(model_bym2_c$dic$dic, model_bym2_c$dic$p.eff)
