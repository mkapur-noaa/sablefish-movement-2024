## Standalone/Descriptive figures for manuscripts and tech memos
## This does NOT have functions for ploting OM/EM outputs, just raw input data and maps
## Kapur M 
library(sf)
library(here)
library(tibble)
require(ggplot2)
require(dplyr)
require(patchwork)
require(ggsidekick)
require(rgdal)
# library(marmap)
sf::sf_use_s2(FALSE)#https://stackoverflow.com/questions/68343004/ggplot-and-geom-sf-not-showing-graticule-and-tick-marks-on-world-map

## Updated F1 map of strata using Luke's approach ----
load(here('input','downloads','eez_nepac_regions.rda'))
regions <- eez_nepac_regions
st_crs(regions) <- st_crs(regions)# source("./R/breakpoints_map.R") ## makes the breakpoint lines & clips



usa <- map_data("world") %>%
  filter(long  > -180 & long < -110, lat > 30 & lat < 75 ) 
  # mutate( x = ifelse(long < 0, long*-1,long), y = lat) 
 
# https://stackoverflow.com/questions/29736577/how-to-convert-data-frame-to-spatial-coordinates
spdf_usa <-st_as_sf(x = usa, 
                    coords = c("long", "lat"),
                    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# st_crs(usa) <- st_crs(regions)

regions$Region_Name <- c('Alaska (AK)',   'British Columbia (BC)',  "California Current (CC)")
## orthogonal clips
# load(here('input','downloads',"sub_area_clips_B1.Rdata")) ## default clips for original 7 areas
# load(here('input','downloads',"sub_area_clips_B2.Rdata")) ## default clips for original 7 areas
# load(here("input","downloads","n36_shape.rda"))
# load(here("input","downloads","n50_shape.rda"))

## 50N flat clips
load(here('input','downloads',"sub_area_clips_50N.Rdata") )

## ocean currents
# shape <- readOGR(dsn = "C:/Users/MKapur/Dropbox/UW/sab-growth/raw_data/Major_Ocean_Currents_arrowPolys_30m_8", 
#                  layer = 'Major_Ocean_Currents_arrowPolys_30m_8')
# 
# shapefile_df <- fortify(shape)
# shapefile_df$long <- shapefile_df$long+360
# currents.col <- c("#000000", "#009E73", "#0072B2","#e79f00","#e79f00")

# x1 <- rep(180, 4); x1end <- rep(190, 4)
# y1 <- rev(seq(30,35,length.out = 4))

mapnames = expand.grid(c('mgmt','stock','subarea'), c('nolabs','labs')) %>% arrange(Var1)


for(i in 1:nrow(mapnames)){
  ggplot(data = regions) + 
    theme_classic(base_size = 14) +
    coord_sf(xlim = c(165, 245), ylim = c(26, 65)) +
    labs(x ="",y="") + 
    theme(legend.position = 'none',panel.grid = element_blank(),
          plot.background  = element_rect(color = NA, fill = NA),
          panel.background = element_rect(color = NA, fill = NA))+
    
    ## mgmt fill
    {if(mapnames$Var1[i] == 'mgmt') geom_sf(data = clips[[1]], fill = mgmtPal[1], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'mgmt')  geom_sf(data = clips[[2]], fill = mgmtPal[1], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'mgmt')  geom_sf(data = clips[[3]], fill = mgmtPal[2], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'mgmt')  geom_sf(data = clips[[4]], fill = mgmtPal[2], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'mgmt')  geom_sf(data = clips[[5]], fill = mgmtPal[3], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'mgmt')  geom_sf(data = clips[[6]], fill = mgmtPal[3], alpha = 0.9, color = NA)} +
    
    ## mgmt labs
    {if(i == 2)  geom_sf_label(aes(label = Region_Name, color = Region_Name),size = 4, fill = 'white')} +
    {if(i == 2)  scale_color_manual(values = mgmtPal)} +
    ## stock fill
    {if(mapnames$Var1[i] == 'stock') geom_sf(data = clips[[1]], fill = demPal[1], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'stock')  geom_sf(data = clips[[2]], fill = demPal[2], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'stock')  geom_sf(data = clips[[3]], fill = demPal[2], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'stock')  geom_sf(data = clips[[4]], fill = demPal[3], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'stock')  geom_sf(data = clips[[5]], fill = demPal[3], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'stock')  geom_sf(data = clips[[6]], fill = demPal[4], alpha = 0.9, color = NA)} +
    
    ## stock labs
    {if(i == 4)  geom_sf_label(data = clips[[1]],aes(label = 'Stock 4'),size = 4, fill = 'white', color = demPal[1])} +
    {if(i == 4)  geom_sf_label(data = clips[[2]],aes(label = 'Stock 3'),size = 4, fill = 'white', color = demPal[2])} +
    {if(i == 4)  geom_sf_label(data = clips[[4]],aes(label = 'Stock 2'),size = 4, fill = 'white', color = demPal[3])} +
    {if(i == 4)  geom_sf_label(data = clips[[6]],aes(label = 'Stock 1'),size = 4, fill = 'white', color = demPal[4])} +
    
    
    ## subarea fill
    {if(mapnames$Var1[i] == 'subarea') geom_sf(data = clips[[1]], fill = subareaPal[1], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'subarea')  geom_sf(data = clips[[2]], fill = subareaPal[2], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'subarea')  geom_sf(data = clips[[3]], fill = subareaPal[3], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'subarea')  geom_sf(data = clips[[4]], fill = subareaPal[4], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'subarea')  geom_sf(data = clips[[5]], fill = subareaPal[5], alpha = 0.9, color = NA)} +
    {if(mapnames$Var1[i] == 'subarea')  geom_sf(data = clips[[6]], fill = subareaPal[6], alpha = 0.9, color = NA)} +
    
    ## subarea labs
    {if(i == 6)  geom_sf_label(data = clips[[1]],aes(label = 'A4'),size = 4, fill = 'white', color = subareaPal[1])} +
    {if(i == 6)  geom_sf_label(data = clips[[2]],aes(label = 'A3'),size = 4, fill = 'white', color = subareaPal[2])} +
    {if(i == 6)  geom_sf_label(data = clips[[3]],aes(label = 'B3'),size = 4, fill = 'white', color = subareaPal[3])} +
    {if(i == 6)  geom_sf_label(data = clips[[4]],aes(label = 'B2'),size = 4, fill = 'white', color = subareaPal[4])} +
    {if(i == 6)  geom_sf_label(data = clips[[5]],aes(label = 'C2'),size = 4, fill = 'white', color = subareaPal[5])} +
    {if(i == 6)  geom_sf_label(data = clips[[6]],aes(label = 'C1'),size = 4, fill = 'white', color = subareaPal[6])} 
    
      ggsave(last_plot(),
             height = 6, width = 8, dpi = 520,
             file = here('figs','maps',paste0(i,"2_",mapnames$Var1[i],"_",mapnames$Var2[i],".png")))
}


## EM PLOTS ----

mapnames = data.frame('Var1' = c('EM1','EM2','EMs 3&4','EM5'))
plist = list()

for(i in 1:nrow(mapnames)){
  plist[[i]] <-
    ggplot(data = regions) + 
    coord_sf(xlim = c(165, 245), ylim = c(26, 65)) +
    labs(x ="",y="") + 
    theme(legend.position = 'none',panel.grid = element_blank(),
          plot.background  = element_rect(color = NA, fill = '#f5f6f8'),
          panel.background = element_rect(color = NA, fill = '#f5f6f8'))+
    
    # geom_polygon(data = usa, aes(x = long, y = lat, group = group),   fill = 'black') +
    # geom_sf(data = spdf_usa, fill= 'black') +
    ## EM1 (like the OM): 6 areas (alpha), 4 stocks (colors)
    {if(mapnames$Var1[i] == 'EM1') geom_sf(data = clips[[1]], fill = demPal[1], alpha = 0.9, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EM1')  geom_sf(data = clips[[2]], fill = demPal[2], alpha = 0.75, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EM1')  geom_sf(data = clips[[3]], fill = demPal[2], alpha = 0.9, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EM1')  geom_sf(data = clips[[4]], fill = demPal[3], alpha = 0.75, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EM1')  geom_sf(data = clips[[5]], fill = demPal[3], alpha = 0.9, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EM1')  geom_sf(data = clips[[6]], fill = demPal[4], alpha = 0.75, lwd = 0, color = NA)} +

    ## EM 2: 6 areas, 3 stocks
    {if(mapnames$Var1[i] == 'EM2') geom_sf(data = clips[[1]], fill = mgmtPal[1], alpha = 0.9, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EM2')  geom_sf(data = clips[[2]], fill = mgmtPal[1], alpha = 0.65, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EM2')  geom_sf(data = clips[[3]], fill = mgmtPal[2], alpha = 0.9, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EM2')  geom_sf(data = clips[[4]], fill = mgmtPal[2], alpha = 0.65, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EM2')  geom_sf(data = clips[[5]], fill = mgmtPal[3], alpha = 0.9, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EM2')  geom_sf(data = clips[[6]], fill = mgmtPal[3], alpha = 0.65, lwd = 0, color = NA)} +


    ## EM 4&5: 3 areas = 3 stocks
    {if(mapnames$Var1[i] == 'EMs 3&4') geom_sf(data = clips[[1]], fill = mgmtPal[1], alpha = 0.9, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EMs 3&4')  geom_sf(data = clips[[2]], fill = mgmtPal[1], alpha = 0.9, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EMs 3&4')  geom_sf(data = clips[[3]], fill = mgmtPal[2], alpha = 0.9, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EMs 3&4')  geom_sf(data = clips[[4]], fill = mgmtPal[2], alpha = 0.9, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EMs 3&4')  geom_sf(data = clips[[5]], fill = mgmtPal[3], alpha = 0.9, lwd = 0, color = NA)} +
    {if(mapnames$Var1[i] == 'EMs 3&4')  geom_sf(data = clips[[6]], fill = mgmtPal[3], alpha = 0.9, lwd = 0, color = NA)} +


    ## EM 6: panmixia
    {if(mapnames$Var1[i] == 'EM5') geom_sf(data = subset(regions, Region_Number == 1), fill = 'grey55', alpha = 0.9, lwd = 0.75, color = NA)} +
    {if(mapnames$Var1[i] == 'EM5') geom_sf(data = subset(regions, Region_Number == 2), fill = 'grey55', alpha = 0.9, lwd = 0.75, color = NA)} +
    {if(mapnames$Var1[i] == 'EM5') geom_sf(data = subset(regions, Region_Number == 3), fill = 'grey55', alpha = 0.9, lwd = 0.75, color = NA)} +

    ## mgmt fill
    # {if(mapnames$Var1[i] == 'EM2') geom_sf(data = subset(regions, Region_Number == 1),
    #                                        fill = mgmtPal[1], alpha = 0.9, lwd = 0.75, color = NA)} +
    # {if(mapnames$Var1[i] == 'EM2') geom_sf(data = subset(regions, Region_Number == 2),
    #                                        fill = mgmtPal[2], alpha = 0.9, lwd = 0.75, color = NA)} +
    # {if(mapnames$Var1[i] == 'EM2') geom_sf(data = subset(regions, Region_Number == 3),
    #                                        fill = mgmtPal[3], alpha = 0.9, lwd = 0.75, color = NA)} +
    # 
    # geom_text(aes( x=  165, y = 62), label = paste0(LETTERS[i],") "
    #                                                 # mapnames$Var1[i]
    #                                                 ), check_overlap = T) +
    ## all maps: mgmt boundaries with dashed lines
    geom_sf(data = subset(regions, Region_Number == 1), 
            fill = NA, 
            linewidth = 0.75,
            color = 'grey22', linetype = 'dotted') +
    geom_sf(data = subset(regions, Region_Number == 2),    
            fill = NA, 
            linewidth = 0.75,
            color = 'grey22', linetype = 'dotted') +
    geom_sf(data = subset(regions, Region_Number == 3),   
            fill = NA, 
            linewidth = 0.75,
            color = 'grey22', linetype = 'dotted') +
    {if(i ==1) theme_classic(base_size = 12)} +
    {if(i !=1) theme_void()}
  ggsave( plist[[i]] ,
         height = 4, width = 4, dpi = 520,
         file = here('figs','maps',paste0(i,"_",mapnames$Var1[i],".png")))
  
}

require(patchwork)
plist[[1]]  |
  plist[[2]] +theme_void() |
  plist[[3]] +theme_void()|
  plist[[4]]+theme_void()
ggsave(last_plot(),
       height = 10, width = 25, dpi = 520,
       file = here('figs','maps','EM_maps_combined.png'))

## OM strata labels
# geom_label(data = data.frame(), aes( x = c(238, 233, 232, 225, 220, 200),
#   y = c(33, 40, 49, 52, 57, 53)),
#   size = 5,
#   label = list(c(1:6),
#                c(paste0('Subarea ',c("C1", "C2", "B2","B3","A3", "A4"))),
#              c("C1", "C2", "B2","B3","A3", "A4"))[[3]] ,
#   fill = 'white',
#   color = rev(subareaPal)) +

## OM Stock labels
# geom_label(data = data.frame(), aes( x = c(238, 233, 224,  200),
#                                      y = c(33, 48,  54, 53)),
#            size = 5,
#            label = list(c(1:4),
#                         c(paste0('Stock ',c(1:4))),
#                       c("R1", "R2", "R3","R4"))[[2]] ,
#            fill = 'white',
#            color = rev(demPal)) +


  ## mgmt fills


  ## show demography from growth paper
  ##R3
# geom_sf(data =   st_union(x=clips[[2]],y=clips[[3]])  ,
# fill = NA, lwd = 1.1,color = 'white', linetype = 'dotted') +
# ##r2
# geom_sf(data =   st_union(x=clips[[4]],y=clips[[5]])  ,
# fill = NA, lwd = 1.1,  color = 'white', linetype = 'dotted') +

## panmictic fill
# geom_sf(data = clips[[1]], fill = 'grey22', alpha = 0.9, color = NA) +
#   geom_sf(data =  clips[[2]], fill = 'grey22', alpha = 0.9, color = NA) +
#   geom_sf(data = clips[[3]], fill = 'grey22', alpha = 0.9, color = NA) +
#   geom_sf(data = clips[[4]], fill = 'grey22', alpha = 0.9, color = NA ) +
#   geom_sf(data = clips[[5]], fill = 'grey22', alpha = 0.9, color = NA) +
#   geom_sf(data = clips[[6]], fill = 'grey22', alpha = 0.9,color = NA) +
  
  ## subarea fills
  # geom_sf(data = clips[[1]], fill = subareaPal[1], alpha = 0.9, color = NA) +
  # geom_sf(data =  clips[[2]], fill = subareaPal[2], alpha = 0.9, color = NA) +
  # geom_sf(data = clips[[3]], fill = subareaPal[3], alpha = 0.9, color = NA) +
  # geom_sf(data = clips[[4]], fill = subareaPal[4], alpha = 0.9, color = NA ) +
  # geom_sf(data = clips[[5]], fill = subareaPal[5], alpha = 0.9, color = NA) +
  # geom_sf(data = clips[[6]], fill = subareaPal[6], alpha = 0.9,color = NA) +
  
  ## stock fills
  # geom_sf(data = clips[[1]], fill = demPal[1], alpha = 0.9, color = NA) +
  # geom_sf(data =  clips[[2]], fill = demPal[2], alpha = 0.9, color = NA) +
  # geom_sf(data = clips[[3]], fill = demPal[2], alpha = 0.9, color = NA) +
  # geom_sf(data = clips[[4]], fill = demPal[3], alpha = 0.9, color = NA ) +
  # geom_sf(data = clips[[5]], fill = demPal[3], alpha = 0.9, color = NA) +
  # geom_sf(data = clips[[6]], fill = demPal[4], alpha = 0.9,color = NA) +
  

 

  
  ## show major currents
  # geom_polygon(data = shapefile_df,
  #              aes(x = long, y = lat, group = group, fill = id),
  #              fill = rep(currents.col,
  #                         length(shapefile_df$id)/5), size = 0.2) +
  # annotate("segment", x = x1, xend = x1end, y = y1, yend = y1,
  #          colour = currents.col[1:4], size = 1.1) +
  # annotate("text", x = x1+20, y = y1,
  #          label =  c('Alaskan Current',
  #                     'N. Pacific Current',
  #                     'S. California Bight',
  #                     'California Current')) +





# Save
ggsave(here::here("figs","maps", "map-EEZ_mgmt.png"), 
       width = 10, height = 8)


p1+labs(title = 'Management')

ggsave(p1+labs(title = 'Management') | p2+labs(title = 'Stocks (demography)')  |
         p3+labs(title = 'Modeled Subareas'),  
       dpi = 520,
       file = here("figs", "map-mgmt_strata_stock2.png"),
       width = 22, height = 18)



ggsave(last_plot(),
       file = "C:/Users/mkapur/Dropbox/mkapur.github.io/static/slides/kapur_genex/demog_dark3.png", 
       width = 10, height = 8)

## BC zoom map ----

p1 <- ggplot(data = regions) +
  geom_sf(data = B1, fill = "gold", alpha = 0.5, color = NA) +
  geom_sf(data = B2, fill= "blue", alpha = 0.5, color = NA) +
  geom_sf(lwd = 1, col = 'black', fill = NA) +
  coord_sf(xlim = c(220, 240), ylim = c(30, 65)) +
  labs(x ="",y="")+
  theme_classic(base_size = 14)
p2 <- autoplot(BCbath, geom=c("r", "c"), colour="white", size=0.05) + 
  scale_fill_etopo() +
  geom_abline(slope = ydif/xdif, intercept = int, col = 'red', lwd = 1.1) +
  
  labs(x ="",y="")+
  theme_classic(base_size = 14)

p1 |p2
# Save

ggsave(here::here("_writing","figures", "map-bc_bathy.png"), 
       width = 10, height = 8)
# 
# 
# usa <- map_data("world") 
# 
# load("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-01-23_nx=500_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2018/Data_Geostat.Rdata")
# 
# survLims <-  Data_Geostat %>% 
#   group_by(Region) %>% 
#   summarise(ymin = min(Lat),
#             xmin = min(Lon),
#             xmax = max(Lon))
# 
# Data_Geostat %>% filter(Region == 'BC') %>%
#   group_by(Survey) %>% 
#   summarise(lat_min = min(Lat),
#             lat_max = max(Lat), 
#             lon_min = min(Lon),
#             lon_max = max(Lon))
# 
# Data_Geostat %>% filter(Region == 'BC') %>%
#   # group_by(Survey) %>% 
#   filter(Lon > -126) 
#   summarise(round(max(Lon),10))
# 
# ## load polygons  
# load(paste0("./_writing/figures/spdf_fortified_BC.Rdata"))
# load(paste0("./_writing/figures/spdf_fortified_US.Rdata"))
# 
# ## clockwise from A1; two for A2 
# regLims <- data.frame(ymax = c(65,65,65,65,50,49,36),
#                       ymin = c(50,50,50,50,49,36,30), 
#                       xmax = c(-145,-132, -130, rep(-120,3),-115), 
#                       xmin = c(-180,-145, -132, rep(-130,4)) )
# 
# lukeregLims <- data.frame(ymax = c(65, 65, 65, 50.5, 49,  36),
#                       ymin = c(50.5, 50.5, 50.5, 49, 36, 30), 
#                       xmax = c(-147, -132 ,-115,  -115, -115, -115), 
#                       xmin = c(-180, -147, -132, -132, -132, -132 ))
# 
# mgmtLims <- data.frame(ymax = c(65, 65, 49),
#                       ymin = c(49, 49, 30), 
#                       xmax = c(-180, -115, -115), 
#                       xmin = c(-132, -132, -132))
# 
# demoLims <- data.frame(ymax = c(65,65,65,50,36),
#                        ymin = c(50,50,50,36,30), 
#                        xmin = c(-180,-145, -130,-130,-130), 
#                        xmax = c(-145,-130, -115, -115, -115))
# 
# lukeLims <- data.frame(ymax = c(65,65,65,50.5,36),
#                        ymin = c(50.5,50.5,49,36,30), 
#                        xmin = c(-180,-147, -132,-132,-132), 
#                        xmax = c(-147,-115, -115, -115, -115))
# 
# ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group),
#                         fill = 'grey22') +
#   # kaputils::theme_mk(base_size = 16) +
#   theme(axis.title =element_blank()) +
#   scale_x_continuous(expand = c(0,0), limits = c(-180,-110), breaks = seq(-180,-120,10), labels = paste(seq(-180,-120,10), "°W")) +
#   scale_y_continuous(expand = c(0,0), limits = c(30,75), breaks = seq(30,60,10),
#                      labels =  paste(seq(30,60,10), "°N"))  +
#   
#   ## CURRENT MGMT BOUNDARIES
#   # geom_polygon(data = spdf_fortified_US, aes( x = long, y = lat, group = group),
#   #              fill= 'red', color="red") +
#   # geom_polygon(data = spdf_fortified_BC, aes( x = long, y = lat, group = group),
#   #              fill= 'red', color="red")+
#   # geom_polygon(data = spdf_fortified_world, aes( x = long, y = lat, group = group),
#   #              fill= NA, color="red") +
#   geom_rect(data = mgmtLims, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
#   fill =NA, size = 1, colour = 'red') +
#   
#   # Complexity: actual survey boundaries
#   # geom_rect(data = survLims, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
#   # fill = NA , size = 1, colour = 'blue',linetype = 'dotted', alpha = 0.2) +
# 
#   
#   # DEMOGRAPHIC BOUNDARIES
#   geom_rect(data = demoLims, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
#   fill = NA , size = 1, colour = 'black',linetype = 'dashed', alpha = 0.2) +
# 
#   ## line to delete
#   # geom_vline(xintercept = -130, col = 'blue', lwd = 1.1) +
#   
#   ## lines to shift
#   geom_vline(xintercept = -145, col = 'green', lwd = 1.1) +
#   geom_hline(yintercept = 50, col = 'green', lwd = 1.1) +
#   
  ## LUKE'S PROPOSAL
  # geom_rect(data = lukeLims, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
  #           fill = NA , size = 1, colour = 'blue',linetype = 'dotted', alpha = 0.2) +
  

  
  ## USING GROWTH PAPER
  ## OM SUB AREAS
  # geom_rect(data = regLims, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
  #           fill = rev(cbbPalette[1:7]), size = 1, colour = NA, alpha = 0.3) +
  # geom_label(aes(
  #     x = c(rep(-125, 4), -131.5, -140, -155),
  #     y = c(33, 40, 49.5, rep(53, 4)),
  #     label = c("C1", "C2", "B1", "B2", "B3", "A2", "A1")
  #   ),
  #   size = 5,
  #   fill = cbbPalette[c(1:7)],
  #   color = c("grey88", rep('black', 3), 'grey88', rep('black',2))
  # ) +

## W Luke's Proposal
# geom_rect(data = lukeregLims, aes(xmin = xmin, ymin = ymin,
#                                   xmax = xmax, ymax = ymax),
#           fill = rev(cbbPalette[1:6]), size = 1, colour = NA, alpha = 0.3) +
#   geom_label(aes(
#     x = c(rep(-125, 4), -140, -155),
#     y = c(33, 40, 49.5, rep(53, 3)),
#     label = c("C1", "C2", "B1",  "B2", "A2", "A1")
#   ),
#   size = 5,
#   fill = cbbPalette[c(1:6)],
#   color = c("grey88", rep('black', 3), 'grey88', rep('black',1))
#   ) +

#   labs(x = "", y = "") +
#   coord_quickmap()  
# 
# ggsave(plot = last_plot(),
#        file = paste0("./docs/slides/img/luke3.png"), #paste0("./_writing/figures/Fig1_strata_mapsC.png"),
#        width = 10, height = 8, units = 'in', dpi = 720)


## Figure 2 panel of OM Indices (made by me with VAST) ----

## load tables_for_ss3.csv
OM1File <- "C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-01-23_nx=500_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2018/Table_for_SS3.csv"
vastc <- read.csv(OM1File) %>%
  mutate(TYPE = 'Abundance', Source = 'VAST',
         lci = Estimate_metric_tons-SD_mt,
         uci = Estimate_metric_tons+SD_mt) %>%
  select(Year, Fleet, Estimate_metric_tons, SD_log, TYPE, Source, uci, lci ) 
for(i in 1:nrow(vastc)){
  vastc$Fleet2[i] <- ifelse(vastc$Fleet[i] == "California_current",
                            "WC", 
                            ifelse(vastc$Fleet[i] == "British_Columbia", "BC",
                                   "AK"))
  if(vastc$Fleet[i] == 'AllAreas')   vastc$Fleet2[i] <- "ALL"
}
vastc <- vastc %>%
  filter(Fleet != 'Eastern_Bering_Sea' & Fleet != 'AllAreas') 

vastc$SubArea[vastc$Fleet == "British_Columbia"] <- "BC_VAST"
vastc$SubArea[vastc$Fleet == "California_current"] <- "CC_VAST"
vastc$SubArea[vastc$Fleet == "Gulf_of_Alaska"] <- "AK_VAST_2"
vastc$SubArea[vastc$Fleet == "Aleutian_Islands"] <- "AK_VAST_1"

## load bc nominal from brendan
# "0.317 which is for the nominal commercial trap index."
bcnom <- read.csv('./input/raw/om_indexSeries.csv') %>%
  mutate(SE = 0.317, lci = nominal.Trap.CPUE-1.96*SE, uci =nominal.Trap.CPUE+1.96*SE )

# genPal <- c('brown','dodgerblue','goldenrod','grey22')
p1 <- ggplot(vastc,   aes(x = Year, y = Estimate_metric_tons, col = SubArea)) +
  kaputils::theme_mk(base_size = 16) +
    theme(legend.position = c(0.75,0.75))+
  scale_color_manual(values = newPal) +
  scale_fill_manual(values = newPal) +
  labs(x = 'Year', y = 'Estimate (mt)', 
       fill = "Mgmt. Area", color = "Mgmt. Area") +
  geom_line(lwd = 0.9)+
  # geom_point(pch = 1, cex = 3) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci, fill = SubArea),
              alpha = 0.2,
              show.legend = TRUE)



p2 <- ggplot(bcnom, aes(x = YEAR, y = nominal.Trap.CPUE, 
                        col = "BC_NOM_CPUE" )) +
  kaputils::theme_mk(base_size = 16) +
  theme(legend.position = c(0.75,0.15))+
  labs(x = 'Year', y = 'Index', 
       fill = "Mgmt. Area", color = "Mgmt. Area") +
  geom_line(lwd = 0.9)+
  scale_x_continuous(limits = c(1979,2009)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci, fill = "BC_NOM_CPUE"),
              alpha = 0.2,
              show.legend = TRUE) +
  scale_fill_manual(values = 'dodgerblue') +
  scale_color_manual(values = 'dodgerblue') 
  
  ggsave(plot = p1| p2,
         file = paste0("./_writing/figures/Fig2_OM_indices.png"),
         width = 12, height = 7, units = 'in', dpi = 720)

p2 <- vastc %>%
  filter(Fleet == 'AllAreas') %>%
  ggplot(.,   aes(x = Year, y = Estimate_metric_tons, col = Fleet)) +
  kaputils::theme_mk(base_size = 16) + theme(legend.position = c(0.75,0.75)) +
scale_color_manual(values = cbbPalette) +
  scale_fill_manual(values = cbbPalette) +
  labs(x = 'Year', y = 'Estimate (mt)', title = paste0('OM2 PLACEHOLDER'), 
       fill = "", color = "") +
  geom_line(lwd = 0.9)+
  # geom_point(pch = 1, cex = 3) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci, fill = Fleet),
              alpha = 0.2,
              show.legend = FALSE)

## p3 is allareas from same run as p1
p3 <- vastc %>%
  filter(Fleet == 'AllAreas') %>%
  ggplot(.,   aes(x = Year, y = Estimate_metric_tons, col = Fleet)) +
  kaputils::theme_mk(base_size = 16) + theme(legend.position = c(0.75,0.75))+
  scale_color_manual(values = cbbPalette) +
  scale_fill_manual(values = cbbPalette) +
  labs(x = 'Year', y = 'Estimate (mt)', title = paste0('OM3 Panmictic Index'), 
fill = "", color = "") +
  geom_line(lwd = 0.9)+
  # geom_point(pch = 1, cex = 3) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci, fill = Fleet),
              alpha = 0.2,
              show.legend = FALSE)




ggsave(plot = (p1  | p2  | p3),
       file = paste0("./_writing/figures/Fig2_OM_indices.png"),
       width = 17, height = 10, units = 'in', dpi = 720)

## Figure X pseudo datplot
FleetNames <- c("WCGBTS", "Triennial", "StdTrap","StRs","AK_DOM_LL", "GOA_Trawl",
                "WC_HKL_POT","WC_TWL","BC_Trap","BC_Hook","BC_Trawl", "AK_LL_POT","AK_Trawl")

Fleets0 <- data.frame(FleetNames) %>% mutate(MR = NA)
Fleets0$MR[c(1:2,7:8)] <- "CC"
Fleets0$MR[c(3:4,9:11)] <- "BC"
Fleets0$MR[c(5:6,12:13)] <- "AK"
Fleets0$Type <- rep(NA, 13)
Fleets0$Type[1:6] <- "Survey"
Fleets0$Type[7:13] <- "Fishery"

Fleets <- rbind(Fleets0,Fleets0)
Fleets$Type[14:nrow(Fleets)] <- NA
## Add Age Comps
## CC: All fleets and have ages
Fleets$Type[Fleets$MR=='CC' & is.na(Fleets$Type)] <- "AgeComp"
## BC: Ages mostly from trap and both surveys
Fleets$Type[Fleets$MR=='BC' & is.na(Fleets$Type)] <- "AgeComp"
## AK: Ages from AKPOT and LL Surv
Fleets$Type[Fleets$MR =='AK' & is.na(Fleets$Type) & FleetNames %in% FleetNames[c(5,12)]] <- "AgeComp"
## Add LenComps
Fleets <- rbind(Fleets,Fleets0)
Fleets$Type[27:nrow(Fleets)] <- NA

## CC: from WCGBTS
Fleets$Type[Fleets$MR=='CC' & FleetNames == FleetNames[1] & is.na(Fleets$Type)] <- "LenComp"
## BC: commercial trawl
Fleets$Type[Fleets$MR=='BC' &  FleetNames == FleetNames[11] & is.na(Fleets$Type)] <- "LenComp"
## AK: fixed gear  and trawl fishery
Fleets$Type[Fleets$MR =='AK' & is.na(Fleets$Type) & FleetNames %in% FleetNames[c(12,13)]] <- "LenComp"

Fleets <- Fleets[!is.na(Fleets$Type),]

## basic plot (no years)
ggplot(Fleets, aes(x = FleetNames, y = Type, fill = Type)) +
  kaputils::theme_mk(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90), legend.position = 'none') +
  geom_tile(color = 'white') +
  scale_fill_manual(values = PNWColors::pnw_palette('Starfish')) +
  facet_wrap(.~MR, drop = TRUE, 
             scales = 'free_x') +
  labs(x = "Fleet Names", y = "Data Type")

ggsave(plot = last_plot(),
       file = paste0("./_writing/figures/Data_basic.png"),
       width = 10, height = 7, units = 'in', dpi = 720)
## expand grid into YEAR and drop rows if not applicable


dat <- expand.grid(Fleets,
            "Year" = 1970:2019 ) %>%
  mutate(FleetNames)
  mutate( LengthComp = NA,
          AgeComp = NA,
          CAAL = NA,
          Catch1  = NA,
          Catch2  = NA,
          Abund1 = NA,
          Abund2 = NA)


## Fill in survey years
dat$Abund1[dat$MR == 'CC' & dat$Year  %in% 1980:2004] <- FleetNames[2]
dat$Abund2[dat$MR == 'CC' & dat$Year >= 2003] <- FleetNames[1]

dat$Abund1[dat$MR == 'BC' & dat$Year %in% 1991:2009] <- FleetNames[3]
dat$Abund2[dat$MR == 'BC' & dat$Year  %in% 2003:2019] <- FleetNames[4]

dat$Abund1[dat$MR == 'AK' & dat$Year >= 1979 ] <- FleetNames[5]
dat$Abund2[dat$MR == 'AK' & dat$Year  >= 1980] <- FleetNames[6]


## Fill in Comps
dat$AgeComp[dat$MR == 'CC' & dat$Year  %in% 1980:2004] <- FleetNames[2]

dat$AgeComp[dat$MR == 'BC' & dat$Year %in% 1991:2009] <- FleetNames[3]

dat$AgeComp[dat$MR == 'AK' & dat$Year >= 1979 ] <- FleetNames[5]

 data.frame("Year" = 1960:2019,
                  LengthComp =NA,
                  AgeComp = NA,
                  CAAL = NA,
                  Catches = NA,
                  Abund = NA)


