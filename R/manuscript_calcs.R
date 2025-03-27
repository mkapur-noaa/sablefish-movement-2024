## one-off calculations for manuscript
## M Kapur Spr 2025
## maia.kapur@noaa.gov

## setup ----
load("~/projects/sab-move/data/tag_data.rda")
load("~/projects/sab-move/data/abundance.rda")
source("~/projects/sablefish-data-2024/R/plot.R", echo=TRUE)
library(dplyr)
library(akgfmaps)
library(ggplot2)
library(patchwork)

## save things from model run ----
abundance_exchange <-tar_read(abundance_exchange)
save(abundance_exchange, file = "ms/tabs/abundance_exchange.Rda")
abundance_exchange_block <-tar_read(abundance_exchange_block)
save(abundance_exchange_block, file = "ms/tabs/abundance_exchange_block.Rda")

percent_attributable <-tar_read(percent_attributable)
save(percent_attributable, file = "ms/tabs/percent_attributable.Rda")

percent_attributable_block <-tar_read(percent_attributable_block)
save(percent_attributable_block, file = "ms/tabs/percent_attributable.Rda")


mmmstan_regions_3_mean_3x_cv_fishing_rate <-tar_read(mmmstan_regions_3_mean_3x_cv_fishing_rate)
save(mmmstan_regions_3_mean_3x_cv_fishing_rate, file = "ms/tabs/mmmstan_regions_3_mean_3x_cv_fishing_rate.Rda")

mmmstan_regions_3_mean_3x_sd_reporting_rate <-tar_read(mmmstan_regions_3_mean_3x_sd_reporting_rate)
save(mmmstan_regions_3_mean_3x_sd_reporting_rate, file = "ms/tabs/mmmstan_regions_3_mean_3x_sd_reporting_rate.Rda")

mmmstan_regions_3_mean_block_1979_1994 <-tar_read(mmmstan_regions_3_mean_block_1979_1994)
save(mmmstan_regions_3_mean_block_1979_1994, file = "ms/tabs/mmmstan_regions_3_mean_block_1979_1994.Rda")

mmmstan_regions_3_mean_block_1995_2006 <-tar_read(mmmstan_regions_3_mean_block_1995_2006)
save(mmmstan_regions_3_mean_block_1995_2006, file = "ms/tabs/mmmstan_regions_3_mean_block_1995_2006.Rda")


mmmstan_regions_3_mean_block_2007_2017 <-tar_read(mmmstan_regions_3_mean_block_2007_2017)
save(mmmstan_regions_3_mean_block_2007_2017, file = "ms/tabs/mmmstan_regions_3_mean_block_2007_2017.Rda")

mmmstan_regions_3_size_no_duration_constraint <-tar_read(mmmstan_regions_3_size_no_duration_constraint)
save(mmmstan_regions_3_size_no_duration_constraint, file = "ms/tabs/mmmstan_regions_3_size_no_duration_constraint.Rda")

mmmstan_regions_3_size_no_recovery_transition <-tar_read(mmmstan_regions_3_size_no_recovery_transition)
save(mmmstan_regions_3_size_no_recovery_transition, file = "ms/tabs/mmmstan_regions_3_size_no_recovery_transition.Rda")

mmmstan_regions_3_size <-tar_read(mmmstan_regions_3_size)
save(mmmstan_regions_3_size, file = "ms/tabs/mmmstan_regions_3_size.Rda")

mmmstan_regions_6_mean <-tar_read(mmmstan_regions_6_mean)
save(mmmstan_regions_6_mean, file = "ms/tabs/mmmstan_regions_6_mean.Rda")

mmmstan_regions_3_mean <-tar_read(mmmstan_regions_3_mean)
save(mmmstan_regions_3_mean, file = "ms/tabs/mmmstan_regions_3_mean.Rda")

mmmstan_regions_8_mean <-tar_read(mmmstan_regions_8_mean)
save(mmmstan_regions_8_mean, file = "ms/tabs/mmmstan_regions_8_mean.Rda")

## Text for results ----

## confirm ess & rhat in base
sum(mmmstan_regions_3_mean$summary$movement_rate$rhat >1.01)
sum(mmmstan_regions_3_mean$summary$movement_rate$ess_bulk <400)
## mean distance in first 1-2 year
tag_data %>%
  filter(days_liberty <= 2*365 & days_liberty >= 365) %>%
  select(tag_distance) %>%
  summary()

## mean distance across all years
tag_data %>%
  select(tag_distance) %>%
  summary()

##  mean move in first and last x size
tag_data %>%
  filter(days_liberty <= 2*365 & days_liberty >= 365) %>%
  mutate(size_bins = cut(size_recovered, breaks = c(-Inf,400,549,Inf))) %>%
  summarise(mean(tag_distance),.by=size_bins)

## furthest great circle distance
tag_data %>% filter(!is.na(tag_distance)) %>% filter(tag_distance == max(tag_distance)) %>%
  select(date_released, size_released, date_released, size_recovered,region_released_3, region_recovered_3)
tag_data[tag_data$tag_distance==max(tag_data$tag_distance, na.rm = T),]
tag_data %>%
  mutate(size_bins = cut(size_recovered, breaks = c(-Inf,400,549,Inf))) %>%
  summarise(mean(tag_distance),.by=size_bins)

tag_data[which.max(tag_data$tag_distance),c('size_recovered','date_released','date_recovered','region_recovered_8','region_released_8','tag_distance')]



## FIGURES ----
#* Figure 1 ----
# Basic Maps
plot_map(regions = sf_regions_3,
         plot_name = 'map-regions-3',
         colname_regions_short = "region_short_3",
)
plot_map(regions = sf_regions_6,
         plot_name = 'map-regions-6',
         colname_regions_short = "region_short_6",
)

#* Figure 2 ----
## panel of map tags and size histograms
load("~/projects/sablefish-data-2024/data/tags_latlon_recovered.rda")
load("~/projects/sablefish-data-2024/data/tags_latlon_released.rda")

tags_latlon_released <- tags_latlon_released %>% mutate(
  year = lubridate::year(date_released), src = 'released',
  region = region_released_3)
tags_latlon_recovered <- tags_latlon_recovered %>% mutate(
  year = lubridate::year(date_recovered), src = 'recovered',
  region = region_recovered_3)

tag_dat <- bind_rows(tags_latlon_released,
                     tags_latlon_recovered) %>%
  mutate(region = case_when(region == '1' ~ 'Alaska',
                            region == '2' ~ 'British Columbia',
                            region == '3' ~ 'California Current'),
         src  = factor(src, levels = c('released', 'recovered')))

tag_n <- summarise(tag_dat, n = n(), .by = c(region, src)) %>%
  mutate(n_plot = prettyNum(n,big.mark = ','))







mappanel <- ggplot2::ggplot(data = sf_regions_3) +
  ggplot2::geom_sf() +
  ggplot2::geom_point(
    data = sample_n(tag_dat,1000),
    ggplot2::aes(
      x = longitude_released,
      y = latitude_released ,
      color = factor(year)),
    shape = 16,
    size = 1) +
  ggplot2::scale_color_viridis_d(
    option = "plasma",
    alpha = 0.4,
    begin = 0,
    end = 0.7) +
  geom_text(data = tag_n,
            aes(x = 180, y = 31,
                label = paste0("n = ", n_plot)))+
  facet_grid(region ~ src) +
  ggsidekick::theme_sleek() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 5),
        axis.title = element_blank())
# ggplot2::ggsave(
#   last_plot(),
#   file = here::here("ms","figs",  "tag_release_hist.png"),
#   width = 3,
#   dpi = 520,
#   height = 6)
hist <- ggplot( ) +
  geom_histogram(data = subset(tag_dat, src == 'released'), aes(x = size_released), fill = 'grey75', alpha = 0.5) +
  geom_histogram(data = subset(tag_dat, src == 'released' & size_released >= 400 & size_released <= 800), aes(x = size_released), fill = 'grey50') +
  geom_vline(xintercept = 550, linetype = 'dashed')+
  scale_x_continuous(limits = c(200,1000))+
  facet_grid(region~.)+
  ggsidekick::theme_sleek() + theme(axis.title.y = element_blank()) +
  labs(x = 'Sablefish Length at Release (mm)')



ggplot2::ggsave(
  mappanel | hist,
  file = here::here("ms","figs",  "figure2.png"),
  width = 10,
  dpi = 520,
  height = 8)


# ggplot2::ggsave(
#   last_plot(),
#   file = here::here("ms","figs",  "tag_release_recovered.png"),
#   width = 6,
# dpi = 520,
#   height = 8)

#* Figure 3 ----
## panel with networks and abundance exchange, saved separately and munged in .ppt
## these don't play nicely with patchwork, so make and save them separately and paste
#** Network maps ----
## reboot the network maps: include the 3 and 6 and 8 area models and tweak the arrows
load("~/projects/sab-move/data/sf_regions_3.rda")

plot_network(
  regions = sf_regions_3,
  rates = mmmstan_regions_3_mean$summary$movement_rate,
  plot_name = "map-regions-3-network",
  colname_regions_short = "region_short_3",
  size_short = 1.75,
  size_line = 0.25,
  size_text = 5,
  scale_edge_width_min = 0.2,
  scale_edge_width_max = 1.2,
  size_label = 3,
  strength = 4,
  color_land = "white",
  color_ocean = "grey96",
  color_region = "grey30",
  fill_land = "white",
  fill_ocean = "grey95",
  fill_region = "grey85",
  xmin = 169,
  ymin = 31,
  xmax = 240.5,
  ymax = 65.5,
  width = 90,
  height = 65,
  dpi = 400,
  file_type = '.png'
)

plot_network(
  regions = sf_regions_6,
  rates = mmmstan_regions_6_mean$summary$movement_rate,
  plot_name = "map-regions-6-network",
  colname_regions_short = "region_short_6",
  size_short = 1.75,
  size_line = 0.25,
  size_text = 5,
  scale_edge_width_min = 0.2,
  scale_edge_width_max = 1.2,
  hjust_label = c(0.5, 0.3, 1.1, -0.1, 1.5, -0.75, 1.15, -0.45, 1.5, 0.05),
  vjust_label = c(1.5, -0.55, 1.4, -2.3, 1.2, -0.15, 0, 1.7, 0, -0.4),
  size_label = 2.5,
  strength = 2,
  color_land = "white",
  color_ocean = "grey96",
  color_region = "grey30",
  fill_land = "white",
  fill_ocean = "grey95",
  fill_region = "grey85",
  xmin = 169,
  ymin = 31,
  xmax = 240.5,
  ymax = 65.5,
  width = 90,
  height = 65,
  dpi = 400,
  file_type = '.png'
)

#** abundance ----
# percent attributable
plot_abundance(
  data = percent_attributable,
  plot_name = "bar-percent-attributable",
  y_axis_label = "Abundance proportion attributable to movement",
  toptop = "bcak",
  topbottom = "akbc",
  bottomtop = "ccbc",
  bottombottom = "bccc",
  toptop_annotation = "AK",
  topbottom_annotation = "BC",
  bottomtop_annotation = "BC",
  bottombottom_annotation = "CC",
  linewidth_hline = 0.5,
  linewidth_error = 0.5,
  x_limits = c(1978, 2020),
  y_limits_top = c(-0.4, 0.2),
  y_limits_bottom = c(-0.05, 0.4),
  y_limits_top_breaks = round(seq(-0.4, 0.2, 0.2), 1),
  y_limits_bottom_breaks = round(seq(-0.05, 0.4, 0.2), 1),
  relative_panel_height = c(50, 50),
  x_annotation = 2019,
  y_annotation = 0.05,
  width = 190,
  height = 140,
  dpi = 520,
  file_type = '.png'
)

#abundance exchange
plot_abundance(
  data = abundance_exchange,
  plot_name = "bar-abundance-exchange",
  x_axis_label = "Year",
  y_axis_label = "Abundance exchange (millions)",
  toptop = "bcak",
  topbottom = "akbc",
  bottomtop = "ccbc",
  bottombottom = "bccc",
  toptop_annotation = "AK",
  topbottom_annotation = "BC",
  bottomtop_annotation = "BC",
  bottombottom_annotation = "CC",
  linewidth_hline = 0.5,
  linewidth_error = 0.5,
  x_limits = c(1978, 2020),
  y_limits_top = c(-30, 20),
  y_limits_bottom = c(-5, 20),
  y_limits_top_breaks = round(seq(-30, 20, 10), 0),
  y_limits_bottom_breaks = round(seq(-5, 20, 5), 0),
  relative_panel_height = c(50, 50),
  x_annotation = 2019,
  y_annotation = 4,
  width = 190,
  height = 140,
  dpi = 520,
  file_type = '.png'
)

## SUPPLEMENTARY FIGS ----
#* Figure S1 ----
## release and recovery thru time histogram
tag_data2 <- tag_data %>%
  select(date_recovered, region_recovered_3) %>%
  filter(complete.cases(.)) %>%
  mutate(year = lubridate::year(date_recovered) ,
         region = case_when(region_recovered_3  == '1' ~ 'Alaska',
                            region_recovered_3  == '2' ~ 'British Columbia',
                            region_recovered_3  == '3' ~ 'California Current')) %>%
  summarise(n = n(), .by = c(year, region)) %>%
    mutate(src = 'recovered') %>%
  bind_rows(.,tag_data %>%
              select(date_released, region_released_3) %>%
              filter(complete.cases(.)) %>%
              mutate(year = lubridate::year(date_released) ,
                     region = case_when(region_released_3  == '1' ~ 'Alaska',
                                        region_released_3  == '2' ~ 'British Columbia',
                                        region_released_3  == '3' ~ 'California Current')) %>%
              summarise(n = n(), .by = c(year, region)) %>%
              mutate(src = 'released')) %>%
  mutate(src  = factor(src, levels = c('released', 'recovered')))

ggplot(tag_data2, aes(x = year, y = n)) +
  geom_bar(stat = 'identity') +
  facet_grid(region ~ src, scales = 'free_y') +
  labs(x = 'Year', y = 'number of tags')+
  ggsidekick::theme_sleek()+  theme(strip.text = element_text(size = 12),
                                    axis.text = element_text(size = 10),
                                    axis.title  = element_text(size = 12))

ggplot2::ggsave(
  last_plot(),
  file = here::here("ms","figs",  "Figure_S1_tagsxtime.png"),
  width = 8,
  dpi = 520,
  height = 10)

#* Figure S2 ----
# sensitivity TAL and transision move rate bars
# showing with size and base comparison
senslist <- list(mmmstan_regions_3_size$summary$movement_rate,
               mmmstan_regions_3_size_no_duration_constraint$summary$movement_rate,
               mmmstan_regions_3_size_no_recovery_transition$summary$movement_rate)

sensmov <- bind_rows(senslist ) %>%
  mutate(x = case_when(x  == '1' ~ 'Alaska',
                       x  == '2' ~ 'British Columbia',
                       x  == '3' ~ 'California Current'),
         y = case_when(y  == '1' ~ 'Alaska',
                       y  == '2' ~ 'British Columbia',
                       y  == '3' ~ 'California Current'),
         blk = c(rep("base model",18),
                 rep("No TAL Constraint",18),
                 rep("No Size Transition",18)),
         l = factor(case_when(l == 1 ~ ' small',
                              l == 2 ~ ' large'))) %>%
  mutate(blk_inter = paste0(blk, l)) %>%
  mutate(blk_inter = factor(blk_inter, levels = unique(blk_inter)))


ggplot(sensmov,aes(x = blk_inter, group = blk_inter,
                   fill = blk_inter)) +
  geom_bar(aes( y = mean),stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(ymin = q5, ymax = q95),  width = 0,
                position = position_dodge(0.9)) +
  facet_grid(x~y ) +
  labs(x = 'time block', y= 'mean movement rate')+
  scale_fill_manual(values =  c("grey50","grey70" ,
                                "#2c6184", "#1f455e",
                                "#ba7999" ,"#984e73"))+
  ggsidekick::theme_sleek()+
  theme(legend.position = 'none')+
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 10, angle = 90),
        axis.title  = element_text(size = 12))

ggplot2::ggsave(
  last_plot(),
  file = here::here("ms","figs",  "Figure_S2_tal_sz_sens.png"),
  width = 10,
  dpi = 520,
  height = 10)

#* Figure S3 ----
## senstivity time block move rate bars
## not showing size here because it's too much
blklist = list(mmmstan_regions_3_mean$summary$movement_rate,
               mmmstan_regions_3_mean_block_1979_1994$summary$movement_rate,
               mmmstan_regions_3_mean_block_1995_2006$summary$movement_rate,
               mmmstan_regions_3_mean_block_2007_2017$summary$movement_rate)



blkmov <- bind_rows(blklist ) %>%
  mutate(x = case_when(x  == '1' ~ 'Alaska',
                                    x  == '2' ~ 'British Columbia',
                                    x  == '3' ~ 'California Current'),
                 y = case_when(y  == '1' ~ 'Alaska',
                               y  == '2' ~ 'British Columbia',
                               y  == '3' ~ 'California Current'),
         blk = c(rep("base model",9),
                 rep("1979-1994",9),
                 rep("1995-2006",9),
                 rep("2007-2017",9))) %>%
  mutate(blk  = factor(blk, levels = c('base model',
                                       '1979-1994',
                                       '1995-2006',
                                       '2007-2017')))

ggplot(blkmov,aes(x = blk, fill = blk)) +
  geom_bar(aes( y = mean),stat = 'identity') +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0, color = 'grey20') +
  facet_grid(x~y ) +
  labs(x = 'time block', y= 'mean movement rate')+
  scale_fill_manual(values = c("grey50","#015b58", "#2c6184", "#ba7999"))+
  ggsidekick::theme_sleek()+
  theme(legend.position = 'none')+theme(strip.text = element_text(size = 12),
                                       axis.text = element_text(size = 10, angle = 90),
                                       axis.title  = element_text(size = 12))

ggplot2::ggsave(
  last_plot(),
  file = here::here("ms","figs",  "Figure_S3_timeblksens.png"),
  width = 10,
  dpi = 520,
  height = 10)

#* Figure S4 ----
## senstivity prior cv and sd move rate bars
## not showing size here because it's too much
x3list = list(mmmstan_regions_3_mean$summary$movement_rate,
               mmmstan_regions_3_mean_3x_cv_fishing_rate$summary$movement_rate,
               mmmstan_regions_3_mean_3x_sd_reporting_rate$summary$movement_rate)



x3mov <- bind_rows(x3list ) %>%
  mutate(x = case_when(x  == '1' ~ 'Alaska',
                       x  == '2' ~ 'British Columbia',
                       x  == '3' ~ 'California Current'),
         y = case_when(y  == '1' ~ 'Alaska',
                       y  == '2' ~ 'British Columbia',
                       y  == '3' ~ 'California Current'),
         blk = c(rep("base model",9),
                 rep("Fishing rate prior CV x3",9),
                 rep("Reporting Rate prior CV x3",9)))

ggplot(x3mov,aes(x = blk, fill = blk)) +
  geom_bar(aes( y = mean),stat = 'identity') +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0, color = 'grey20') +
  facet_grid(x~y ) +
  labs(x = 'time block', y= 'mean movement rate')+
  scale_fill_manual(values = c("grey50","#015b58", "#2c6184", "#ba7999"))+
  ggsidekick::theme_sleek()+
  theme(legend.position = 'none')+theme(strip.text = element_text(size = 12),
                                        axis.text = element_text(size = 10, angle = 90),
                                        axis.title  = element_text(size = 12))

ggplot2::ggsave(
  last_plot(),
  file = here::here("ms","figs",  "Figure_S4_cvsdsens.png"),
  width = 10,
  dpi = 520,
  height = 10)
## bar regions 6 size ----
list_regions_6=list(wak = 1, eak = 2, nbc = 3, sbc = 4, ncc = 5, scc = 6)
plot_cols(
  data = mmmstan_regions_6_size$summary$movement_rate,
  plot_name = "bar-regions-6-size",
  regions = toupper(names(list_regions_3)),
  xvar = "l",
  xlab = "Length class",
  ylab = "Annual movement rate",
  x_text = c("Small", "Large"),
  x_breaks = 1:2,
  y_text = as.character(seq(0, 1, 0.25)),
  y_breaks = seq(0, 1, 0.25),
  x_angle = 0,
  hjust = 0.5,
  vjust = 0.5,
  size_title = 8,
  size_strip = 8,
  size_text = 6,
  size_error = 0.3,
  panel_spacing = 1,
  xmin = NA,
  xmax = NA,
  ymin = 0.0,
  ymax = 1.0,
  width = 190,
  height = 170,
  dpi = 520,
  file_type = '.png'
)


## DEP ----

## 8 area
plot_network(
  regions = sf_regions_8,
  rates = mmmstan_regions_8_mean$summary$movement_rate,
  plot_name = "map-regions-8-network",
  colname_regions_short = "region_short_8",
  size_short = 1.75,
  size_line = 0.25,
  size_text = 5,
  scale_edge_width_min = 0.2,
  scale_edge_width_max = 1.2,
  hjust_label = c(1,-0.6,1,0,-0.75,0.5,0,0.4,0.3,0,1, -0.2,1.2,0),
  vjust_label = c(0,-1.2,1.5,1.5,0,-0.5,1.2,-1,1,0,0,0,0,0),
  size_label = 2,
  strength = 2,
  color_land = "white",
  color_ocean = "grey96",
  color_region = "grey30",
  fill_land = "white",
  fill_ocean = "grey95",
  fill_region = "grey85",
  xmin = 169,
  ymin = 31,
  xmax = 240.5,
  ymax = 65.5,
  width = 90,
  height = 65,
  dpi = 400,
  file_type = '.png'
)
