##################################################
## Project: SRC Generation
## Script purpose: Generate Paper figures
## Date: 01-06-2020
## Author: Mike Johnson
##################################################

library(ggplot2)
library(ggridges)
library(AOI)
library(dplyr)
library(sf)

# Helpful functions -------------------------------------------------------

mapTheme <- function() {
  theme_void() + 
    theme(
      text = element_text(size = 7),
      plot.title = element_text(size = 14, color = "black", hjust = 0, vjust = 2, face = "bold"), 
      plot.subtitle = element_text(size = 8, color = "black", hjust = 0, vjust = 0),
      axis.ticks = element_blank(), 
      legend.direction = "vertical", 
      legend.position = "right",
      plot.margin = margin(1, 1, 1, 1, 'cm'),
      legend.key.height = unit(1.5, "cm"), legend.key.width = unit(0.4, "cm")
    ) 
}

rank = function(x){
  y = rep(NA, length(x))
  y[between(x, 0, 15)] = 1
  y[between(x, 15, 30)] = 2
  y[between(x, 30, 60)] = 3
  y[between(x, 60, 100)] = 4
  y[between(x, 100, 250)] = 5
  y[between(x, 250, 25e100000)] = 6
  y
}

prep_sf = function(input){
  usgs_filter %>% 
    merge(input, by = 'siteID') %>% 
    dplyr::select(siteID, lon, lat,COMID = COMID.x, nrmse = nrmse, order = order ) %>% 
    mutate(error = rank(nrmse)) %>% 
    st_as_sf(coords = c("lon", 'lat'), crs = '+proj=longlat +datum=WGS84')
}

# External Data (not checked in) ------------------------------------------

load('/Users/mikejohnson/Documents/GitHub/hand_improvements/data/usgs_filter.rda')
files  = list.files('/Users/mikejohnson/Documents/GitHub/hand_improvements/output/', full.names = T)
tmp    = list()

for(i in 1:length(files)){
  load(files[i])
  tmp[[i]] = o
}

raw = bind_rows(tmp) 

usgs_table = filter(raw, type == "table") %>% prep_sf()
usgs_lc    = filter(raw, type == "nlcd")  %>% prep_sf()
usgs_so    = filter(raw, type == "lm")    %>% prep_sf()
usgs_composite = filter(raw, type == "combo")     %>% prep_sf()
usgs_catchment = filter(raw, type == "catchment") %>% prep_sf()
usgs_base      = filter(raw, type == "base")      %>% prep_sf()

conus   = AOI::aoi_get(state = "conus") %>% st_transform(5070)
outline = sf::st_union(conus)

all = list(
  Composite = usgs_composite,
  `land cover` = usgs_lc,
  `Stream Order Composite` = usgs_table,
  Default = usgs_base,
  Catchment = usgs_catchment
)


# Figure 2 ----------------------------------------------------------------

to_plot = raw %>% 
  filter(type %in% c("base", "nlcd", "catchment", 'combo', "table")) %>% 
  mutate(type = factor(type,levels=levels(as.factor(type))[c(1,4,2,3,5)]))

out            = to_plot %>% group_by(type) %>% summarize(count = sum(nrmse > 250, na.rm = T))

png(file="/Users/mikejohnson/Documents/GitHub/SRCgeneration/imgs/methods_boxplot.png",height=12, width = 12, units = 'in', res=300)
  par(mar = c(5,6,3,3))
  boxplot(nrmse~type, data = to_plot,
        names = c("Default", "Land\nCover", "Catchment", "Composite", "Stream Order\nComposite"),frame.plot = FALSE,
        cex.axis = .7,
        outline = F,
        ylim = c(0,500),
        xlab = "Method", 
        pch =16, 
        cex =.2,
        ylab = "Normalized Root Mean Square Error \n(nRMSE)",
        main = paste0("SRC Comparision by Method: " , length(unique(to_plot$COMID)), " Stations"),      
        notch = T, 
        lwd = .4, 
        horizontal = F, 
        las = 1,
        col = c("#95D1C5", '#95D1C5',"#E3D5B8", '#E3D5B8',  '#95D1C5'),
        border = 'gray20')
  abline(h = 250)
  
  for(i in 1:5){ text(paste0('\u2191 ', out$count[i]), x = i, y = 260) }

  legend("topright", inset=.02, title="Method Type",
       c("Extendable","Non-extendable"), fill=c('#95D1C5',"#E3D5B8"), horiz=F, cex=1.2)

dev.off()

# Figure 3 ----------------------------------------------------------------

## PANEL A

for(i in 1:5){
  
  p   = st_transform(all[[i]], 5070) %>% filter(!is.na(error))
  lab = paste0(c("0-15", "15-30", "30-60", "60-100", "100-250", ">250"), " (", table(p$error),")" )
  
  g = ggplot() +
    geom_sf(data = conus, fill = 'black', color = "gray70", lwd = 3) +
    geom_sf(data = conus, fill = "black", color = "black") +
    geom_sf(data = conus, fill = 'black', color = "gray80", lwd = .1) +
    scale_colour_brewer("nRMSE (%)", palette = "RdYlGn", 
                        labels = lab, direction = -1) +
    scale_fill_brewer("nRMSE (%)", palette = "RdYlGn", 
                      labels = lab, direction = -1) +
    geom_sf(data = p, aes(fill = as.factor(error), color = as.factor(error)), 
            size = .05) + 
    labs(title = names(all)[i]) +
    mapTheme() 
  
  ggsave(g, filename = paste0("/Users/mikejohnson/Documents/GitHub/SRCgeneration/imgs/map_", names(all)[i], "_method.png"), width = 9, height = 6, dpi = 300)
}

## PANEL B
composite$nrmse[composite$nrmse < 250] %>% median(na.rm = T)


tab = usgs_composite %>% st_transform(5070)
mer = st_intersection(tab, conus) %>% 
  group_by(state_abbr) %>% 
  summarise(m = median(nrmse, na.rm = TRUE), n= n()) %>% 
  ungroup() %>% 
  st_drop_geometry()

tab2 = usgs_table %>% st_transform(5070)
mer2 = st_intersection(tab2, conus) %>% 
  group_by(state_abbr) %>% 
  summarise(m = median(nrmse, na.rm = TRUE), n= n()) %>% 
  ungroup() %>% 
  st_drop_geometry()

tab_sp2 = merge(conus, mer2)
sorted = tab_sp$state_abbr[order(tab_sp$m)]
vals = tab_sp$m[order(tab_sp$m)]

sta = list(sorted[1], sorted[25], sorted[48])
val = list(vals[1], vals[25], vals[48])
cond = paste(c("Best", "Average", "Worst"), "Performing:\n ")

subs = aoi_get(state = unlist(sta))

### PART 1

g1 = ggplot() +
  geom_sf(data = tab_sp, aes(fill = m/100), color = NA, lwd = .25) +  
  geom_sf(data = st_union(conus), 
          fill = NA, color = 'black', lwd = 1.1) +
  #geom_sf(data = subs, fill = NA,color = 'gray70', lwd = 3) +
  #geom_sf(data = subs, fill = NA,color = 'black', lwd = 1) +
  scale_fill_gradient2(position="bottom" , low = "blue", mid = scales::muted("blue"), high = "darkred", 
                       midpoint = .3) +
  mapTheme() +
  theme(legend.position = 'none')

g2 = ggplot() +
  geom_sf(data = tab_sp2, aes(fill = m/100), color = NA, lwd = .25) +  
  geom_sf(data = st_union(conus), 
          fill = NA, color = 'black', lwd = 1.1) +
  #geom_sf(data = subs, fill = NA,color = 'gray70', lwd = 3) +
  #geom_sf(data = subs, fill = NA,color = 'black', lwd = 1) +
  scale_fill_gradient2(position="bottom" , low = "blue", mid = scales::muted("blue"), high = "darkred", 
                       midpoint = .3) +
  mapTheme() 

library(patchwork)
g1+g2

ggsave(file = paste0("/Users/mikejohnson/Documents/GitHub/SRCgeneration/imgs//state_map_soc.png"), height = 12, width = 18, dpi = 300)

### PART 2

for(i in 1:length(sta)){
  
  fl = AOI::aoi_get(state = sta[[i]]) %>% st_transform(5070)
  flc = AOI::aoi_get(state = sta[[i]], county = 'all') %>% st_transform(5070)
  yy = tab[fl,]
  
  lab = paste0(c("0-15", "15-30", "30-60", "60-100", "100-250", ">250"), " (", table(yy$error),")" )
  
  ggplot() +
    geom_sf(data = fl, fill = 'black', color = "gray70", lwd = 5) +
    geom_sf(data = fl, fill = 'black', color = "black", lwd = 1) +
    geom_sf(data = flc, fill = 'transparent', color = "gray80", lwd = .05) +
    scale_colour_brewer("nRMSE (%)", palette = "RdYlGn", 
                        labels = lab, direction = -1) +
    scale_fill_brewer("nRMSE (%)", palette = "RdYlGn", labels = lab, direction = -1) +
    geom_sf(data = yy, aes(fill = as.factor(error),
                           color = as.factor(error)), 
            size = 1.8) + 
    mapTheme() +
    labs(caption= paste0(cond[i], round(val[[i]], 2), "% mean nRMSE (excluding outliers)")) + 
    theme(plot.caption = element_text(hjust=0.5, size=rel(3))) +
    theme(legend.position = 'none')
  
  ggsave(file = paste0("/Users/mikejohnson/Documents/GitHub/SRCgeneration/imgs/state_",sta[i], "_table_look.png"), height = 12, width = 12, dpi = 300)
}

# Figure 4 ----------------------------------------------------------------

## PART A
composite = filter(raw, type == "combo")
plots = list()
for(i in 1:10){
  
  test = composite[composite$order == i,]
  
  plots[[i]] = ggplot(data = test) + 
    geom_density(aes(x=inN), fill = 'red', alpha=0.4) +
    geom_density(aes(x=outN), fill = 'blue', alpha=0.4) +
    geom_vline(xintercept = mean(test$inN), col = 'red', lty = 2, lwd = 1) +
    geom_vline(xintercept = mean(test$outN), col = 'blue', lty = 2, lwd = 1) + 
    labs(title = paste0("Stream Order ", i),
         x = "N",
         y = "Density") + theme_light() +
    theme(plot.title = element_text(hjust=0.5, size=rel(2)))
}

fin = gridExtra::grid.arrange(plots[[1]], 
                              plots[[2]],
                              plots[[3]],
                              plots[[4]],
                              plots[[5]],
                              plots[[6]],
                              plots[[7]],
                              plots[[8]],
                              plots[[9]],
                              plots[[10]],
                              nrow = 2)

ggsave(plot = fin, file = "/Users/mikejohnson/Documents/GitHub/SRCgeneration/imgs/so_distribution_plots.png", height = 9, width = 20, dpi = 300)

## PART B

ridge_plot = function(d, col,method,bw = 4){
  ggplot(d, aes(x = nrmse, y = as.factor(order))) +
  geom_density_ridges(size = .25, fill = col, bandwidth = bw) +
  xlim(-10,250) +
  labs(title = paste0("nRMSE by Stream Order: \n", method, " method"),
       subtitle = (paste(sum(d$nrmse > 250, na.rm = T), "sites with nRMSE > 250%")),
       x = "nRMSE",
       y = "Stream Order") + ggridges::theme_ridges() + 
  geom_vline(xintercept=30) + 
  geom_vline(xintercept=60) + 
  geom_vline(xintercept=100)
}

g1 = ridge_plot(usgs_base,      "green",      "Default")
g2 = ridge_plot(usgs_composite, "red",       "Composite")
g3 = ridge_plot(usgs_table,     "lightblue", "SO-composite")
g4 = gridExtra::grid.arrange(g1,g2, g3, nrow = 1)

ggsave(plot = g4, filename = "/Users/mikejohnson/Documents/GitHub/SRCgeneration/imgs/ggridges.png", width = 12, height = 5, dpi = 300)




















tmp = usgs_so %>%  st_transform(5070) %>% filter(nrmse > 100)

cities = read_sf('/Users/mikejohnson/Downloads/tufts-uscitiestowns1mil14-shapefile/GISPORTAL_GISOWNER01_USCITIESTOWNS1MIL14.shp') %>% st_transform(5070)

city = cities[conus,] %>% filter(POP_2010 >25000)

ggplot() +
  geom_sf(data = st_union(conus), fill = NA, color = 'gray', lwd = 1.1) +
  geom_sf(data = city, color = 'red', size= .5) +
  geom_sf(data = tmp,  color = 'black', size= .5) +
  mapTheme() 



