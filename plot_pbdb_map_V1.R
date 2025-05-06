
setwd("/Users/max/Desktop/Illustration")

library(ggplot2)

# to download the data. I'm not sure the following works.
# Replace "Tetrapoda" with your group of interest in the following line and paste it into a browser:
# https://paleobiodb.org/data1.2/occs/list.csv?interval=Ediacaran,Holocene&base_name=Tetrapoda&show=class,classext,genus,subgenus,abund,coll,coords,loc,paleoloc,strat,stratext,lith,env,ref,crmod,timebins,timecompare



# load the data
dat <- read.csv("pbdb_data_final.csv",  skip = 20, sep=";")

#mesozoic
dat2 <- subset(dat, as.numeric(max_ma)>65 & as.numeric(max_ma)<251)
#cenozoic
dat2 <- subset(dat, as.numeric(max_ma)<66)
#range(as.numeric(dat2$max_ma))

world<-map_data('world')

#orange #F15A29
#blue #3C96D2

#3.84, 6


ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region), # long, lat are needed here, even though it brings up an error
    fill = "lightgray", size = 0.1
  )  +
  geom_point(
    data = dat,
    aes(as.numeric(lng), as.numeric(lat)),
    alpha = .7, color = "#F15A29", size = 1, shape = 20
  ) +
  theme_void() +
  theme(legend.position = "none")




library(chronosphere) # also required: library(rgdal), library(GPlates

# to download the data
# https://paleobiodb.org/data1.2/occs/list.csv?interval=Ediacaran,Holocene&base_name=Serpentes&show=class,classext,genus,subgenus,abund,coll,coords,loc,paleoloc,strat,stratext,lith,env,ref,crmod,timebins,timecompare
# notes
# https://www.evolv-ed.net/post/divdyn-basic-corals/
# https://www.evolv-ed.net/project/chronosphere/

# download a map
ne <- fetch("NaturalEarth")

# this takes *ages* to run for some reason
plot(ne, col="grey",border=NA)
points(x = dat$lng, y = dat$lat, pch = 16, col = "red", cex = 0.5)

