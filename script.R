crimes <- read.csv("Crimes_-_2001_to_present.csv", stringsAsFactors =  FALSE)

str(crimes)

table(crimes$Primary.Type)

install.packages('qmap')

library(qmap)

install.packages("ggmap")

require(ggmap)

sfMap <- qmap("San Francisco", zoom = 12, color = "bw")

map<-get_map(location="sanfrancisco",zoom=12,source="osm")

chicagomap<-get_map(location="chicago",zoom=12,source="osm")

library(dplyr)
library(ggmap)
library(ggplot2)
library(readr)

install.packages("readr")

counts <- summarise(group_by(crimes, Primary.Type), Counts=length(Primary.Type))
counts <- counts[order(-counts$Counts),]

counts <- subset(counts, Primary.Type != 'OTHER OFFENSE')

top12<- counts[c(1:12),]

top12crimes <- crimes[crimes$Primary.Type %in% top12$Primary.Type,]

summary(top12crimes)

p <- ggmap(chicagomap) +
  geom_point(data=top12crimes, aes(x=Longitude, y=Latitude, color=factor(Primary.Type)), alpha=0.05) +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                               title="Type of Crime")) +
  scale_colour_brewer(type="qual",palette="Paired") + 
  ggtitle("Top 12 Crimes in Chicago ssince 2001") +
  theme_light(base_size=20) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
ggsave("chicago_top_crimes_map.png", p, width=14, height=10, units="in")


q <- ggmap(chicagomap) +
  geom_point(data=top12crimes, aes(x=Longitude, y=Latitude, color=factor(Arrest)), alpha=0.05) +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                               title="Arrest made")) +
  scale_colour_brewer(type="qual",palette="Set1") + 
  ggtitle("Arrests for Top 12 Crimes in Chicago since 2001") +
  theme_light(base_size=20) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("chicago_top_crimes_arrests_map.png", q, width=14, height=10, units="in")

?ggmap
