library(ggplot2)
library(magrittr)
library(tidyverse)
library(tidytuesdayR)
library(rnaturalearth)
library(sf)
library(ggtext)
library(cowplot)
library(extrafont)


water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')
water %<>% mutate(country_name=case_when(country_name=="Congo - Brazzaville"~"Democratic Republic of the Congo",
                                         country_name=="Congo - Kinshasa"~"Democratic Republic of the Congo",
                                         country_name=="Gambia"~"The Gambia",
                                         TRUE ~ country_name))
points_per_country<-water %>% count(country_name)


africa <- ne_countries(scale = "medium", returnclass = "sf", continent = "Africa")


africa  %<>% left_join(points_per_country, by = c("name_long" = "country_name"))

g3<-ggplot(africa)+
  geom_sf(aes(fill=n),colour="white")+
  annotate("richtext", x = 26, y = 2.6, label = "**Uganda**",fontface="bold",fill = NA, label.color = NA,color="white")+
  annotate("richtext", x = 8, y = 9.3, label = "**Nigeria**",fontface="bold",fill = NA, label.color = NA,color="white")+
  annotate("richtext", x = -12, y = 7.3, label = "**Sierra**<br>**Leone**",fontface="bold",fill = NA, label.color = NA,color="black",hjust=1,vjust=1)+
  annotate("segment", x = 29.3, xend = 32.5, y = 2.6, yend = 1.6,colour = "white")+
  annotate("segment", x = -14, xend = -12, y = 7.5, yend = 8.5,colour = "white")+
  scale_fill_continuous(low="#7BC0D6", high="#2E7392", guide="colorbar",na.value="gray80",labels = c("30k","60k","90k"),breaks=c(30000,60000,90000))+
  theme_void()+
  labs(fill="Number of Water Points")+
  theme(legend.position = c(0.23,0.35))+
  theme(plot.background = element_blank())+
  guides(fill = guide_colorbar(title.position = "top",
                               direction = "horizontal",
                               title.hjust = .5, 
                               barwidth = unit(10, "lines"), 
                               barheight = unit(.5, "lines")))+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))


lastdigit <- function(x) x%%10
year_colors <- c(rep(c(rep("gray70",10),rep("#1F66A8",10)),3),"gray70")

g1 <- water %>% filter(country_name%in%africa$name_long) %>%  count(install_year)  %>% filter(between(install_year,1950,2021)) %>% 
  ggplot()+
  geom_segment(aes(x=1945,y=0,xend=2023,yend=0),size = 8,color="#1F66A8")+
  geom_segment(aes(x=1948,y=-600,xend=1948,yend=600),size = 6,color="#1F66A8")+
  geom_segment(aes(x=2023,y=-600,xend=2023,yend=600),size = 6,color="#1F66A8")+
  geom_point(aes(x=install_year,y=0),color="gray20",size=5)+
  geom_segment(aes(x=install_year,y=0,xend=install_year,yend=n),size = 3, lineend = 'round',color="#00ADEF")+
  labs(x="INSTALL YEAR\n\n",y="NO. OF WATER POINTS\n")+
  geom_text(x = 1955,y=1750,label="195_",color="gray70")+
  geom_text(x = 1965,y=1750,label="196_",color="#1F66A8")+
  geom_text(x = 1975,y=1750,label="197_",color="gray70")+
  geom_text(x = 1985,y=1750,label="198_",color="#1F66A8")+
  geom_text(x = 1995,y=1750,label="199_",color="gray70")+
  geom_text(x = 2005,y=1750,label="200_",color="#1F66A8")+
  geom_text(x = 2015,y=1750,label="201_",color="gray70")+
  geom_text(x = 2025,y=1750,label="202_",color="#1F66A8")+
  geom_text(x = 1946,y=6000,label="WHERE IS THE WATER?",color="#1F66A8",hjust=0,size=21.5,fontface="bold",family="Bahnschrift")+
  geom_richtext(x = 2029,y=6000,label="LOCATION AND INSTALL YEAR FOR<br>ALL WATER SOURCES ACROSS <span style='color:#00ADEF'>AFRICA</span>",color="gray50",hjust=1,size=8,fontface="bold",fill = NA, label.color = NA)+
  scale_x_continuous(limits=c(1945,2025),expand=c(0,0),position = "top",labels=lastdigit,breaks=seq(1950,2021,1))+
  scale_y_reverse(limits=c(21000,-700),expand=c(0,0),position = "right",labels=scales::number_format(big.mark =","))+
  coord_cartesian(clip = "off")+
  theme(panel.background = element_blank(),panel.grid.major=element_line(color="gray90"),panel.grid.minor=element_line(color="white"))+
  theme(plot.background = element_rect(fill="#E4F6FA",color="white"))+
  theme(axis.text.x = element_text(colour = year_colors,face="bold"))+
  theme(axis.title.x = element_text(margin = margin(t = 100, r = 0, b = 100, l = 0)))+
  theme(axis.title.x = element_text(hjust = 0, colour="darkgrey",face="bold"))+
  theme(axis.title.y = element_text(hjust = 1, colour="darkgrey",face="bold"))+
  theme(plot.margin = unit(c(4,0.5,1.5,0.5), "cm"))


g2 <- ggdraw(g1) + draw_plot({g3},x = 0, y = 0,width = 0.56, height = 0.7)+
  geom_text(aes(x=0.985,y=0.03,label="Data: Water Point Data Exchange | Rodrigo Revilla"),hjust=1,color="gray60")

save_plot(filename="africa_water.png",plot=g2,base_width = 16,base_height = 9)




