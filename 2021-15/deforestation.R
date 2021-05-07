library(tidyverse)
library(magrittr)
library(here)
library(janitor)
library(hrbrthemes)
library(ggallin)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2021, week = 15)
forest <- tuesdata$forest
forest_area <- read.csv2(here("2021-15","share-global-forest.csv"),sep=",") %>% clean_names() %>% filter(year==2015,!is.na(code),code!="",entity!="World") %>% mutate(share_of_global_forest_area=as.numeric(share_of_global_forest_area)) 
forest %<>% mutate(text_color=ifelse(net_forest_conversion<0,"#AB0730",ifelse(net_forest_conversion==0,"#F6B71A","#7D8F29"))) 
forest %<>% left_join(forest_area,by=c("entity","code","year")) %>%  filter(year==2015,!is.na(code),entity!="World") 

myPalette <- forest$text_color
names(myPalette) <- forest$entity


forest %>%  ggplot(aes(y=reorder(entity,-net_forest_conversion),x=net_forest_conversion/1000000))+
  geom_point(aes(size=share_of_global_forest_area/100,color=ifelse(net_forest_conversion<0,"#AB0730",ifelse(net_forest_conversion==0,"#F6B71A","#7D8F29"))),alpha=0.65)+
  scale_color_identity(guide=F)+theme_minimal()+
  scale_size(labels=scales::percent)+
  scale_x_continuous(labels = scales::dollar_format(suffix = " M ha",prefix = ""))+
  geom_curve(aes(x = 1.8, y = 10, xend = 1.93, yend = 2),arrow = arrow(length = unit(0.01, "npc")),curvature = -0.2) +
  geom_richtext(aes(label="China accounts for **5.14%** of the global forest area,<br>and in '15-'19 contributed with **1.9 M** net hectares",hjust=1,vjust=0,x=1.8,y=10),fill = NA, label.color = NA,size=3.2)+
  geom_curve(aes(x = -1.46, y = 117, xend = -1.34, yend = 109),arrow = arrow(length = unit(0.01, "npc")),curvature = 0.2) +
  geom_richtext(aes(label="Brazil owns the highest share of forest area **(12.3%)**,<br>and lost more than **1.4 M** hectares to deforestation",hjust=1,vjust=0,x=1.8,y=10),fill = NA, label.color = NA,size=3.2)+
  geom_curve(aes(x = 0.05, y = 65.3, xend = 0.05, yend = 40.7),curvature = 0) +
  labs(x="NET FOREST CONVERSION",y="COUNTRY",caption = "M ha: Million hectares",size="Share of global forest area")+
  labs(title="Between 2010 and 2015, 5.1 million hectares of forest were lost worldwide")+
  labs(subtitle="<span style = 'font-size:13.5pt'><span style = 'color:#AB0730'>**53 countries were responsible for this loss,**</span> **whereas 65 countries had either a** <span style = 'color:#F6B71A'>**neutral**</span> **or a**<span style = 'color:#7D8F29'> **positive**</span> **impact**</span>")+
  theme(plot.subtitle = ggtext::element_textbox_simple(hjust = 1))+
  theme(legend.position="top",axis.text.y = element_text(color=myPalette[forest %>% arrange(desc(net_forest_conversion)) %>% pull(entity)]))+
  theme(text=element_text(family = "Roboto Condensed"),plot.title = element_text(size=20,face="bold"))+
  theme(axis.title = element_text(hjust = 0.5,colour = "darkgrey",face="bold"))+
  guides(size=guide_legend(override.aes=list(colour="steelblue")))

ggsave(here("2021-15","deforestation.png"),width = 10,height = 15)

