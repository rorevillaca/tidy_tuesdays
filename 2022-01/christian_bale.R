library(ggplot2)
library(hrbrthemes)
library(ggtext)
library(dplyr)


datos <- ReviMisc::paste_from_excel()



datos %>% 
  mutate(label_x_pos=ifelse(Year%in%c(2012,2019),Year-0.25,ifelse(Year==2014,Year+0.5,Year))) %>% 
  ggplot(aes(x=Year,y=bmi))+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#F8C1C9")) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 30, fill = "#ADD6E4")) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 25, fill = "#ABECEC")) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 18.5, fill = "#FFEAAD")) +
  geom_hline(yintercept = seq(15,33,1),color="gray95")+
  geom_vline(xintercept = seq(1994,2019,1),color="gray95")+
  geom_line(color="white",size=4,alpha=0.4)+
  geom_point(size=5,aes(color=category))+
  geom_richtext(aes(x=label_x_pos,label=paste0("<span>**",Movie,"**</span><br>",Year,"<br>",source2_kg," kg"),y=ifelse(bmi>25.5,bmi+1.3,bmi-1.3)),family="IBM Plex Sans Bold",fill = NA, label.color = NA,size=4.4)+
  annotate("text", x = 2020.5, y = 16.25, label = "UNDERWEIGHT",angle=-90,family = "RobotoCondensed-Bold",color="#FFC410",fontface="bold",hjust=0.5)+
  annotate("text", x = 2020.5, y = 21.75, label = "NORMAL RANGE",angle=-90,family = "RobotoCondensed-Bold",color="#11CCC3",fontface="bold",hjust=0.5)+
  annotate("text", x = 2020.5, y = 27.5, label = "OVERWEIGHT",angle=-90,family = "RobotoCondensed-Bold",color="#1587AA",fontface="bold",hjust=0.5)+
  annotate("text", x = 2020.5, y = 32, label = "OBESE",angle=-90,family = "RobotoCondensed-Bold",color="#EB4C66",fontface="bold",hjust=0.5)+
  #scale_x_continuous(breaks=seq(1994,2019,1),labels = gsub('^(.{2})(.*)$', '\\1\n\\2', as.character(seq(1994,2019,1))))+
  scale_x_continuous(breaks=seq(1994,2019,1))+
  scale_y_continuous(limits=c(15,33),breaks=seq(15,33,1))+
  scale_color_manual(values=c("#11CCC3","#EB4C66","#1587AA","#FFC410"))+
  scale_fill_identity()+
  coord_cartesian(clip = "off",xlim = c(1994, 2019))+
  labs(title="Christian Bale's BMI Through the Years",
       caption="DATA: METAFLIX | VIZ: @ROREVILLACA",
       subtitle="From 1994 to 2019, the actor classified into the 4 weight-status groups according to the WHO",x="YEAR",y="BODY MASS INDEX (kg/m²)   ")+
  guides(color="none")+
  theme_ipsum_rc()+
  theme(axis.text.x = element_text(family = "RobotoCondensed-Bold", size = 12,face = "bold"),
        axis.text.y = element_text(family = "RobotoCondensed-Bold", size = 12,face = "bold"),
        axis.title.x = element_text(family = "RobotoCondensed-Bold", size = 12,face = "bold",color="gray50"),
        axis.title.y = element_text(family = "RobotoCondensed-Bold", size = 12,face = "bold",color="gray50"),
        plot.subtitle = element_text(size=16),
        plot.caption = element_text(family = "RobotoCondensed-Bold", size = 10,face = "bold",color="gray50"))
