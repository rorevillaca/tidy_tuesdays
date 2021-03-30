library(tidytuesdayR)
library(tidyverse)
library(magrittr)
library(ggimage)
library(here)
library(cowplot)

# Load Data -------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 13)
unvotes <- tuesdata$unvotes
roll_cals <- tuesdata$roll_calls
issues <- tuesdata$issues

# Calculate Score -------------------------------------------------------------------
country_votes <- unvotes %>% left_join(issues,by="rcid") %>% drop_na() %>% mutate(score=ifelse(vote=="yes",1,ifelse(vote=="no",-1,0))) %>% select(country_code,short_name,score) 
country_votes %<>% group_by(country_code) %>% mutate(n=n()) %>% ungroup() %>% group_by(country_code,short_name) %>% summarise(n=mean(n),avg_score = mean(score)) %>% ungroup()


a <- country_votes %>% slice_max(n,n=240) %>% mutate(COUNTRY="COUNTRY") %>% ggplot(aes(x=0,y=reorder(country_code,n)))+
  geom_image(aes(image = here("2021-13","flags", paste0(country_code, ".png"))),asp=0.20,size=0.18)+
  scale_x_continuous(expand=c(0,0))+
  theme_void()+
  theme(axis.text.y = element_text(size=10,face="bold"))+
  theme(plot.margin = unit(c(0,0,0,0.5), "cm"),panel.border = element_blank())

b <- country_votes %>% slice_max(n,n=240) %>% ggplot(aes(x=avg_score,y=reorder(country_code,n)))+
  geom_tile(aes(x=0),height=0.2,width=2,fill="gray90")+
  geom_bar(stat="identity",aes(fill=ifelse(avg_score>0,"#5B92E5","#FFB6B3")),width = 0.5)+
  scale_x_continuous(limits=c(-1,1))+
  facet_wrap(~short_name,nrow = 1)+
  scale_fill_identity()+
  #theme_void()+
  theme(axis.text.y = element_blank(),axis.title.y = element_blank(),axis.ticks.y = element_blank(),  strip.background = element_blank(),strip.text.x = element_blank())+
  theme(panel.background = element_blank())+
  theme(plot.margin = unit(c(5,0.5,1,0), "cm"))+
  labs(x="Average Score")+
  theme(axis.title.x = element_text(color="gray",size=11,hjust = 0,face = "bold"))

c <- plot_grid(a,b,rel_widths = c(1,9),nrow = 1,align = "h", axis = "bt")
c <- ggdraw(c) + draw_image(here("2021-13","icons",'co.png'),scale=0.04,x=-0.328,y=0.37)
c <- ggdraw(c) + draw_image(here("2021-13","icons",'di.png'),scale=0.04,x=-0.178,y=0.37)
c <- ggdraw(c) + draw_image(here("2021-13","icons",'ec.png'),scale=0.04,x=-0.028,y=0.37)
c <- ggdraw(c) + draw_image(here("2021-13","icons",'hr.png'),scale=0.04,x=0.122,y=0.37)
c <- ggdraw(c) + draw_image(here("2021-13","icons",'me.png'),scale=0.04,x=0.268,y=0.37)
c <- ggdraw(c) + draw_image(here("2021-13","icons",'nu.png'),scale=0.04,x=0.418,y=0.37)

issue_labels <- c("Colonialism","Arms Control and Disarmament","Economic Development","Human Rights","Palestinian Conflict","Nuclear Weapons and Material")
c <- c + draw_text(text=issue_labels,seq(0.175,0.915,0.148),y=0.83,size=12)
c <- c + draw_text(text="UN General Assembly Votes per Country and Issue",x=0.01,y=.96, size=20, hjust = 0,fontface = "bold",family="Roboto Condensed")
c <- c + draw_text(text="Each bar represents the vote average, where -1 corresponds to all-negative votes and +1 to all-positive ones. 40 countries with the most votes across all issues presented.",x=0.01,y=.93, size=17, hjust = 0,family="Roboto Condensed",color="gray50")
c <- c + draw_text(text="Data: Harvard Dataverse | Rodrigo Revilla",x=0.99,y=.03, size=10, hjust = 1,family="Roboto Condensed",color="gray50")

save_plot("./2021-13/un_votes.png",c,base_width=16,base_height=10)
