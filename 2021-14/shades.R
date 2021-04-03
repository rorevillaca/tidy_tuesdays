library(tidyverse)
library(tidytext)
library(waffle)
library(extrafont)
library(cowplot)
library(magrittr)

extrafont::font_import(paths = "C:/Users/Revi/Appdata/Local/Microsoft/Windows/Fonts/")
loadfonts(device = "win")

tuesdata <- tidytuesdayR::tt_load(2021, week = 14)
shades <- tuesdata $allShades


common_words_25 <- shades %>% drop_na(name) %>%  unnest_tokens("word", name, token = "ngrams", n = 1) %>% select(word) %>% group_by(word) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head(25) %>% pull(word)
 
common_words_25 <- shades %>%  unnest_tokens("word", name, token = "ngrams", n = 1) %>% filter(word %in% common_words_25)
 

common_words_25 %>% select(word,hex,lightness) %>% group_by(word) %>% mutate(n=n_distinct(hex)) %>% arrange(desc(n))

common_words_25 %<>% group_by(word) %>% mutate(repeticiones=n()) %>% ungroup() %>% mutate(no_filas=ceiling(repeticiones/60)) %>% arrange(desc(repeticiones))

title <- ggdraw() +
  draw_label(x = 0, "Colors associated with the most common words found in make up shade names", hjust = 0, fontfamily = "Oswald Medium", fontface = "bold", size = 15) +
  theme(plot.margin = margin(15, 0, 0, 20))

waffle <- function(data, num_filas){
data %>% filter(no_filas==num_filas)%>% arrange(lightness)%>% ggplot()+
  geom_waffle(aes(values=1,fill=hex),flip = T,n_rows = 60)+
  scale_fill_identity()+coord_fixed(clip = "off")+
  theme_void()+
  facet_wrap(vars(factor(word,levels = data %>% distinct(repeticiones,word) %>% arrange(desc(repeticiones)) %>% pull(word))), ncol = 1, labeller = labeller(word))+
  theme(legend.position = "none",
    strip.text = element_text(margin = margin(10, 0, 8, 20), size = 11,hjust = 0,family = "Oswald Medium"),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(0, 0, 0, 0))
} 


waffle_8 <- waffle(common_words_25,8)
waffle_7 <- waffle(common_words_25,7)
waffle_6 <- waffle(common_words_25,6)
waffle_5 <- waffle(common_words_25,5)
waffle_4 <- waffle(common_words_25,4)
waffle_4_2  <-  add_sub(waffle_4,"Data: The Pudding | Rodrigo Revilla", size = 8, fontfamily = "Oswald Medium", color = "grey50")

plot_grid(title,waffle_8,waffle_7,waffle_6,waffle_5,waffle_4_2, ncol = 1,rel_heights = c(0.5,1.05,3,0.87,1.623,3.18) ) +
  theme(plot.background = element_rect(fill = "grey97", color = NA))+
  ggsave("./2021-14/makeup.png", dpi = 320, height = 12, width = 7)

