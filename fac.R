library(ggplot2)
library(tidyverse)
library(data.table)
library(RColorBrewer)
library(sjPlot)
library(car)

data <- readxl::read_xlsx("FAC.xlsx")

data$Contagem <- as.numeric(data$Contagem)

data <- data %>%
  mutate(relO2 = O2/Contagem) %>%
  mutate(relamonia = Amônia/Contagem)

data <- data %>% 
  rowwise() %>% 
  group_by(Tratamento, Tempo) %>%
  mutate(meanO2 = mean(O2))%>%
  mutate(meanrelO2 = mean(relO2)) %>%
  mutate(meanamonia = mean(Amônia)) %>%
  mutate(meanrelamonia = mean(relamonia)) %>%
  ungroup()


data <- data %>%
  mutate(meanamonia_perc = case_when(Tempo == "T0" ~ 100,
                                     Tempo == "T1" ~ (meanamonia*100)/shift(meanamonia),
                                     Tempo == "T2" ~ (meanamonia*100)/shift(meanamonia, 2),
                                     Tempo == "T3" ~ (meanamonia*100)/shift(meanamonia, 3),
                                     Tempo == "T4" ~ (meanamonia*100)/shift(meanamonia, 4))) %>%
  
  mutate(meanrelamonia_perc = case_when(Tempo == "T0" ~ 100,
                                     Tempo == "T1" ~ (meanrelamonia*100)/shift(meanrelamonia),
                                     Tempo == "T2" ~ (meanrelamonia*100)/shift(meanrelamonia, 2),
                                     Tempo == "T3" ~ (meanrelamonia*100)/shift(meanrelamonia, 3),
                                     Tempo == "T4" ~ (meanrelamonia*100)/shift(meanrelamonia, 4))) %>%
  
  mutate(meanO2_perc = case_when(Tempo == "T0" ~ 100,
                                 Tempo == "T1" ~ (meanO2*100)/shift(meanO2),
                                 Tempo == "T2" ~ (meanO2*100)/shift(meanO2, 2),
                                 Tempo == "T3" ~ (meanO2*100)/shift(meanO2, 3),
                                 Tempo == "T4" ~ (meanO2*100)/shift(meanO2, 4))) %>%
  mutate(Concentração = case_when(Tratamento == "Controle Fermento" ~ 0,
                                  Tratamento == "Controle" ~ 0,
                                  Tratamento == "Controle 30 ppt" ~ 0, 
                                  Tratamento == "AgNP 0.1" ~ 0.1,
                                  Tratamento == "AgNP 10" ~ 10,
                                  Tratamento == "AgNP 100" ~ 100))


## FIGURA 3
data %>% 
  mutate(Tratamento = fct_reorder(Tratamento, Concentração)) %>%
  filter(Tratamento != "Controle 30 ppt" & Tratamento != "Controle Fermento" & Contagem != "NA") %>%
  ggplot(data, mapping = aes(x = Tratamento, y = Contagem, color = Tratamento)) +
    geom_point(shape = 95,size  = 10) +
    geom_line(size = 2) +
    stat_summary(geom = "point", fun.y = "mean", size = 4) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(expand = c(0,0), limits = c(0,650)) +
    theme_bw() +
    labs(y = "Rotíferos por mL") +
    theme(axis.title = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))  

##FIGURA 4
data %>% 
  mutate(Tratamento = fct_reorder(Tratamento, Concentração)) %>%
  filter(Tratamento != "Controle 30 ppt" & Tratamento != "Controle Fermento") %>%
  ggplot(data, mapping = aes(x = Tratamento, y = relO2, color = Tratamento)) +
    geom_point(shape = 95,size  = 10) +
    geom_line(size = 2) +
    stat_summary(geom = "point", fun.y = "mean", size = 4) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(limits = c(0,NA), expand = c(0,NA)) +
    #scale_y_continuous(expand = c(0,0), limits = c(3,6), 
                       #breaks = seq(from = 0, to = 6, by = 0.25),
                       #labels = scales::label_number_si(unit = "mg/L")) +
    theme_bw() +
    facet_grid(~ Tempo) +
    labs(y = "Concentração O2 (mg/L) relativa por indivíduo",
         x = NULL) +
    theme(axis.title = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(0.56)),
          legend.title = element_text(size = rel(1.5)),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))

##FIGURA 5a:
data %>% 
  mutate(Tratamento = fct_reorder(Tratamento, Concentração)) %>%
  filter(Tratamento != "Controle 30 ppt" & Tratamento != "Controle Fermento") %>%
  ggplot(data, mapping = aes(x = Tempo, y = meanrelO2, group = Tratamento, color = Tratamento)) +
    geom_line(size = 1.5) +
    scale_color_brewer(palette = "Dark2") +
    #scale_y_continuous(expand = c(0,0), limits = c(3,6), 
   #                    breaks = seq(from = 0, to = 6, by = 0.25),
     #                  labels = scales::label_number_si(unit = "mg/L")) +
    scale_x_discrete(expand = c(0,0)) + 
    theme_bw() +
    labs(y = "Concentração média de O2 (mg/L) relativa por indivíduo") +
    theme(axis.title = element_text(size = rel(1.3)),
          axis.text.x = element_text(size = rel(1.3)),
          legend.position = "none")
  

##FIGURA 5b:
data %>% 
  mutate(Tratamento = fct_reorder(Tratamento, Concentração)) %>%
  filter(Tratamento != "Controle 30 ppt" & Tratamento != "Controle Fermento") %>%
  ggplot(data, mapping = aes(x = Tempo, y = meanO2_perc, group = Tratamento, color = Tratamento)) +
    geom_line(size = 1.5) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_discrete(expand = c(0,0)) + 
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       limits = c(70,100),
                       expand = c(0, 0)) +
    theme_bw() +
    labs(y = "Concentração média de O2 (%)") +
    theme(axis.title = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.3)),
          axis.text.y = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.5)),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) 




#FIGURA 6
data %>% 
  mutate(Tratamento = fct_reorder(Tratamento, Concentração)) %>%
  filter(Tratamento != "Controle 30 ppt" & Tratamento != "Controle Fermento") %>%
  ggplot(data, mapping = aes(x = Tratamento, y = relamonia, color = Tratamento)) +
  geom_point(shape = 95,size  = 10) +
  geom_line(size = 2) +
  stat_summary(geom = "point", fun.y = "mean", size = 4) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0,NA), expand = c(0,NA)) +
  #scale_y_continuous(expand = c(0,0), limits = c(0,1), 
  #                   breaks = seq(from = 0, to = 1, by = 0.1),
   #                  labels = scales::label_number_si(unit = "mg/L")) +
  theme_bw() +
  facet_grid(~ Tempo) +
  labs(y = "Concentração de amônia (mg/L) relativa por indivíduo") +
  theme(axis.title = element_text(size = rel(1.4)),
        axis.text.x = element_text(size = rel(0.56)),
        legend.title = element_text(size = rel(1.5)),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

#FIGURA 7a:
data %>% 
  mutate(Tratamento = fct_reorder(Tratamento, Concentração)) %>%
  filter(Tratamento != "Controle 30 ppt" & Tratamento != "Controle Fermento") %>%
  ggplot(data, mapping = aes(x = Tempo, y = meanrelamonia, group = Tratamento, color = Tratamento)) +
    geom_line(size = 1.5) +
    scale_color_brewer(palette = "Dark2") +
    #scale_y_continuous(expand = c(0,0), limits = c(0,1), 
     #                  breaks = seq(from = 0, to = 1, by = 0.1),
      #                 labels = scales::label_number_si(unit = "mg/L")) +
    scale_x_discrete(expand = c(0,0)) +
    theme_bw() +
    labs(y = "Concentração de amônia (mg/L) relativa por indivíduo") +
    theme(axis.title = element_text(size = rel(1.3)),
          axis.text.x = element_text(size = rel(1.3)),
          legend.position = "none")

#FIGURA 7b:
data %>% 
  mutate(Tratamento = fct_reorder(Tratamento, Concentração)) %>%
  filter(Tratamento != "Controle 30 ppt" & Tratamento != "Controle Fermento") %>%
  ggplot(data, mapping = aes(x = Tempo, y = meanamonia_perc, group = Tratamento, color = Tratamento)) +
  geom_line(size = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     limits = c(0, NA),
                     expand = c(0, 0)) +
  theme_bw() +
  labs(y = "Concentração média de amônia (%)") +
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.3)),
        axis.text.y = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.5)),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) 
