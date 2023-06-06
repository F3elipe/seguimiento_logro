rm(list = ls())

# Análisis sistema de seguimiento de logro --------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               sjmisc,
               sjPlot,
               ggplot2)

# Cargar datos ------------------------------------------------------------

data = readRDS("output/data/directorio_proc.rds")

# Análisis ----------------------------------------------------------------


# Logro nacional ----------------------------------------------------------


## Logro global ------------------------------------------------------------


#Frq. abs.

data %>% 
  group_by(logro_tot) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=logro_tot, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Número de unidades económicas logradas a nivel nacional",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = 'Total') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

data %>% 
  group_by(logro) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=logro, y=n,fill=logro)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Número de unidades económicas logradas a nivel nacional",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = 'Total') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# data %>% 
#   summarise(aa = sum(autoap),
#             emp = sum(emp),
#             st = sum(sind_trab)) %>% 
#   pivot_longer(1:3,
#                names_to = "cuestionario",
#                values_to = "n") %>% 
#   ggplot(aes(x=cuestionario, y=n, fill=cuestionario)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   labs(title = "Número de unidades económicas logradas a nivel nacional",
#        caption = "Fuente: Elaboración propia",
#        x = '',
#        y = 'Total') + 
#   guides(fill=guide_legend(title="Estado de logro")) +
#   geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank())
  
#Frq. rel.

data %>% 
  group_by(logro_tot) %>% 
  summarise(n = round(n()/nrow(.), 4)*100) %>%
  ggplot(aes(x=logro_tot, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Porcentaje de unidades económicas logradas a nivel nacional",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = '%') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 


## Logro por tamaño -----------------  

#Frq. abs.

data %>% 
  group_by(TAMANO, logro_tot) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=TAMANO, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Porcentaje de unidades económicas logradas a nivel nacional",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = 'Total') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  labs(title = "Número de unidades económicas logradas a nivel nacional por tamaño de empresa",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = 'Total') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)

#Frq. rel.

data %>% 
  group_by(TAMANO, logro_tot) %>% 
  summarise(n = round(n()/nrow(.), 4)*100) %>% 
  ggplot(aes(x=TAMANO, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Porcentaje de unidades económicas logradas a nivel nacional por tamaño de empresa",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = '%') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)

## Logro por actividad económica -------------------------------------------

#Frq. abs.

data %>% 
  group_by(ACTECONOMICAr, logro_tot) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=ACTECONOMICAr, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  labs(title = "Porcentaje de unidades económicas logradas a nivel nacional",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = 'Total') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  labs(title = "Número de unidades económicas logradas a nivel nacional por actividad económica",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = 'Total') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), hjust=-0.25, vjust = 0.5)

#Frq. rel.

data %>% 
  group_by(ACTECONOMICAr, logro_tot) %>% 
  summarise(n = round(n()/nrow(.), 4)*100) %>%
  ggplot(aes(x=ACTECONOMICAr, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  labs(title = "Porcentaje de unidades económicas logradas a nivel nacional por actividad económica",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = '%') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), hjust=-0.25, vjust = 0.5)

# Logro regional ----------------------------------------------------------


