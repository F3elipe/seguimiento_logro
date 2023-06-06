rm(list = ls())

# Análisis sistema de seguimiento de logro --------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               sjmisc,
               sjPlot,
               ggplot2)

# Cargar datos ------------------------------------------------------------

data = readRDS("output/data/directorio_proc.rds")

# Crear tablas ------------------------------------------------------------

# Region, tamaño, actividad


l_reg = merge(data %>% group_by(REGION, logro_tot) %>% count(.),
              data %>% group_by(REGION) %>% summarise(sm = sum(unique(act_tot_sm))),
              "REGION") 

l_tam = merge(data %>% group_by(REGION, TAMANO, logro_tot) %>% count(.),
              data %>% group_by(REGION, TAMANO) %>% summarise(sm = sum(unique(tamano_tot_sm))),
              c("REGION", "TAMANO"))

l_act = merge(data %>% group_by(REGION, ACTECONOMICAr, logro_tot) %>% count(.),
              data %>% group_by(REGION, ACTECONOMICAr) %>% summarise(sm = sum(unique(act_tot_sm))),
              c("REGION", "ACTECONOMICAr"))


# Análisis ----------------------------------------------------------------

# Logro nacional ----------------------------------------------------------


## Logro global ------------------------------------------------------------


#Frq. abs.

saveRDS(l_reg %>% 
  group_by(logro_tot) %>% 
  summarise(n = sum(n)) %>% 
  ggplot(aes(x=logro_tot, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Número de unidades económicas logradas a nivel nacional",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = 'Total') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label= format(n, big.mark = ".")), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()), "output/img/global_abs.rds")


# data %>% 
#   group_by(logro) %>% 
#   summarise(n = n()) %>% 
#   ggplot(aes(x=logro, y=n,fill=logro)) +
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

saveRDS(l_reg %>% 
  group_by(logro_tot) %>% 
  summarise(n = round(sum(n)/4267, 4)*100) %>% 
  ggplot(aes(x=logro_tot, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Porcentaje de unidades económicas logradas a nivel nacional",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = '%') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()), "output/img/global_rel.rds") 


## Logro por tamaño -----------------  

#Frq. abs.

saveRDS(l_tam %>% 
  group_by(TAMANO, logro_tot) %>% 
  summarise(n = sum(n)) %>% 
  ggplot(aes(x=TAMANO, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Número de unidades económicas logradas a nivel nacional \n por tamaño de empresa",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = 'Total') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label= format(n, big.mark = ".")), position=position_dodge(width=0.9), vjust=-0.25), "output/img/global_tam_abs.rds")

#Frq. rel.

saveRDS(l_tam %>% 
  rowwise() %>% 
  mutate(n = round(n/sm,4)*100) %>% 
  group_by(TAMANO, logro_tot) %>% 
  summarise(n = round(mean(n),2)) %>% 
  ggplot(aes(x=TAMANO, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Porcentaje de unidades económicas logradas a nivel nacional \n por tamaño de empresa",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = '%') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25), "output/img/global_tam_rel.rds")

## Logro por actividad económica -------------------------------------------

#Frq. abs.

saveRDS(l_act %>% 
  group_by(ACTECONOMICAr, logro_tot) %>% 
  summarise(n = sum(n)) %>% 
  ggplot(aes(x=fct_reorder(ACTECONOMICAr,desc(ACTECONOMICAr)), y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
    scale_fill_discrete(breaks = c("Logro parcial", "Logro parcial (sin AA)", "Logro total")) +
  labs(title = "Número de unidades económicas logradas a nivel nacional \n por actividad económica",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = 'Total') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label= format(n, big.mark = ".")), position=position_dodge(width=0.9), hjust=-0.25, vjust = 0.5), "output/img/global_act_abs.rds")

#Frq. rel.

saveRDS(l_act %>% 
  rowwise() %>% 
  mutate(n = round(n/sm,4)*100) %>% 
  group_by(ACTECONOMICAr, logro_tot) %>% 
  summarise(n = round(mean(n),2)) %>% 
  ggplot(aes(x=fct_reorder(ACTECONOMICAr,desc(ACTECONOMICAr)), y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
    scale_fill_discrete(breaks = c("Logro parcial", "Logro parcial (sin AA)", "Logro total")) +
  labs(title = "Porcentaje de unidades económicas logradas a nivel nacional \n por actividad económica",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = '%') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), hjust=-0.25, vjust = 0.5), "output/img/global_act_rel.rds")

# Logro regional ----------------------------------------------------------

# Frq. abs.
saveRDS(l_act %>% 
  group_by(REGION, logro_tot) %>% 
  summarise(n = sum(n)) %>% 
  ggplot(aes(x=fct_reorder(REGION,desc(REGION)), y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
    scale_fill_discrete(breaks = c("Logro parcial", "Logro parcial (sin AA)", "Logro total")) +
  labs(title = "Número de unidades económicas logradas a nivel nacional por región",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = 'Total') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label= format(n, big.mark = ".")), position=position_dodge(width=0.9), hjust=-0.25, vjust = 0.5), "output/img/global_reg_abs.rds")

#Frq. rel.

saveRDS(l_reg %>% 
  rowwise() %>% 
  mutate(n = round(n/sm,4)*100) %>% 
  group_by(REGION, logro_tot) %>% 
  summarise(n = round(mean(n),2)) %>% 
  ggplot(aes(x=fct_reorder(REGION,desc(REGION)), y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
    scale_fill_discrete(breaks = c("Logro parcial", "Logro parcial (sin AA)", "Logro total")) +
  labs(title = "Porcentaje de unidades económicas logradas a nivel nacional \n por región",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = '%') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), hjust=-0.25, vjust = 0.5), "output/img/global_reg_rel.rds")



