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


# Global por región -------------------------------------------------------

# Frq. abs.

# Iniciar el loop para generar los gráficos por separado
for (i in 1:16) {
  
  # Filtrar los datos correspondientes a la región actual
  datos = filter(l_reg, REGION == unique(REGION)[i])
  
  # Crear el gráfico utilizando ggplot2
  grafico <- datos %>% 
    group_by(logro_tot) %>% 
    summarise(n = sum(n)) %>% 
    ggplot(aes(x=logro_tot, y=n,fill=logro_tot)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(title = "Número de unidades económicas logradas",
         subtitle = paste("Región de",datos$REGION[1]),
         caption = "Fuente: Elaboración propia",
         x = '',
         y = 'Total') + 
    guides(fill=guide_legend(title="Estado de logro")) +
    geom_text(aes(label= format(n, big.mark = ".")), position=position_dodge(width=0.9), vjust=-0.25) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  # Guardar el gráfico en un archivo png con el nombre de la región actual
  saveRDS(grafico, paste0(paste0("output/img/", datos$REGION[1]), "_abs_tot.rds"))
}

#Frq. rel.

for (i in 1:16){
  # Filtrar los datos correspondientes a la región actual
  datos = filter(l_reg, REGION == unique(REGION)[i])
  
  # Crear gráfico
  grafico <- datos %>% 
    rowwise() %>% 
    mutate(n = round(n/sm,4)*100) %>% 
    group_by(REGION, logro_tot) %>% 
    summarise(n = round(mean(n),3)) %>% 
    ggplot(aes(x=labels(REGION), y=n,fill=logro_tot)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_x_discrete(labels = c("Logro total", "Logro parcial (sin AA)", "Logro parcial")) +
    labs(title = "Tasa de respuesta de unidades económicas a nivel nacional",
         subtitle = paste("Región de",datos$REGION[1]),
         caption = "Fuente: Elaboración propia",
         x = '',
         y = '%') + 
    guides(fill=guide_legend(title="Estado de logro")) +
    geom_text(aes(label=n), position=position_dodge(width=0.9), hjust=0.5, vjust = -0.2)
  
  # Guardar gráfico
  saveRDS(grafico, paste0(paste0("output/img/", datos$REGION[1]), "_rel_tot.rds"))
}

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
          geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank()), "output/img/global_abs.rds")

# Frq. rel.

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


# Por tamaño --------------------------------------------------------------

# Frq. abs.

for (i in 1:16) {
  # Filtrar los datos correspondientes a la región actual
  datos = filter(l_tam, REGION == unique(REGION)[i])
  
  # Crear el gráfico utilizando ggplot2
  grafico <- datos %>% 
    group_by(REGION,TAMANO, logro_tot) %>% 
    summarise(n = sum(n)) %>% 
    ggplot(aes(x=TAMANO, y=n,fill=logro_tot)) +
    geom_bar(stat="identity", position=position_dodge()) +
    guides(fill=guide_legend(title="Estado de logro")) +
    labs(title = "Número de unidades económicas logradas por tamaño de empresa",
         subtitle = paste("Región de",datos$REGION[1]),
         caption = "Fuente: Elaboración propia",
         x = '',
         y = 'Total') + 
    guides(fill=guide_legend(title="Estado de logro")) +
    geom_text(aes(label= format(n, big.mark = ".")), position=position_dodge(width=0.9), vjust=-0.25) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  # Guardar gráfico
  saveRDS(grafico, paste0(paste0("output/img/", datos$REGION[1]), "_abs_tam.rds"))
}


# Frq. rel.

for(i in 1:16) {
  # Filtrar los datos correspondientes a la región actual
  datos = filter(l_tam, REGION == unique(REGION)[i])
  
  # Crear gráfico
  grafico <- datos %>% 
    rowwise() %>% 
    mutate(n = round(n/sm,4)*100) %>% 
    group_by(TAMANO, logro_tot) %>% 
    summarise(n = round(mean(n),3)) %>% 
    ggplot(aes(x=TAMANO, y=n,fill=logro_tot)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(title = "Tasa de respuesta de unidades económicas por tamaño de empresa",
         subtitle = paste("Región de",datos$REGION[1]),
         caption = "Fuente: Elaboración propia",
         x = '',
         y = '%') + 
    guides(fill=guide_legend(title="Estado de logro")) +
    geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)
  
  # Guardar gráfico
  saveRDS(grafico, paste0(paste0("output/img/", datos$REGION[1]), "_rel_tam.rds"))
}


# Por actividad -----------------------------------------------------------

# Frq. abs.
for(i in 1:16) {
  # Filtrar los datos correspondientes a la región actual
  datos = filter(l_act, REGION == unique(REGION)[i])
  
  # Crear gráfico
  grafico <- datos %>% 
    group_by(ACTECONOMICAr, logro_tot) %>% 
    summarise(n = sum(n)) %>% 
    ggplot(aes(x=ACTECONOMICAr, y=n,fill=logro_tot)) +
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() +
    labs(title = "Número de unidades económicas logradas por \n actividad económica",
         subtitle = paste("Región de",datos$REGION[1]),
         caption = "Fuente: Elaboración propia",
         x = '',
         y = 'Total') + 
    guides(fill=guide_legend(title="Estado de logro")) +
    #labs(title = "Número de unidades económicas logradas a nivel nacional por actividad económica",
    #     caption = "Fuente: Elaboración propia",
    #     x = '',
    #     y = 'Total') + 
    guides(fill=guide_legend(title="Estado de logro")) +
    geom_text(aes(label= format(n, big.mark = ".")), position=position_dodge(width=0.9), hjust=-0.25, vjust = 0.5)
  
  # Guardar gráfico
  saveRDS(grafico, paste0(paste0("output/img/", datos$REGION[1]), "_abs_act.rds"))
}


# Frq. rel.
for(i in 1:16) {
  # Filtrar los datos correspondientes a la región actual
  datos = filter(l_act, REGION == unique(REGION)[i])
  
  # Crear gráfico
  grafico <- datos %>% 
    rowwise() %>% 
    mutate(n = round(n/sm,4)*100) %>% 
    group_by(ACTECONOMICAr, logro_tot) %>% 
    summarise(n = round(mean(n),3)) %>% 
    ggplot(aes(x=ACTECONOMICAr, y=n,fill=logro_tot)) +
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() +
    scale_fill_discrete(breaks = c("Logro parcial", "Logro parcial (sin AA)", "Logro total")) +
    labs(title = "Tasa de respuesta de unidades económicas \n por actividad económica",
         subtitle = paste("Región de",datos$REGION[1]),
         caption = "Fuente: Elaboración propia",
         x = '',
         y = '%') + 
    guides(fill=guide_legend(title="Estado de logro")) +
    geom_text(aes(label=n), position=position_dodge(width=0.9), hjust=-0.25, vjust = 0.5)
  
  # Guardar gráfico
  saveRDS(grafico, paste0(paste0("output/img/", datos$REGION[1]), "_rel_act.rds"))
}

