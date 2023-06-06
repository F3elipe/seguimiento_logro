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

# Region y tamaño

l_reg = merge(data %>% group_by(REGION, logro_tot) %>% count(.),
              data %>% group_by(REGION) %>% summarise(m_obj = sum(unique(tot_reg_tam))),
              "REGION")

l_tam = merge(data %>% group_by(REGION, TAMANO, logro_tot) %>% count(.),
              data %>% group_by(REGION, TAMANO) %>% summarise(m_obj = sum(unique(tot_reg_tam))),
              c("REGION", "TAMANO"))

# Análisis ----------------------------------------------------------------


# Global por región -------------------------------------------------------

#Frq. rel.

for (i in 1:16){
  # Filtrar los datos correspondientes a la región actual
  datos = filter(l_reg, REGION == unique(REGION)[i])
  
  # Crear gráfico
  grafico <- datos %>% 
    rowwise() %>% 
    mutate(n = round(n/m_obj,4)*100) %>% 
    group_by(REGION, logro_tot) %>% 
    summarise(n = round(mean(n),2)) %>% 
    ggplot(aes(x=labels(REGION), y=n,fill=logro_tot)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(title = "Porcentaje de unidades económicas logradas a nivel nacional",
         subtitle = paste("Región de",datos$REGION[1]),
         caption = "Fuente: Elaboración propia",
         x = '',
         y = '%') + 
    scale_x_discrete(labels=c('Logro total','Logro parcial (sin AA)','Logro parcial'))+
    guides(fill=guide_legend(title="Estado de logro")) +
    geom_text(aes(label=n), position=position_dodge(width=0.9), hjust=0.5, vjust = -0.2)
  
  # Guardar gráfico
  saveRDS(grafico, paste0(paste0("output/img/", datos$REGION[1]), "_rel_mtot.rds"))
}

# Por tamaño --------------------------------------------------------------

# Frq. rel.

for(i in 1:16) {
  # Filtrar los datos correspondientes a la región actual
  datos = filter(l_tam, REGION == unique(REGION)[i])
  
  # Crear gráfico
  grafico <- datos %>% 
    rowwise() %>% 
    mutate(n = round(n/m_obj,4)*100) %>% 
    group_by(TAMANO, logro_tot) %>% 
    summarise(n = round(mean(n),2)) %>% 
    ggplot(aes(x=TAMANO, y=n,fill=logro_tot)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(title = "Porcentaje de unidades económicas logradas por tamaño de empresa",
         subtitle = paste("Región de",datos$REGION[1]),
         caption = "Fuente: Elaboración propia",
         x = '',
         y = '%') + 
    guides(fill=guide_legend(title="Estado de logro")) +
    geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)
  
  # Guardar gráfico
  saveRDS(grafico, paste0(paste0("output/img/", datos$REGION[1]), "_rel_mtam.rds"))
}



