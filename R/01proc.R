
# Procesamiento Sistema seguimiento de logro Encla 2023 -------------------
rm(list = ls())

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               readxl,
               car,
               sjmisc)

# Cargar datos ------------------------------------------------------------
data = read_xlsx("input/data/directorio.xlsx")
#region = read_xlsx("input/data/muestra-objetivo-encla2023.xlsx", sheet = 1)
#act = read_xlsx("input/data/muestra-objetivo-encla2023.xlsx", sheet = 2)
#tamano = read_xlsx("input/data/muestra-objetivo-encla2023.xlsx", sheet = 3)
tamano_sm = read_xlsx("input/data/muestra_sm.xlsx",sheet = 1)
act_sm = read_xlsx("input/data/muestra_sm.xlsx",sheet = 2)
obj_reg_tam = read_xlsx("input/data/objetivo_reg_tam.xlsx")

# Procesar datos ----------------------------------------------------------
proc = data %>% 
  rename(autoap = Autoaplicado,
         emp = Empleador,
         sind_trab = "Sindicato/Trabajadores",
         logro = "Logro de la unidad económica") %>% 
  mutate(logro = factor(case_when(if_all(c(autoap, emp, sind_trab), ~.==1) ~ "Logrado",
                                  autoap == 1 & if_all(c(emp, sind_trab), ~.==0) ~ "AA",
                                  if_all(c(autoap, emp), ~.==1) & sind_trab == 0 ~ "AA + E", 
                                  if_all(c(autoap, sind_trab), ~.==1) & emp == 0 ~ "AA + S/T",
                                  if_all(c(autoap, sind_trab), ~.==0) & emp == 1 ~ "E",
                                  if_all(c(emp, sind_trab), ~.==1) & autoap == 0 ~ "E + S/T",
                                  if_all(c(autoap, emp), ~.==0) & sind_trab == 1 ~ "S/T",
                                  if_all(c(autoap, emp, sind_trab), ~.==0) ~ "Sin logro",
                                  TRUE ~ NA_character_),
                        levels = c("Logrado",
                                   "AA",
                                   "AA + E",
                                   "AA + S/T",
                                   "E",
                                   "E + S/T",
                                   "S/T",
                                   "Sin logro")),
         logro_tot = factor(case_when(if_all(c(autoap, emp, sind_trab), ~.==1) ~ "Logro total",
                                      if_all(c(emp, sind_trab), ~.==1) & autoap == 0 ~ "Logro parcial (sin AA)",
                                      if_any(c(autoap, emp, sind_trab), ~.==1) ~ "Logro parcial",
                                      if_all(c(autoap, emp, sind_trab), ~.==0) ~ "Sin logro",
                                      TRUE ~ NA_character_),
                            levels = c("Logro total",
                                       "Logro parcial (sin AA)",
                                       "Logro parcial",
                                       "Sin logro")),
         TAMANO = car::recode(.$TAMANO,
                              "1 = '1. Micro';
                              2 = '2. Pequeña';
                              3 = '3. Mediana';
                              4 = '4. Grande'", as.factor = T),
         REGION = car::recode(.$REGION, 
                              "1 = 'I. Tarapacá';
                              2 = 'II. Antofagasta';
                              3 = 'III. Atacama';
                              4 = 'IV. Coquimbo';
                              5 = 'V. Valparaíso';
                              6 = 'VI. OHiggins';
                              7 = 'VII. Maule';
                              8 = 'VIII. Bío-bío';
                              9 = 'IX. La Araucanía';
                              10 = 'X. Los Lagos';
                              11 = 'XI. Aysén';
                              12 = 'XII. Magallanes';
                              13 = 'XIII. Metropolitana';
                              14 = 'XIV. Los Ríos';
                              15 = 'XV. Arica y Parinacota';
                              16 = 'XVI. Ñuble'", as.factor = T,
                              levels = c('XV. Arica y Parinacota',
                                         'I. Tarapacá',
                                         'II. Antofagasta',
                                         'III. Atacama',
                                         'IV. Coquimbo',
                                         'V. Valparaíso',
                                         'XIII. Metropolitana',
                                         'VI. OHiggins',
                                         'VII. Maule',
                                         'XVI. Ñuble',
                                         'VIII. Bío-bío',
                                         'IX. La Araucanía',
                                         'XIV. Los Ríos',
                                         'X. Los Lagos',
                                         'XI. Aysén',
                                         'XII. Magallanes')),
         ACTECONOMICAr = car::recode(.$ACTECONOMICA,
                                    "'A' = 'A. Agr. y pesca';
                                    'B' = 'B. Minería';
                                    'C' = 'C. Manufactura';
                                    'D-E' = 'D-E. Serv. básicos';
                                    'F' = 'F. Construcción';
                                    'G' = 'G. Comercio';
                                    'H-J' = 'H-J. Transporte y TIC';
                                    'I' = 'I. Alojamiento';
                                    'K-L' = 'K-L. Financieras e inmobiliarias';
                                    'M-N' = 'M-N. Profesionales y administrativos';
                                    'P' = 'P. Enseñanza';
                                    'Q' = 'Q. Salud';
                                    'R-S' = 'R-S. Otros servicios'", as.factor = T),
         tot = 4267)

# region = region %>% 
#   select(1,3) %>% 
#   mutate(REGION = car::recode(.$REGION, 
#                               "1 = 'I. Tarapacá';
#                               2 = 'II. Antofagasta';
#                               3 = 'III. Atacama';
#                               4 = 'IV. Coquimbo';
#                               5 = 'V. Valparaíso';
#                               6 = 'VI. OHiggins';
#                               7 = 'VII. Maule';
#                               8 = 'VIII. Bío-bío';
#                               9 = 'IX. La Araucanía';
#                               10 = 'X. Los Lagos';
#                               11 = 'XI. Aysén';
#                               12 = 'XII. Magallanes';
#                               13 = 'XIII. Metropolitana';
#                               14 = 'XIV. Los Ríos';
#                               15 = 'XV. Arica y Parinacota';
#                               16 = 'XVI. Ñuble'", as.factor = T,
#                               levels = c('XV. Arica y Parinacota',
#                                          'I. Tarapacá',
#                                          'II. Antofagasta',
#                                          'III. Atacama',
#                                          'IV. Coquimbo',
#                                          'V. Valparaíso',
#                                          'XIII. Metropolitana',
#                                          'VI. OHiggins',
#                                          'VII. Maule',
#                                          'XVI. Ñuble',
#                                          'VIII. Bío-bío',
#                                          'IX. La Araucanía',
#                                          'XIV. Los Ríos',
#                                          'X. Los Lagos',
#                                          'XI. Aysén',
#                                          'XII. Magallanes')))
# act = act %>% select(ACTECONOMICAr = 1,3) %>% 
#   mutate(ACTECONOMICAr = car::recode(.$ACTECONOMICAr,
#                                      "'A' = 'A. Agr. y pesca';
#                                     'B' = 'B. Minería';
#                                     'C' = 'C. Manufactura';
#                                     'D-E' = 'D-E. Serv. básicos';
#                                     'F' = 'F. Construcción';
#                                     'G' = 'G. Comercio';
#                                     'H-J' = 'H-J. Transporte y TIC';
#                                     'I' = 'I. Alojamiento';
#                                     'K-L' = 'K-L. Financieras e inmobiliarias';
#                                     'M-N' = 'M-N. Profesionales y administrativos';
#                                     'P' = 'P. Enseñanza';
#                                     'Q' = 'Q. Salud';
#                                     'R-S' = 'R-S. Otros servicios'", as.factor = T))
# 
# tamano = select(tamano,1,3)

tamano_sm = tamano_sm %>% 
  rename(tamano_tot_sm = n) %>% 
  mutate(REGION = car::recode(.$REGION, 
                              "1 = 'I. Tarapacá';
                              2 = 'II. Antofagasta';
                              3 = 'III. Atacama';
                              4 = 'IV. Coquimbo';
                              5 = 'V. Valparaíso';
                              6 = 'VI. OHiggins';
                              7 = 'VII. Maule';
                              8 = 'VIII. Bío-bío';
                              9 = 'IX. La Araucanía';
                              10 = 'X. Los Lagos';
                              11 = 'XI. Aysén';
                              12 = 'XII. Magallanes';
                              13 = 'XIII. Metropolitana';
                              14 = 'XIV. Los Ríos';
                              15 = 'XV. Arica y Parinacota';
                              16 = 'XVI. Ñuble'", as.factor = T,
                              levels = c('XV. Arica y Parinacota',
                                         'I. Tarapacá',
                                         'II. Antofagasta',
                                         'III. Atacama',
                                         'IV. Coquimbo',
                                         'V. Valparaíso',
                                         'XIII. Metropolitana',
                                         'VI. OHiggins',
                                         'VII. Maule',
                                         'XVI. Ñuble',
                                         'VIII. Bío-bío',
                                         'IX. La Araucanía',
                                         'XIV. Los Ríos',
                                         'X. Los Lagos',
                                         'XI. Aysén',
                                         'XII. Magallanes')),
         TAMANO = car::recode(.$TAMANO,
                              "1 = '1. Micro';
                              2 = '2. Pequeña';
                              3 = '3. Mediana';
                              4 = '4. Grande'", as.factor = T))

act_sm = act_sm %>%
  rename(act_tot_sm = n) %>% 
  mutate(REGION = car::recode(.$REGION, 
                              "1 = 'I. Tarapacá';
                              2 = 'II. Antofagasta';
                              3 = 'III. Atacama';
                              4 = 'IV. Coquimbo';
                              5 = 'V. Valparaíso';
                              6 = 'VI. OHiggins';
                              7 = 'VII. Maule';
                              8 = 'VIII. Bío-bío';
                              9 = 'IX. La Araucanía';
                              10 = 'X. Los Lagos';
                              11 = 'XI. Aysén';
                              12 = 'XII. Magallanes';
                              13 = 'XIII. Metropolitana';
                              14 = 'XIV. Los Ríos';
                              15 = 'XV. Arica y Parinacota';
                              16 = 'XVI. Ñuble'", as.factor = T,
                              levels = c('XV. Arica y Parinacota',
                                         'I. Tarapacá',
                                         'II. Antofagasta',
                                         'III. Atacama',
                                         'IV. Coquimbo',
                                         'V. Valparaíso',
                                         'XIII. Metropolitana',
                                         'VI. OHiggins',
                                         'VII. Maule',
                                         'XVI. Ñuble',
                                         'VIII. Bío-bío',
                                         'IX. La Araucanía',
                                         'XIV. Los Ríos',
                                         'X. Los Lagos',
                                         'XI. Aysén',
                                         'XII. Magallanes')),
         ACTECONOMICAr = car::recode(.$ACTECONOMICAr,
                                     "'A' = 'A. Agr. y pesca';
                                    'B' = 'B. Minería';
                                    'C' = 'C. Manufactura';
                                    'D-E' = 'D-E. Serv. básicos';
                                    'F' = 'F. Construcción';
                                    'G' = 'G. Comercio';
                                    'H-J' = 'H-J. Transporte y TIC';
                                    'I' = 'I. Alojamiento';
                                    'K-L' = 'K-L. Financieras e inmobiliarias';
                                    'M-N' = 'M-N. Profesionales y administrativos';
                                    'P' = 'P. Enseñanza';
                                    'Q' = 'Q. Salud';
                                    'R-S' = 'R-S. Otros servicios'", as.factor = T))


obj_reg_tam = obj_reg_tam %>% 
  rename(tot_reg_tam = total) %>% 
  mutate(TAMANO = car::recode(.$TAMANO,"'Micro' = '1. Micro';
                                        'Pequeña' = '2. Pequeña';
                                        'Mediana' = '3. Mediana';
                                        'Grande' = '4. Grande'", as.factor = T),
         REGION = car::recode(.$REGION,"'tarapaca' = 'I. Tarapacá';
                              'antofagasta' = 'II. Antofagasta';
                              'atacama' = 'III. Atacama';
                              'coquimbo' = 'IV. Coquimbo';
                              'valparaiso' = 'V. Valparaíso';
                              'ohiggins' = 'VI. OHiggins';
                              'maule' = 'VII. Maule';
                              'bio bio' = 'VIII. Bío-bío';
                              'la araucania' = 'IX. La Araucanía';
                              'los lagos' = 'X. Los Lagos';
                              'aysen' = 'XI. Aysén';
                              'magallanes' = 'XII. Magallanes';
                              'metropolitana' = 'XIII. Metropolitana';
                              'los rios' = 'XIV. Los Ríos';
                              'arica y parinacota' = 'XV. Arica y Parinacota';
                              'ñuble' = 'XVI. Ñuble'", as.factor = T,
                              levels = c('XV. Arica y Parinacota',
                                         'I. Tarapacá',
                                         'II. Antofagasta',
                                         'III. Atacama',
                                         'IV. Coquimbo',
                                         'V. Valparaíso',
                                         'XIII. Metropolitana',
                                         'VI. OHiggins',
                                         'VII. Maule',
                                         'XVI. Ñuble',
                                         'VIII. Bío-bío',
                                         'IX. La Araucanía',
                                         'XIV. Los Ríos',
                                         'X. Los Lagos',
                                         'XI. Aysén',
                                         'XII. Magallanes')))

# Unificar ----------------------------------------------------------------

# proc = merge(proc, region, 
#              by = "REGION")
# proc = merge(proc, act, 
#              by = "ACTECONOMICAr")
# 
# proc = merge(proc, tamano,
#              by = "TAMANO")

proc = merge(proc,tamano_sm,
             by= c("REGION","TAMANO"))

proc = merge(proc,act_sm,
             by = c("REGION","ACTECONOMICAr"))

proc = merge(proc,obj_reg_tam,
             by = c("REGION","TAMANO"))

rm(act_sm, tamano_sm, data,obj_reg_tam)

# Exportar datos ----------------------------------------------------------
saveRDS(proc, "output/data/directorio_proc.rds")






