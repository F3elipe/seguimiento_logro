# Sistema de seguimiento de logro Encla 2023

El presente repositorio tiene como objetivo la programación de los códigos asociados al sistema de seguimiento de logro muestral de la Encuesta Laboral (Encla) ejecutada por la Dirección del Trabajo (DT) y el Instituto Nacional de Estadísticas (INE) en su versión 2023. Este está estructurado de la siguiente manera:

1. **R**: carpeta que contiene los códigos de procesamiento (`01proc.R`) y análisis (`02analisis_XX.R`) de datos asociados al seguimiento de logro de unidades económicas (empresas) en la Encuesta. El código cuyo nombre termina en el sufijo `logro_global.R` incluye el análisis global de la muestra, mientras que los códigos con sufijo `_tasa_logro.R` y `_tasa_respuesta.R` corresponden exactamente a lo que indica su nombre, o sea al análisis de seguimiento de logro y respuesta para cada una de las regiones del país. Cada uno de los códigos realiza un análisis global del logro de unidades, para luego realizarlos por **región** [^1], **actividad económica** y **tamaño**.

[^1]: Sólo válido para `02analisis_00.R`.

2. **input**: incluye los insumos necesarios para el procesamiento de datos (`input/data`), así como documentos asociados a tales datos. 
3. **output**: incluye los productos asociados al procesamiento y análisis de los datos de seguimiento de logro de unidades económicas. En `output/data` se incluyen los datos procesados, mientras que la subcarpeta `output/docs` incluye los informes de cada iteración. El formato del nombre de tales documentos es `logro_encla_DD_MM`. 

4. Nomenclatura: la nomenclatura utilizada en el código para su posterior uso sistemático, consistió en distintos conceptos como, "abs" para clasificar gráficos con frecuencias absolutas; "rel" para los gráficos de frecuencias relativas; "tot" para indicar gráficos a nivel nacional; "reg" para gráficos indicando información de alguna región en específico; "tam" para gráficos indicando el tamaño de la unidad económica, y por último "act" para indicar que el gráfico corresponde a la actividad económica de la unidad económica.
