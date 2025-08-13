# Criação de cubo de dados, Self-Organizing Maps (SOM) e Classificação  --------------------------------------------------------------------
# Teste de Amostras  -----------------------------------------------------------------------------------------------------------------------
# Tiles 034018, 035018, 034017, 033018 -----------------------------------------------------------------------------------------------------
# Conjunto de classes: grupo B --------------------------------------------
# Teste de acurácia com novas amostras em todos os tiles ------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tibble) # Pacote para visualizar tabelas
library(sits) # Pacote para análises de séries temporais de imagens de satélite
#library(sitsdata) # Pacote para obter conjunto de dados de amostras
library(kohonen) # Pacote para plotar o mapa SOM
library(randomForestExplainer)
# library(torch)
# torch::install_torch()
library(tidyverse)
library(terra)
library(raster)

# Ler cubo com todos os tiles ---------------------------------------------

cubo_tile_034018_entorno <- readRDS("cubo_tile_034018_entorno.rds")

# Ler cubo com novos índices para grupo 4 ---------------------------------

cubo_tile034018_entorno_g4_2b <- readRDS("cubo_tile034018_entorno_g4_2b.rds")

view(cubo_tile034018_entorno_g4_2b)

# Adicionar amostras ao cubo ----------------------------------------------

cubo_samples_tile034018_entorno_g4_2b <- sits_get_data(
  cubo_tile034018_entorno_g4_2b, # Cubo geral com bandas e índices
  samples = "Tile_034018_amostras_classificacao123_treinadas_manual_classes_B.shp", # Arquivo shapefile do tile 034018
  label_attr = "classe_b", # Coluna que indica as classes das amostras (pontos)
  bands = c("B11", "DBSI", "NDII", "NDVI"), 
  memsize = 15, # consumo de memória
  multicores = 2, # Número de núcleos a serem usados. Quanto maior, mais rápido o processamento
  progress = TRUE) # Acompanhar carregamento

## Salvar cubo com amostras

saveRDS(cubo_samples_tile034018_entorno_g4_2b, file = "cubo_samples_tile034018_entorno_g4_2b.rds") 
cubo_samples_tile034018_entorno_g4_2b <- readRDS("cubo_samples_tile034018_entorno_g4_2b.rds")

view(cubo_samples_tile034018_entorno_g4_2b)
sits_bands(cubo_samples_tile034018_entorno_g4_2b)
sits_labels(cubo_samples_tile034018_entorno_g4_2b)