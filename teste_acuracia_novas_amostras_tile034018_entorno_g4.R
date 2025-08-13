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
library(sf)

# Ler cubo com todos os tiles ---------------------------------------------

cubo_tile_034018_entorno <- readRDS("cubo_tile_034018_entorno.rds")

# Ler cubo com novos índices para grupo 4 ---------------------------------

cubo_tile034018_entorno_g4_2b <- readRDS("cubo_tile034018_entorno_g4_2b.rds")

view(cubo_tile034018_entorno_g4_2b)

# Ler arquivos .shp -------------------------------------------------------

amostras_t034018 <- sf::read_sf("Tile_034018_amostras_classificacao123_treinadas_manual_classes_B.shp")
amostras_novas <- sf::read_sf("Novas_Amostras_Tiles33018_34017_35018.shp")

view(amostras_t034018)
view(amostras_novas)

# Selecionar apenas tiles do entorno --------------------------------------

tiles_entorno_t034018 <- sits_select(data = cubo_tile034018_entorno_g4_2b,
                                     tiles = c("033018", "034017", "035018"))

view(tiles_entorno_t034018)

# Adicionar amostras ao cubo ----------------------------------------------

cubo_samples_tile034018_entorno_g4_2b <- sits_get_data(
  tiles_entorno_t034018, # Cubo geral com bandas e índices
  samples = amostras_novas , # Arquivo shapefile do tile 034018
  label_attr = "classe", # Coluna que indica as classes das amostras (pontos)
  bands = c("B11", "DBSI", "NDII", "NDVI"), 
  memsize = 15, # consumo de memória
  multicores = 7, # Número de núcleos a serem usados. Quanto maior, mais rápido o processamento
  progress = TRUE) # Acompanhar carregamento

## Salvar cubo com amostras

saveRDS(cubo_samples_tile034018_entorno_g4_2b, file = "cubo_samples_tile034018_entorno_g4_2b.rds") 
cubo_samples_tile034018_entorno_g4_2b <- readRDS("cubo_samples_tile034018_entorno_g4_2b.rds")

view(cubo_samples_tile034018_entorno_g4_2b)
sits_bands(cubo_samples_tile034018_entorno_g4_2b)
sits_labels(cubo_samples_tile034018_entorno_g4_2b)