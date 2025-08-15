# Criação de cubo de dados, Self-Organizing Maps (SOM) e Classificação  --------------------------------------------------------------------
# Teste de Amostras  -----------------------------------------------------------------------------------------------------------------------
# Tiles 034018, 035018, 034017, 033018 -----------------------------------------------------------------------------------------------------
# Conjunto de classes: grupo B --------------------------------------------
# Teste de acurácia com novas amostras em todos os tiles ------------------
# Teste usado após classificação ------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tibble) # Pacote para visualizar tabelas
library(sits) # Pacote para análises de séries temporais de imagens de satélite
#library(sitsdata) # Pacote para obter conjunto de dados de amostras
library(kohonen) # Pacote para plotar o mapa SOM
library(randomForestExplainer)
library(torch)
torch::install_torch()
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

unique(is.na(amostras_novas))


