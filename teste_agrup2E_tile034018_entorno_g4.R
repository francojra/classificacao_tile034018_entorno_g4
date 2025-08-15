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
library(luz)
library(torch)
torch::install_torch()
library(tidyverse)
library(terra)
library(raster)
library(sf)

# Ler cubo com todos os tiles ---------------------------------------------

cubo_tile_034018_entorno <- readRDS("cubo_tile_034018_entorno.rds")

sits_bands(cubo_tile_034018_entorno)

# Ler arquivos .shp -------------------------------------------------------

amostras_t034018 <- sf::read_sf("Tile_034018_amostras_classificacao123_treinadas_manual_classes_A2.shp")
amostras_novas <- sf::read_sf("Novas_Amostras_Tiles33018_34017_35018.shp")

view(amostras_t034018)
view(amostras_novas)
class(amostras_t034018)

unique(is.na(amostras_novas))

# Organizar arquivos de amostras com tidyverse ----------------------------

amostras_t034018 <- amostras_t034018 |>
  rename(c(id = fid, classe = Clss_gr)) |>
  dplyr::select(-clss_ms) 

view(amostras_t034018)
  
# Unir arquivos com amostras ----------------------------------------------

amostras_t034018_novas_ams_entorno <- bind_rows(amostras_t034018, amostras_novas)

view(amostras_t034018_novas_ams_entorno)
class(amostras_t034018_novas_ams_entorno)

# Exportar para um novo shapefile
st_write(amostras_t034018_novas_ams_entorno, "amostras_t034018_novas_ams_entorno.shp")

amostras_t034018_novas_ams_entorno <- amostras_t034018_novas_ams_entorno |>
  drop_na()

# Adicionar amostras ao cubo ----------------------------------------------

cubo_samples_tile034018_entorno_2e <- sits_get_data(
  cubo_tile_034018_entorno, # Cubo geral com bandas e índices
  samples = "amostras_t034018_novas_ams_entorno.shp", # Arquivo shapefile do tile 034018
  label_attr = "classe", # Coluna que indica as classes das amostras (pontos)
  bands = c("B01",   "B02",   "B03",   "B04",   "B05",   
            "B06",   "B07",   "B08",   "B09",   "B11",   
            "B12", "B8A"), 
  memsize = 15, # consumo de memória
  multicores = 7, # Número de núcleos a serem usados. Quanto maior, mais rápido o processamento
  progress = TRUE) # Acompanhar carregamento

## Salvar cubo com amostras

saveRDS(cubo_samples_tile034018_entorno_g4_2b, file = "cubo_samples_tile034018_entorno_g4_2b.rds") 
cubo_samples_tile034018_entorno_g4_2b <- readRDS("cubo_samples_tile034018_entorno_g4_2b.rds")

view(cubo_samples_tile034018_entorno_g4_2b)
sits_bands(cubo_samples_tile034018_entorno_g4_2b)
sits_labels(cubo_samples_tile034018_entorno_g4_2b)

