# Criação de cubo de dados, Self-Organizing Maps (SOM) e Classificação  --------------------------------------------------------------------
# Teste de Amostras  -----------------------------------------------------------------------------------------------------------------------
# Tiles 034018, 035018, 034017, 033018 -----------------------------------------------------------------------------------------------------
# Conjunto de classes: grupo E --------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tibble) # Pacote para visualizar tabelas
library(sits) # Pacote para análises de séries temporais de imagens de satélite
library(sitsdata) # Pacote para obter conjunto de dados de amostras do sits
library(kohonen) # Pacote para plotar o mapa SOM
library(randomForestExplainer) # Pacote para treinar modelo de classificação
library(luz) # Pacote para facilitar criação, treino e avaliação de modelos no Torch
library(torch) # Pacote para criar modelos deep learning e treinar redes neurais
torch::install_torch()
library(tidyverse) # Pacote para manipulação de tabelas e gráficos
library(terra) # Pacote para manipular dados espaciais (imagens raster, dados de satélite)
library(raster) # Pacote mais antigo para manipulação de dados raster
library(sf) # Pacote para manipulação de dados vetoriais (pontos, linhas, polígonos)

# Ler cubo com todos os tiles ---------------------------------------------

cubo_tile_034018_entorno <- readRDS("cubo_tile_034018_entorno.rds")

sits_bands(cubo_tile_034018_entorno)

# Ler arquivos .shp -------------------------------------------------------

amostras_t034018 <- sf::read_sf("Tile_034018_amostras_classificacao123_treinadas_manual_classes_A2.shp")
amostras_novas <- sf::read_sf("Novas_Amostras_Tiles33018_34017_35018.shp")
amostras_novas_je <- sf::read_sf("novas_amostras_jeanne.shp")

view(amostras_t034018)
view(amostras_novas)
view(amostras_novas_je)

class(amostras_t034018)

unique(is.na(amostras_novas))

# Organizar arquivos de amostras com tidyverse ----------------------------

unique(amostras_t034018$Clss_gr)

amostras_t034018 <- amostras_t034018 |>
  rename(c(id = fid, classe = Clss_gr)) |>
  dplyr::select(- clss_ms, - id, -x, -y) |>
  filter(!classe %in% c("supveg_veg", "queimada"))

view(amostras_t034018)
unique(amostras_t034018$classe)

amostras_novas <- amostras_novas |>
  dplyr::select(- id, -x, -y) |>
  filter(!classe %in% c("supveg_veg", "queimada"))

view(amostras_novas)
unique(amostras_novas$classe)

amostras_novas_je <- amostras_novas_je |>
  dplyr::select(- id) 
  
view(amostras_novas_je)

# Unir arquivos com amostras ----------------------------------------------

st_crs(amostras_t034018)
st_crs(amostras_novas)
st_crs(amostras_novas_je)

crs_alinhada <- st_transform(amostras_novas_je, st_crs(amostras_novas))

amostras_t034018_novas_ams_entorno <- bind_rows(amostras_t034018, amostras_novas, crs_alinhada)

view(amostras_t034018_novas_ams_entorno)
class(amostras_t034018_novas_ams_entorno)

# Retirar amostras NAs

amostras_t034018_novas_ams_entorno <- amostras_t034018_novas_ams_entorno |>
  drop_na()

amostras_t034018_novas_ams_entorno <- amostras_t034018_novas_ams_entorno |>
  filter(classe != "veg_natual")

# Exportar para um novo shapefile
st_write(amostras_t034018_novas_ams_entorno, "amostras_t034018_novas_ams_entorno.shp")
unique(is.na(amostras_t034018_novas_ams_entorno))

view(amostras_t034018_novas_ams_entorno)
unique(amostras_t034018_novas_ams_entorno$classe)

amostras_t034018_novas_ams_entorno <- read_sf("amostras_t034018_novas_ams_entorno.shp")

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

saveRDS(cubo_samples_tile034018_entorno_2e, file = "cubo_samples_tile034018_entorno_2e.rds") 
cubo_samples_tile034018_entorno_2e <- readRDS("cubo_samples_tile034018_entorno_2e.rds")

view(cubo_samples_tile034018_entorno_2e)
sits_bands(cubo_samples_tile034018_entorno_2e)
sits_labels(cubo_samples_tile034018_entorno_2e)

# Visualizar padrões de séries temporais de cada classe ---------------------------------------------------------------------------------------------------



