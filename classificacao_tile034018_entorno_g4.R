# Criação de cubo de dados, Self-Organizing Maps (SOM) e Classificação  --------------------------------------------------------------------
# Teste de Amostras  -----------------------------------------------------------------------------------------------------------------------
# Tiles 034018, 035018, 034017, 033018 -----------------------------------------------------------------------------------------------------
# Conjunto de classes: grupo B --------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tibble) # Pacote para visualizar tabelas
library(sits) # Pacote para análises de séries temporais de imagens de satélite
#library(sitsdata) # Pacote para obter conjunto de dados de amostras
library(kohonen) # Pacote para plotar o mapa SOM
library(randomForestExplainer) # Gerar modelo Random Forest

# Estabelecer diretório de trabalho  -------------------------------------------------------------------------------------------------------

## Estabelecer um diretório de trabalho (pasta do computador com seus arquivos)
### 1ª Opção:
setwd("C:/caminho/da/sua/pasta")  

### 2ª opção (manual):
# Session > Set Working Directory > Choose Directory

## Conferir o caminho do diretório de trabalho definido:
getwd() 

# Criar cubo de dados - Sentinel 2 ---------------------------------------------------------------------------------------------------------

cubo_tile_034018_entorno <- sits_cube(
  source     = "BDC", # Fonte dos cubos de dados
  collection = "SENTINEL-2-16D", # Coleção de imagens
  tiles      = c("034018", "035018", "034017", "033018"), # Região definida pelo Tile
  start_date = "2020-01-01", # Data inicial 
  end_date   = "2020-12-31") # Data final (período de 1 ano)

sits_bands(cubo_tile_034018_entorno)
sits_timeline(cubo_tile_034018_entorno)

view(cubo_tile_034018_entorno)
view(cubo_tile_034018_entorno$file_info)

# Calcular índice NDII ----------------------------------------------------

## Calcular NDII para os tiles 035018, 034017, 033018

cube_oper_tile_ent <- sits_select(data = cubo_tile_034018_entorno,
                                  tiles = c("035018", "034017", "033018"))

sits_bands(cube_oper_tile_ent)

tempdir_r <- "cube_operations_tile034018_entorno"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

cubo_tile034018_entorno_ndii <- sits_apply(cube_oper_tile_ent,
                                    NDII = (B08 - B11) / (B08 + B11),
                                    normalized = FALSE,
                                    output_dir = tempdir_r,
                                    progress = TRUE
)

sits_bands(cubo_tile034018_entorno_ndii)

## cubo_tile034018_entorno_g4_2b

## Após os cálculos, será adicionado o índice NDII à pasta cube_operations_tile034018_entorno
## complementando o outro índice DBSI já calculado anteriormente para todos os tiles.

# Adicionar amostras ao cubo ----------------------------------------------

cubo_samples_tile034018_entorno_g4_2b <- sits_get_data(
  cubo_tile034018_entorno_g4_2b, # Cubo geral com bandas e índices
  samples = "Tile_034018_amostras_classificacao123_treinadas_manual_classes_B.shp", # Arquivo shapefile do tile 034018
  label_attr = "classe_b", # Coluna que indica as classes das amostras (pontos)
  bands = c("B11", "DBSI", "NDII", "NDVI"), 
  memsize = 7, # consumo de memória
  multicores = 2, # Número de núcleos a serem usados. Quanto maior, mais rápido o processamento
  progress = TRUE) # Acompanhar carregamento

## Salvar cubo com amostras

saveRDS(cubo_samples_tile034018_entorno_g4_2b, file = "cubo_samples_tile034018_entorno_g4_2b.rds") 
cubo_samples_tile034018_entorno_g4_2b <- readRDS("cubo_samples_tile034018_entorno_g4_2b")

sits_bands(cubo_samples_tile034018_entorno_g4_2b)
sits_labels(cubo_samples_tile034018_entorno_g4_2b)

