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
