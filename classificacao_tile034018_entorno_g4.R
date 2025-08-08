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

## Salvar cubo principal

saveRDS(cubo_tile_034018_entorno, file = "cubo_tile_034018_entorno.rds") 
cubo_tile_034018_entorno <- readRDS("cubo_tile_034018_entorno.rds")

# Calcular índice NDII ----------------------------------------------------

## Calcular NDII para os tiles 035018, 034017, 033018

cube_oper_tile_ent <- sits_select(data = cubo_tile_034018_entorno,
                                  tiles = c("034018", "035018", 
                                            "034017", "033018"))

sits_bands(cube_oper_tile_ent)

tempdir_r <- "cube_operations_tile034018_entorno"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

cubo_tile034018_entorno_ndii <- sits_apply(cube_oper_tile_ent,
                                    NDII = (B08 - B11) / (B08 + B11),
                                    normalized = FALSE,
                                    output_dir = tempdir_r,
                                    progress = TRUE
)

cubo_tile034018_entorno_ndii_dbsi <- sits_apply(cubo_tile034018_entorno_ndii,
                                    DBSI = ((B11 - 1) - B03) / ((B11 - 1) + B03) - NDVI,
                                    normalized = FALSE,
                                    output_dir = tempdir_r,
                                    progress = TRUE
)

sits_bands(cubo_tile034018_entorno_ndii_dbsi)
view(cubo_tile034018_entorno_ndii_dbsi)

## cubo_tile034018_entorno_g4_2b

## Após os cálculos, será adicionado o índice NDII à pasta cube_operations_tile034018_entorno
## complementando o outro índice DBSI já calculado anteriormente para todos os tiles.

## Salvar cubo com novos índices para grupo 4

saveRDS(cubo_tile034018_entorno_ndii_dbsi, file = "cubo_tile034018_entorno_g4_2b.rds") 
cubo_tile034018_entorno_g4_2b <- readRDS("cubo_tile034018_entorno_g4_2b.rds")

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

sits_bands(cubo_samples_tile034018_entorno_g4_2b)
sits_labels(cubo_samples_tile034018_entorno_g4_2b)

# Visualizar padrões de séries temporais de cada classe -------------------

padroes_ts_samples_tile034018_entorno_g4_2b <- sits_patterns(cubo_samples_tile034018_entorno_g4_2b) # Média harmônica das séries temporais com curva suavizada
view(padroes_ts_samples_tile034018_entorno_g4_2b$time_series[[1]])

## Gráficos

p <- plot(padroes_ts_samples_tile034018_entorno_g4_2b)

library(ggplot2)

labels_personalizados <- c(
  "veg_natural" = "Vegetação Natural",
  "supressao" = "Supressão"
)

p + geom_line(linewidth = 1.2) + 
  theme_bw() +
  facet_wrap(~label, labeller = labeller(label = labels_personalizados))

p <- ggplot(padroes_ts_samples_tile034018_entorno_g4_2b$time_series, 
            aes(x = time, y = values, label = label)) +
  geom_line(linewidth = 1.2) +
  theme_bw() +
  facet_wrap(~ label, labeller = labeller(label = c(
    "veg_natural" = "Vegetação Natural",
    "supressao" = "Área Suprimida"
  )))

# Balanceamento de amostras ----------------------------------------------------------------------------------------------------------------

## A diferença entre classes cria um desequilíbrio que pode afetar negativamente o desempenho do seu
## modelo de classificação — particularmente a capacidade do modelo de identificar corretamente a 
## classe minoritária, que, neste caso, são as áreas sem vegetação.

## Modelos supervisionados tendem a favorecer a classe majoritária durante o treinamento. 
## Isso significa que:

## - O modelo pode alcançar alta acurácia geral, mas com baixa sensibilidade/recall para a classe 
## minoritária.

## - Pode subestimar a presença de áreas sem vegetação, o que pode ser crítico dependendo da sua 
## aplicação (ex: degradação, desmatamento, uso do solo etc.).

## Reduzir desigualdade no número de classes

cubo_samples_tile034018_entorno_g4_2b_bal <- sits_reduce_imbalance(
  cubo_samples_tile034018_entorno_g4_2b,
  n_samples_over = 200, 
  n_samples_under = 200) 

summary(cubo_samples_tile034018_entorno_g4_2b) # Nº de amostras não balanceadas
summary(cubo_samples_tile034018_entorno_g4_2b_bal) # Nº amostras balanceadas

# Gerar SOM ---------------------------------------------------------------

## Definir cores das classes

sits_colors_set(tibble(
  name = c("supressao", "veg_natural"),
  color = c("#bf812d", "#01665e")))

# Clustering de séries temporais - SOM

## Com balanceamento

som_cluster_tile034018_entorno_g4_2b <- sits_som_map(
  data = cubo_samples_tile034018_entorno_g4_2b_bal, # SOM feito com o nosso grupo de amostras 
  grid_xdim = 10, # Grade eixo x. Aqui é 10 x 10 para gerar 100 neurônios
  grid_ydim = 10, # Grade eixo y
  distance = "dtw", # Método de calcular a distância,
  mode = "pbatch", # Gera o mesmo mapa SOM a cada run
  rlen = 20) # Número de iterações (quantidade de vezes que o mapa é gerado)

# Gerar mapa SOM ----------------------------------------------------------

view(som_cluster_tile034018_entorno_g4_2b$data) # Tabela com coordenadas, classes, id das amostras e id neurônios

windows(width = 10, height = 6) # Abranger janela do windows para ver o gráfico

plot(som_cluster_tile034018_entorno_g4_2b, band = "DBSI")
plot(som_cluster_tile034018_entorno_g4_2b, band = "NDVI")
plot(som_cluster_tile034018_entorno_g4_2b, band = "B11")
plot(som_cluster_tile034018_entorno_g4_2b, band = "NDII")

# Seleção de neurônios no SOM --------------------------------------------------------------------------------------------------------------

samples_filt_tile034018_entorno_g4_2b <- som_cluster_tile034018_entorno_g4_2b$data[som_cluster_tile034018_entorno_g4_2b$data$id_neuron == 25, ]
view(samples_filt_tile034018_entorno_g4_2b)

# Detectar ruídos das amostras -------------------------------------------------------------------------------------------------------------

all_samples_tile034018_entorno_g4_2b <- sits_som_clean_samples(som_map = som_cluster_tile034018_entorno_g4_2b, 
                                                keep = c("clean", "analyze", "remove"))
plot(all_samples_tile034018_entorno_g4_2b)
summary(all_samples_tile034018_entorno_g4_2b) # Mesma quantidade de amostras balanceadas

# Remover amostras ruidosas ----------------------------------------------------------------------------------------------------------------

samples_clean_tile034018_entorno_g4_2b <- 
  sits_som_clean_samples(som_cluster_tile034018_entorno_g4_2b,
                         keep = c("clean", "analyze"))

view(samples_clean_tile034018_entorno_g4_2b)
view(samples_clean_tile034018_entorno_g4_2b$time_series)

plot(samples_clean_tile034018_entorno_g4_2b)

summary(samples_clean_tile034018_entorno_g4_2b) # Nova quantidade de amostras entre as classes

# Ver diferenças na quantidade de amostras antes e após filtragem --------------------------------------------------------------------------

summary(all_samples_tile034018_entorno_g4_2b)
summary(samples_clean_tile034018_entorno_g4_2b) # Manteve boa proporção entre as classes

# Gerar SOM dos dados sem ruídos -----------------------------------------------------------------------------------------------------------

som_cluster_limpo_tile034018_entorno_g4_2b <- sits_som_map(
  data = samples_clean_tile034018_entorno_g4_2b, # SOM feito com o nosso grupo de amostras 
  grid_xdim = 10, # Aqui é 10 x 10 para gerar 100 neurônios
  grid_ydim = 10,
  mode = "pbatch", # Gera o mesmo mapa SOM a cada run
  distance = "dtw", # Método para calcular a distância
  rlen = 20) # Número de iterações

windows(width = 9, height = 7)

plot(som_cluster_limpo_tile034018_entorno_g4_2b, band = "DBSI")
plot(som_cluster_limpo_tile034018_entorno_g4_2b, band = "NDVI")
plot(som_cluster_limpo_tile034018_entorno_g4_2b, band = "B11")
plot(som_cluster_limpo_tile034018_entorno_g4_2b, band = "NDII")

# Avaliar matriz de confusão das amostras antes e após limpeza -----------------------------------------------------------------------------

# Função de avaliação

avaliacao_som_tile034018_entorno_g4_2b <- sits_som_evaluate_cluster(som_cluster_tile034018_entorno_g4_2b)
avaliacao_som_limpo_tile034018_entorno_g4_2b <- sits_som_evaluate_cluster(som_cluster_limpo_tile034018_entorno_g4_2b)

# Gráficos

p1 <- plot(avaliacao_som_tile034018_entorno_g4_2b)
p1 + theme(axis.text = element_text(color = "black"),
           legend.position = "top", title = element_blank())

avaliacao_som_tile034018_entorno_g4_2b

p2 <- plot(avaliacao_som_limpo_tile034018_entorno_g4_2b)
p2 + theme(axis.text = element_text(color = "black"),
           legend.position = "top", title = element_blank())

avaliacao_som_limpo_tile034018_entorno_g4_2b

# Classificações ---------------------------------------------------------------------------------------------------------------------------

# Leitura de dados para classificação ------------------------------------------------------------------------------------------------------

cubo_tile034018_entorno_g4_2b <- readRDS("cubo_tile034018_entorno_g4_2b.rds")

# Treinar modelo Random Forest -------------------------------------------------------------------------------------------------------------

## Treinar modelo Random Forest das amostras limpas

set.seed(03024)

rf_model_tile034018_entorno_g4_2b <- sits_train(
  samples = samples_clean_tile034018_entorno_g4_2b, 
  ml_method = sits_rfor()) # Modelo Random Forest

## Gráfico com as variávies mais importantes do modelo

plot(rf_model_tile034018_entorno_g4_2b)

## Salvar modelo RF

saveRDS(rf_model_tile034018_entorno_g4_2b, "rf_model_tile034018_entorno_g4_2b.rds")
rf_model_tile034018_entorno_g4_2b <- readRDS("rf_model_tile034018_entorno_g4_2b.rds")
View(rf_model_tile034018_entorno_g4_2b)

# Produzir mapa de probabilidades de classes -----------------------------------------------------------------------------------------------

tempdir_r <- "mapa_probabilidades_tile034018_entorno"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

probs_tile034018_entorno_g4_2b <- sits_classify(
  data = cubo_tile034018_entorno_g4_2b, 
  ml_model = rf_model_tile034018_entorno_g4_2b,
  multicores = 3,
  memsize = 7,
  output_dir = tempdir_r)

## Salvar dados do cubo de probabilidades

saveRDS(probs_tile034018_entorno_g4_2b, file = "probs_tile034018_entorno_g4_2b.rds")
probs_tile034018_entorno_g4_2b <- readRDS("probs_tile034018_entorno_g4_2b.rds")

view(probs_tile034018_entorno_g4_2b$file_info)

## Visualizar mapas de probabilidades

plot(probs_tile034018_entorno_g4_2b, labels = "supressao", palette = "YlOrRd")
plot(probs_tile034018_entorno_g4_2b, labels = "veg_natural", palette = "Greens")