# Criação de cubo de dados, Self-Organizing Maps (SOM) e Classificação  --------------------------------------------------------------------
# Teste de Amostras  -----------------------------------------------------------------------------------------------------------------------
# Tiles 034018, 035018, 034017, 033018 -----------------------------------------------------------------------------------------------------
# Conjunto de classes: grupo B --------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

## Pacotes SITS

library(tibble) # Pacote para visualizar tabelas
library(sits) # Pacote para análises de séries temporais de imagens de satélite
#library(sitsdata) # Pacote para obter conjunto de dados de amostras
library(kohonen) # Pacote para plotar o mapa SOM
library(randomForestExplainer) # Gerar modelo Random Forest

## Pacotes para leitura da máscara

library(tidyverse)
library(st)
library(sf)
library(terra)

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

view(cubo_samples_tile034018_entorno_g4_2b)
sits_bands(cubo_samples_tile034018_entorno_g4_2b)
sits_labels(cubo_samples_tile034018_entorno_g4_2b)

# Visualizar padrões de séries temporais de cada classe -------------------

### Filtrar por tiles

padroes_ts_samples_tile034018_entorno_g4_2b <- sits_patterns(cubo_samples_tile034018_entorno_g4_2b) # Média harmônica das séries temporais com curva suavizada
view(padroes_ts_samples_tile034018_entorno_g4_2b$time_series[[1]])

## Gráficos

p <- plot(padroes_ts_samples_tile034018_entorno_g4_2b)

labels_personalizados <- c(
  "veg_natural" = "Vegetação Natural",
  "supressao" = "Supressão"
)

# Passo 1: garantir que a tabela interna seja desaninhada
df_exp <- padroes_ts_samples_tile034018_entorno_g4_2b %>%
  tidyr::unnest(time_series)  # transforma lista de tibbles em linhas reais

# Conferir se virou tibble plano
print(df_exp)

# Passo 2: reorganizar bandas em uma coluna
df_long <- df_exp %>%
  #select(label, Index, B11, DBSI, NDII, NDVI) %>%
  pivot_longer(
    cols = c(B11, DBSI, NDII, NDVI),
    names_to = "indice",
    values_to = "valor"
  )

p <- ggplot(df_long, aes(x = Index, y = valor, color = indice)) +
  geom_line(linewidth = 1.2) +
  theme_bw() +
  facet_wrap(~ label, labeller = labeller(label = c(
    "veg_natural" = "Vegetação Natural",
    "supressao" = "Supressão"
  ))) +
  labs(x = "Data", y = "Valor", color = "Bandas") +
  theme(axis.text = element_text(color = "black"),
        axis.text.x = element_text(hjust = 1, angle = 60))

p

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
p_ruid <- plot(all_samples_tile034018_entorno_g4_2b)
summary(all_samples_tile034018_entorno_g4_2b) # Mesma quantidade de amostras balanceadas

p_ruid + theme(axis.text = element_text(color = "black")) +
  scale_y_discrete(labels = c("veg_natural" = "Vegetação Natural",
                              "supressao" = "Supressão"))

# Remover amostras ruidosas ----------------------------------------------------------------------------------------------------------------

samples_clean_tile034018_entorno_g4_2b <- 
  sits_som_clean_samples(som_cluster_tile034018_entorno_g4_2b,
                         keep = c("clean", "analyze"))

view(samples_clean_tile034018_entorno_g4_2b)
view(samples_clean_tile034018_entorno_g4_2b$time_series)

p_clean <- plot(samples_clean_tile034018_entorno_g4_2b)

p_clean + theme(axis.text = element_text(color = "black")) +
  scale_y_discrete(labels = c("veg_natural" = "Vegetação Natural",
                              "supressao" = "Supressão"))

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
           legend.position = "top", title = element_blank()) +
  scale_y_discrete(labels = c("veg_natural" = "Vegetação Natural",
                              "supressao" = "Supressão"))

avaliacao_som_tile034018_entorno_g4_2b

p2 <- plot(avaliacao_som_limpo_tile034018_entorno_g4_2b)
p2 + theme(axis.text = element_text(color = "black"),
           legend.position = "top", title = element_blank()) +
  scale_y_discrete(labels = c("veg_natural" = "Vegetação Natural",
                              "supressao" = "Supressão"))

avaliacao_som_limpo_tile034018_entorno_g4_2b

# Classificações ---------------------------------------------------------------------------------------------------------------------------

# Leitura de dados para classificação ------------------------------------------------------------------------------------------------------

cubo_tile034018_entorno_g4_2b <- readRDS("cubo_tile034018_entorno_g4_2b.rds")

view(cubo_tile034018_entorno_g4_2b)

c_033018 <- cubo_tile034018_entorno_g4_2b[1, ]

view(c_033018)

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
  multicores = 7,
  memsize = 15,
  output_dir = tempdir_r)

## Salvar dados do cubo de probabilidades

saveRDS(probs_tile034018_entorno_g4_2b, file = "probs_tile034018_entorno_g4_2b.rds")
probs_tile034018_entorno_g4_2b <- readRDS("probs_tile034018_entorno_g4_2b.rds")

view(probs_tile034018_entorno_g4_2b$file_info)

### É exibido apenas o primeiro tile da lista.

# Unir tiles vizinhos com tile central ------------------------------------

tempdir_r <- "mosaico_probabilidades_tile034018_entorno"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

mosaico_proba <- sits_mosaic(probs_tile034018_entorno_g4_2b,
  output_dir = "mosaico_probabilidades_tile034018_entorno",
  multicores = 7,
  progress   = TRUE, 
)

view(mosaico_proba)

plot(mosaico_proba)

## Salvar dados do cubo do mosaico de probabilidades

saveRDS(mosaico_proba, file = "mosaico_proba_tile034018_entorno.rds")
mosaico_proba_tile034018_entorno <- readRDS("mosaico_proba_tile034018_entorno.rds")

# Suavização do mapa de probabilidades -----------------------------------------------------------------------------------------------------

tempdir_r <- "mosaico_prob_suav_tile034018_entorno"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

smooth_tile034018_entorno <- sits_smooth(
  cube = mosaico_proba,
  multicores = 7,
  memsize = 15,
  output_dir = "mosaico_prob_suav_tile034018_entorno"
)

p_sm <- plot(smooth_tile034018_entorno)

p_sm + tm_raster(col = "probs",
                   palette = "Greens",
                   title = "")

## Salvar dados do cubo suavizado

saveRDS(smooth_tile034018_entorno, file = "smooth_tile034018_entorno.rds")
smooth_tile034018_entorno <- readRDS("smooth_tile034018_entorno.rds")

# Rotulando o cubo de probabilidades - Classificações finais de amostras -------------------------------------------------------------------

tempdir_r <- "mosaico_classificado_tile034018_entorno"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

map_class_tile034018_entorno <- sits_label_classification(
  cube = smooth_tile034018_entorno, 
  output_dir = "mosaico_classificado_tile034018_entorno", 
  memsize = 15,
  multicores = 7, 
)

## Salvar dados do cubo suavizado

saveRDS(map_class_tile034018_entorno, file = "map_class_tile034018_entorno.rds")
map_class_tile034018_entorno <- readRDS("map_class_tile034018_entorno.rds")

view(map_class_tile034018_entorno)

## Definir cores das classes

sits_colors_set(tibble(
  name = c("supressao", "veg_natural"),
  color = c("#dfc27d", "#003c30")))

plot(map_class_tile034018_entorno)
class(map_class_tile034018_entorno)

## Salvar dados do cubo classificado

saveRDS(map_class_tile034018_entorno, file = "map_class_tile034018_entorno.rds")
map_class_tile034018_entorno <- readRDS("map_class_tile034018_entorno.rds")

# Adicionar máscara -------------------------------------------------------------------------------------------------------------------------------------

# Carregar mapa classificado e shapefile da máscara

library(terra) # para ler mapas .tif
library(sf) # para ler shapefiles

######### Importante estabelecer diretório antes

# Carregar o seu mapa classificado (já no mosaico final)

mapa_class_final <- rast("SENTINEL-2_MSI_MOSAIC_2020-01-01_2020-12-18_class_v1.tif")

plot(mapa_class_final)
class(mapa_class_final)

unique(values(mapa_class_final)) # Verificar pixels 
plot(is.na(mapa_class_final), main = "Valores NA") # não tem máscara, NA está fora de todos os tiles

# Carregar o seu shapefile de máscara

mascara_shp <- st_read("mask_rec_2019_34018_entornos_dissolv.shp")

class(mascara_shp)

plot(mascara_shp)

# Adicionar máscara usando tmap (estático) ----------------------------------------------------------------------------------------------------

library(tmap)

# Garantir mesmo CRS e resolução

mascara_raster <- rasterize(
  st_transform(mascara_shp, crs(mapa_class_final)),
  mapa_class_final,
  field = 1
)

# Definir a máscara NA como valor 3

mapa_completo <- mapa_class_final
mapa_completo[!is.na(mascara_raster)] <- 3  # 3 = máscara PRODES

tm_shape(mapa_completo) +
  tm_raster(
    palette = c("#a50026", "#006837", "gray10"),
    labels = c("Desmatamento 2020", "Vegetação natural", "Máscara PRODES de desmatamento 2000-2019"),
    title = "Classes"
  ) +
  tm_layout(legend.outside = TRUE)

# Adicionar máscara com pacote terra ------------------------------------------------------------------------------------------------------------------------

# Aplicar a máscara ao mapa classificado

## Conferir CRS:

crs(mapa_class_final)
crs(mascara_shp)

## Reprojetar a máscara para o CRS do raster

mascara_shp <- st_transform(mascara_shp, crs(mapa_class_final))

# Esta função irá atribuir NA a todos os pixels que estão dentro da geometria da máscara

mapa_com_mascara <- mask(mapa_class_final, mascara_shp, inverse = TRUE)

# Agora, o mapa 'mapa_com_mascara' contém os valores classificados
# apenas para as áreas fora da sua máscara.
# As áreas da máscara terão o valor NA.

plot(mapa_com_mascara)

# Definir as cores para as classes

mapa_plot <- mapa_com_mascara

mapa_plot[is.na(mapa_plot)] <- 3  # Classe 3 = máscara

cores_com_mascara <- c("#a50026", "#006837", "gray10")

plot(mapa_plot, col = cores_com_mascara, 
                legend = FALSE, 
                axes = FALSE, box = FALSE)

# Adicionar máscara com Reclassificação do SITS ---------------------------

# Configurações e pacotes ------------------------------------------------------------------------------------------------------------------

library(torch)
torch::install_torch()
library(luz)
library(sits)
#library(sitsdata)
library(tibble)

# Estabelecer diretório dos mapas classificado e PRODES

tempdir_r <- "map_classificado_2B"
dir.create(tempdir_r, showWarnings = FALSE)
getwd() # O diretório deve apresentar a pasta acima criada

# Gerar cubo do mapa classificado

cubo_class_2B <- sits_cube(
  source = "BDC",
  collection = "SENTINEL-2-16D",
  data_dir = tempdir_r, # A imagem classificada deve estar nesta pasta
  parse_info = c("satellite", "sensor", 
                 "tile", "start_date", "end_date",
                 "band", "version"),
  bands = "class",
  labels = c("1" = "supressao", # Definir os pixels da imagem
             "2" = "veg_natural"))

view(cubo_class_2B)

## Definir cores das classes

sits_colors_set(tibble(
  name = c("supressao", "veg_natural"),
  color = c("#bf812d", "#01665e")))

# Visualizar mapa 

plot(cubo_class_2B, 
     legend_text_size = 0.7, legend_position = "outside")

class(cubo_class_2B)

# Mapa equivalente no ggplot

library(ggplot2)

# Vamos supor que cubo_class_2B tem colunas: x, y e class
names(cubo_class_2B)
# Ajuste se o nome da coluna da classe for diferente:
# names(cubo_class_2B)[3] <- "class"

library(stars)
library(ggplot2)

# Converter para stars (se o pacote cubeR ou equivalentes suportar)
# Extrair cubo como data.frame com coordenadas e valores
cubo_df <- as.data.frame(cubo_class_2B, cells = TRUE)
view(cubo_df)
# Veja os nomes das colunas para achar a variável de classe
names(cubo_df)

# Criar mapa no ggplot
ggplot() +
  geom_raster(data = cubo_class_2B, aes(fill = factor(labels))) +
  scale_fill_viridis_d(name = "Classe") +
  coord_equal() +
  theme_minimal() +
  geom_sf(data = mascara_shp, fill = NA, 
          color = "black", size = 0.3)

