# 1) Leitura de Bibliotecas
library(st)
library(sf)
library(sits)
library(rstac)
library(terra)
library(magrittr)
library(tibble)
library(kohonen)
library(RColorBrewer)
library(ggplot2)
library(sysfonts)
library(dplyr)

## Alterar o arquivo do cubo com o caminho e barras para a direita
bdc_cube_caso4 <- readRDS("caminho/da/pasta/cubo_caatinga_caso3.rds")
sits_bands(bdc_cube_caso4)

## 1) Gerar a classificação das classes do SOM com as amostras para o Caso 4
## Mudar o caminho das amostras
amostras_caso4 <- sits_get_data(bdc_cube_caso4, ## Cubo Tile 034018
                                samples = "XXXXXXXXXXXXXXXXX", ## Checar o caminho de onde vão ser puxadas as amostras
                                label_attr = "CLASSE",                         ## Coluna na qual estão as classes das amostras para este shapefile específico ** Caso use outro arquivo verificar (pode ser no QGIS) o nome da coluna
                                multicores = 4,                                           ## Número de processamentos ao mesmo tempo
                                bands = c("DBSI", "NDVI", "B08", "B11"), ## Adicionando as bandas desejadas para a classificação
                                progress = TRUE
)

## Mostrando um resumo das amostras utilizadas
summary(amostras_caso4)

## Definir as cores ANTES de rodar o SOM
sits_colors_set(tibble(name = c("f_aflor_rocha", "veg_nat_ag","veg_nat", "queimada", "reservatorio", "supveg_solo"), 
                       
                       color = c("#D691C1", "#4B644B", "#27AE60", "#F1C40F", "#2980B9","#C0392B"))
)

# 2) Aplicando a classificação das classes do cubo de dados usando o método do SOM para o Caso 4
som_caso4 <- sits_som_map(amostras_caso4,   ## Série temporal das amostras no Cubo do Tile 034018
                          grid_xdim = 10,   ## Tamanho da grade na dimensão X do SOM (10 x 10 = 100 neurônios)
                          grid_ydim = 10,   ## Tamanho da grade na dimensão Y do SOM 
                          alpha = 1,        ## Parâmetro de taxa de aprendizagem inicial
                          rlen = 20,        ## Número de iterações da classificação 
                          distance = "dtw"  ## Parâmetro de medida de separação entre as amostras
)

## Plotando o resultado obtido (Opcional)
plot(som_caso4,
     band = c("DBSI", "NDVI", "B08", "B11"),
     legend_position = "outside",
     labels = labels,
     scale = 1.0,
)

## Produzindo um 'tibble' com um resumo dos rótulos misturados e mostrando os resultados
avaliacao_som_caso4 <- sits_som_evaluate_cluster(som_caso4)
avaliacao_som_caso4 ## Resultado no CONSOLE

# Plot da confusão entre os clusters do Caso 4
plot(avaliacao_som_caso4)

# 3) Detecção de amostras ruidosas usando o SOM
all_samples_4 <- sits_som_clean_samples(som_map = som_caso4,
                                        prior_threshold = 0.5,
                                        posterior_threshold = 0.5,
                                        keep = c("clean", "analyze", "remove") 
                                        ## tem um parâmetro adicional (keep),
                                        ## que indica quais amostras devem ser mantidas
                                        ## no conjunto com base em suas probabilidades anteriores e posteriores.
)

## Plotando a distribuição da amostra original com base na avaliação (Opcional)
plot(all_samples_4)

## 4) Removendo as amostras ruidosas para melhorar a qualidade do conjunto de treinamento
novas_amostras_caso4 <- sits_som_clean_samples(som_map = som_caso4,
                                               prior_threshold = 0.5,
                                               posterior_threshold = 0.5,
                                               keep = c("clean", "analyze")
)

## Resumo Amostras (Opcional)
summary(novas_amostras_caso4) ## Resumo amostras 'limpas'
summary(all_samples_4)          ## Resumo amostras originais

## Plotando a distribuição das amostras 'limpas' com base na avaliação (Opcional)
plot(novas_amostras_caso4)

# 5) Produzindo um novo mapa SOM com as amostras limpas
som_caso4_clean <- sits_som_map(data = novas_amostras_caso4,
                                grid_xdim = 10,
                                grid_ydim = 10,
                                alpha = 1.0,
                                rlen = 20,
                                distance = "dtw"
)

## Avaliando a mistura nos novos clusters do SOM
avaliacao_som_caso4_clean <- sits_som_evaluate_cluster(som_caso4_clean)
avaliacao_som_caso4_clean ## Resultado no CONSOLE

# Plot das informações da mistura
plot(avaliacao_som_caso4_clean)

# 6) Etapas Random Forest
set.seed(11) #para garantir a reprodutibilidade
rf_model <- sits_train(samples   = novas_amostras_caso4,
                       ml_method = sits_rfor()
)

# Importância das variáveis
plot(rf_model)

## Mudar o caminho da máscara
mascara_34018 = sf::read_sf("caminho/da/pasta/rec_34018_mask.shp")

## Checando se os polígonos possuem geometrias válidas

## Joga na variável "polygons" as geometrias da máscara
polygons = sf::st_cast(mascara_34018, "POLYGON")

## Checando se as geometrias são válidas
is_valid = sf::st_is_valid(polygons)

## Joga na variável polygons tudo o que é valido
polygons = polygons[is_valid, ]

## validação
polygons = sf::st_make_valid(mascara_34018)

## Plot da máscara
plot(mascara_34018)

# Classifica o cubo com o modelo treinado
grupo4_probs <- sits_classify(data = bdc_cube_caso4,
                              ml_model   = rf_model,
                              multicores = 4,
                              memsize    = 8,
                              progress = TRUE,
                              exclusion_mask = mascara_34018,
                              output_dir = "XXXXXXXXX", ## Alterar o caminho pra onde deseja as saídas
                              version = "rf_caso4"
)

# Visualização da probabilidade de uma classe
plot(grupo4_probs, labels = "supveg_solo", palette = "YlOrBr")


# Suavização espacial (remove efeito tipo "sal e pimenta" do mapa)
grupo4_smooth <- sits_smooth(cube = grupo4_probs,
                             multicores = 4,
                             memsize    = 8,
                             progress = TRUE,
                             output_dir = "XXXXXXXXX", ## Alterar o caminho pra onde deseja as saídas,
                             version = "rf_caso4"
)

plot(grupo4_smooth, labels = "supveg_solo", palette = "YlOrBr")

# Classificação final com rótulos
grupo4_map <- sits_label_classification(cube = grupo4_smooth,
                                        multicores = 4,
                                        memsize = 8,
                                        output_dir = "XXXXXXXXX", ## Alterar o caminho pra onde deseja as saídas
                                        version = "rf_caso4"
)

plot(grupo4_map)

# Avaliação com validação cruzada (k-fold)
set.seed(11) #garantir reprodutibilidade
rfor_validacao4 <- sits_kfold_validate(samples    = novas_amostras_caso4,
                                       folds      = 5, #número divisões do grupo, nesse caso em 5 conjuntos para treino e teste
                                       ml_method  = sits_rfor(),
                                       multicores = 4
)

rfor_validacao4 #aqui você vai observar os valores de acurácia (geral) e também por classes

incerteza_grupo4 <- sits_uncertainty(cube = grupo4_smooth, #aqui é usado o arquivo do cubo de probabilidades, resultado da função sits_classify()
                                     type = "margin",
                                     output_dir = "XXXXXXXXX", ## Alterar o caminho pra onde deseja as saídas
                                     multicores = 4,
                                     memsize = 8,
                                     progress = TRUE,
                                     version = "rf_caso4"
)

#Plota o mapa de incertezas. Quanto mais próximo de 1, maior a incerteza
plot(incerteza_grupo4)