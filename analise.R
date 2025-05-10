# Instalar pacotes necessários (caso não estejam instalados)
install.packages(c("readxl", "dplyr", "ggplot2", "corrplot"))

# Carregar pacotes
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)

# Ler o arquivo
dados_bruto <- read_excel('Documentos/github/Trabalho-01-Aprendizado-de-M-quina-e-Reconhecimento-de-Padr-es/dados/DataSet_18_10_2023-Depurado.xlsx')

# Selecionar as colunas desejadas (as primeiras 26 colunas)
dados <- dados_bruto[, 1:26]

dados$dropout <- ifelse(dados$dropout == "True", 1, ifelse(dados$dropout == "False", 0, NA))
dados$has_scholarship <- ifelse(dados$has_scholarship == "True", 1, 
                                ifelse(dados$has_scholarship == "False", 0, NA))
dados$moved_student <- ifelse(dados$moved_student == "True", 1, ifselse(dados$moved_student == "False", 0, NA))


# Transformando dados para numéricos:
dados$dropout <- as.integer(dados$dropout)  # 0 para falso e 1 para verdadeiro
dados$has_scholarship <- as.integer(dados$has_scholarship)
dados$working_student <- as.integer(dados$working_student)
dados$moved_student <- as.integer(dados$moved_student)

# Selecionar todas as colunas numéricas:
df_numeros <- dados %>% select(where(is.numeric))

# Montar a matriz de correlação
matrix_correlacao <- cor(df_numeros)

# Mostrar a correlação com 'dropout'
dropout_correlacao <- sort(matrix_correlacao[, 'dropout'], decreasing = TRUE)
print(dropout_correlacao)

# Descrição estatística agrupada por 'dropout'
summary_by_dropout <- dados %>%
  group_by(dropout) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    # More numeric columns
    birth_date_summary = list(summary(birth_date)),
    .groups = "drop"
  )

# Mostrando a matriz de correlação:
corrplot(matrix_correlacao, method = 'color', type = 'upper', tl.col = 'black', tl.srt = 45)
title('Matriz de Correlação')