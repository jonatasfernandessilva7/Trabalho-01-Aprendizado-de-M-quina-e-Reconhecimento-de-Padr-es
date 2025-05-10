install.packages("dplyr")
library(dplyr)
library(readxl)
DataSet_18_10_2023_Depurado <- read_excel("Documentos/github/Trabalho-01-Aprendizado-de-M-quina-e-Reconhecimento-de-Padr-es/dados/DataSet_18_10_2023-Depurado.xlsx")
View(DataSet_18_10_2023_Depurado)

DataSet_18_10_2023_Depurado$dropout <- as.integer(DataSet_18_10_2023_Depurado$dropout)
DataSet_18_10_2023_Depurado$has_scholarship <- as.integer(DataSet_18_10_2023_Depurado$has_scholarship)
DataSet_18_10_2023_Depurado$working_student <- as.integer(DataSet_18_10_2023_Depurado$working_student)
DataSet_18_10_2023_Depurado$moved_student <- as.integer(DataSet_18_10_2023_Depurado$moved_student)

colunas_com_numeros <- DataSet_18_10_2023_Depurado %>% select(where(is.numeric())


                                                              