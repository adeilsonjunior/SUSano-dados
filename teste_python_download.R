# Teste leitura de arquivo dbc baixado via Python
library(read.dbc)
dbc_file = "RDAC2201.dbc"
file_df <- read.dbc(file = dbc_file)