library(tidyverse)
library(pdftools)
library(lubridate)

# Lendo o PDF -----------------------------------------------------------------

pdf <- pdf_text("cadastro.pdf")
pdf <- str_split_1(pdf, "\\n")
pdf


# CEP (Tarefa 3) ----------------------------------------------------------

pdf[grepl("CEP:", pdf)]

z <- pdf[grepl("CEP:", pdf)]
z <- gsub("\\w+: |\\w+ |\\w+, ", "", z)
z <- gsub("\\.|\\-", "", z)

z1 <- str_extract(z, "^\\d{5}")
z2 <- str_extract(z, "\\d{3}$")

z <- paste(z1, z2, sep = "-")
z


# Endereço (Tarefa 4) -----------------------------------------------------

end <- pdf[grepl("Endereço", pdf)]
end <- gsub("CEP.*", "", end)
end <- gsub("Endereço: *", "", end)
end

# número

numero <- str_extract(end, "\\d+")
# ifelse(is.na(numero), "SN", numero)

# Logradouro 

logradouro <- str_extract(end, "[A-Za-z]+\\s([A-Za-z]+\\s)?[A-Za-z]+\\,")
logradouro <- gsub("\\,", "", logradouro)

# Bairro

bairro <- gsub(".*,", "", end)
bairro <- gsub("[\\. ]", "", bairro)

# data frame

data.frame(Nome = x, Alias = y, CEP = z, CPF = cpf, Telefone = tel, 'Data de Nascimento' = DN, Número = numero, logradouro, bairro)

df <- data.frame(Nome = x, Alias = y, CEP = z, CPF = cpf, Telefone = tel, 'Data de Nascimento' = DN, Número = numero, logradouro, bairro)

openxlsx::write.xlsx(df, "resultado.xlsx")


# Tarefa 5 ----------------------------------------------------------------

library(read.dbc)
library(tidyverse)

tab <- read.table("RACA_COR.CNV", skip = 1, sep = ";", encoding = "latin1")

tab$V1

str_split(tab$V1, "  ")

str_extract(tab$V1, "[\\dA-Za-z,]+$")

tab$V1[10]

cod_prob <- str_extract(tab$V1[10], "1M.*") %>%
  str_split_1(",") %>% trimws("both")

desc_prob <- rep("RAÇA/COR (OUTROS INDEVIDOS)", length(cod_prob))

tab$V1

cod_bom <- tab$V1 %>%
  gsub(",", "", .) %>% 
  str_extract( "\\d+\\s*$") %>% 
  trimws("right")
cod_bom = cod_bom[1:9]

desc_bom <- tab$V1[1:9] %>%
  gsub(",", "", .) %>%
  trimws("both") %>%
  str_extract("(?<=\\d{1,2}).*(?=\\d{2})") %>%
  gsub("^\\d", "", .) %>%
  trimws("both")


df <- data.frame(descrição = c(desc_prob, desc_bom), código = c(cod_prob, cod_bom))

openxlsx::write.xlsx(df, "resultado.xlsx")


