library(psych)
#Dados
clientes <- read.csv("Clientes.csv", header = TRUE, sep = ",")
clientes_columns <- names(clientes)

columns_freqs <- c("INADIMPLENTE", "SCORE", "TEM_AUXILIO", "TEM_BOLSAFAMILIA", "PF_CNT","PF_VOL","PF_ITS","PF_TPR","PF_GRV","PF_MRC","PF_TDC","PF_FRQ","PF_VLR","PF_REC","PF_ETB","PF_MOD01","PF_MOD02","PF_MOD03","PF_MOD04","OCUPACAO","IDADE")  #nolint
# ["CD_CLIENTE","INADIMPLENTE", "NM_MUN","SG_UF","SCORE","TEM_AUXILIO","TEM_BOLSAFAMILIA", #nolint
#                     "NM_REGIAO","CD_UF","CD_MUN","PF_CNT","PF_VOL","PF_ITS","PF_TPR","PF_GRV","PF_MRC", #nolint
#                     "PF_TDC","PF_FRQ","PF_VLR","PF_REC","PF_ETB","PF_MOD01","PF_MOD02","PF_MOD03","PF_MOD04", #nolint
#                     "Bancarizado","OCUPACAO","IDADE","CD_MESO"]

#1 - Sumarizações, frequências e gráficos
#1.1 - Sumarizações
summary(clientes)
freq <- function(x) {
  table(x)
}

for (i in clientes_columns) {
    if (i == "CD_CLIENTE") {
        next
    }
    png(paste0("./frequencias/",paste0(i, ".png")))
    barplot(freq(clientes[, i]), main = paste0("Frequencia de ", i)) #nolint
    dev.off()
}

correlations = c();
for (i in columns_freqs) {
  
  
  cor_x = cor(clientes["INADIMPLENTE"], clientes[i])
  if (!is.na(cor_x)) {
    
    correlations = c(correlations, cor_x)
  }
}
correlations

png("correlation.png", width=800, height=800)
plot(correlations, main = "Correlação entre variaveis e inadimplencia", xlab = "Variaveis", ylab = "Correlacao", xlim = c(0, 20), ylim = c(-1, 1)) #nolint
axis(side = 1, at = 1:21, labels = columns_freqs, las = 2) #nolint
text(x = 1:21, y = correlations, labels = round(correlations, 2), pos = 3, cex = 1) #nolint

dev.off()

png("correlations_matrix.png", width=1000, height = 1000, units = "px")
corPlot(cor(clientes[,columns_freqs]), cex=1)
dev.off()


#make hipothesis test
for(i in columns_freqs) {
  if( i == "INADIMPLENTE") {
    next
  }
  #make a t-value test
  #normalize clientes[i] from 0 to 1
  clientes_i <- clientes[i]/max(clientes[i])
  print(i)
  t_test = t.test(clientes["INADIMPLENTE"], clientes_i)  
  print(t_test)
}



# Conclusao:
# PF_REC, PF_ETB, PF_MOD03 sao as variaveis mais importantes para perdizer a inadimplencia #nolint
# Outras variaveis importantes: SCORE, TEM_AUXILIO, TEM_BOLSAFAMILIA, PF_VOL, PF_TDC, PF_MOD04, IDADE #nolint

#2 -Transformações e preparação

#Tirar valores NA
clientes <- clientes[complete.cases(clientes), ]

for (i in columns_freqs) {
  clientes[, i] <- clientes[, i]/max(clientes[, i])
}

hipoteses <- c("PF_REC", "PF_ETB", "PF_MOD03")
other <- c("SCORE", "TEM_AUXILIO", "TEM_BOLSAFAMILIA", "PF_VOL", "PF_TDC", "PF_MOD04", "IDADE") #nolint

clientes_hipoteses <- clientes[, c("INADIMPLENTE", hipoteses)]
clientes_other <- clientes[, c("INADIMPLENTE", other)]

#linear regression with hipoteses
modelo <- lm(INADIMPLENTE ~ ., data = clientes_hipoteses)
summary(modelo)
anova(modelo)

#test other
modelo_other <- lm(INADIMPLENTE ~ ., data = clientes_other)
summary(modelo_other)
anova(modelo_other) #nolint

#describe the model
png("modelo.png", width=1000, height = 1000, units = "px")
plot(modelo)
dev.off()

#describe the model other
png("modelo_other.png", width=1000, height = 1000, units = "px")
plot(modelo_other)
dev.off()

png("modelo2.png", width=1000, height = 1000, units = "px")
plot(modelo_other, which = 2)
dev.off()

png("modelo_other2.png", width=1000, height = 1000, units = "px")
plot(modelo_other, which = 2)
dev.off()

#Descricao dos inadimplentes:
#PF_REC, PF_ETB, PF_MOD03 sao as variaveis mais importantes para perdizer a inadimplencia #nolint
# O principal é o PF_MOD03, Indicador de períodos com utilização de créditos na faixa de valores 3. #nolint


