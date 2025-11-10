rm(list=ls())
library(rstudioapi)
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Caricamento dati
data <- read_excel("customer_segmentation_reduced.xlsx")
# Converte in formato Date
data$Day <- dmy(data$Day)
summary(data)

######## DATA QUALITY ###########
# somma per colonna tutti gli na di data:
colSums(is.na(data))

# Vediamo se ci sono righe duplicate
duplicated(data)
# andiamo a vedere quale riga è duplicata
which(duplicated(data))
# Non ci sono righe duplicate

# Rimuoviamo le righe 29,30,59,189 perchè il valore di Discounts è più del doppio di Net Sales
data <- data[-c(29,30,59,189),]

# BOXPLOT --- vediamo se ci sono outliers
dev.new()
par(mfrow=c(1,3))
boxplot(data$`Gross Sales`, col="lightgreen", main="Gross Sales")
boxplot(data$Discounts, col="lightgreen", main="Discounts")
boxplot(data$Returns, col="lightgreen", main="Returns") # non verranno rimossi i record che sembrano outliers perchè in realtà non si tratta di outliers ma di valori eccezionali
dev.new()
par(mfrow=c(1,3))
boxplot(data$Taxes, col="lightgreen", main="Taxes")
boxplot(data$`Net Sales`, col="lightgreen", main="Net Sales")
boxplot(data$`Total Sales`, col="lightgreen", main="Total Sales")

# boxplot(data$`Net Quantity`, col="lightgreen", main="Net Quantity") non necessario perchè non ci sono outlier
# boxplot(data$`Returned Item Quantity`, col="lightgreen", main="Returned Item Quantity") non necessario perchè non ci sono outlier
# boxplot(data$`Ordered Item Quantity`, col="lightgreen", main="Ordered Item Quantity") non necessario perchè non ci sono outlier

#### TOGLIAMO GLI OUTLIERS

# Calcola l'IQR per la variabile Gross Sales
Q1 <- quantile(data$`Gross Sales`, 0.25)
Q3 <- quantile(data$`Gross Sales`, 0.75)
IQR_value <- Q3 - Q1

# Identifica gli outlier
lower_bound <- Q1 - 25 * IQR_value
upper_bound <- Q3 + 25 * IQR_value

# Filtra il dataset rimuovendo gli outlier
new_data <- subset(data, `Gross Sales`> lower_bound & `Gross Sales` < upper_bound)
View(new_data) # sono stati tolti 6 outliers
boxplot(new_data$`Gross Sales`, col="lightgreen", main="Gross Sales")


# Calcola l'IQR per la variabile Discounts
Q1 <- quantile(new_data$Discounts, 0.10)
Q3 <- quantile(new_data$Discounts, 0.90)
# abbiamo impostato 0.10 e 0.90 perchè considerano primo e terzo quartile avremmo avuto invece un IQR nullo
IQR_value <- Q3 - Q1

# Identifica gli outlier
lower_bound <- Q1 - 5 * IQR_value
upper_bound <- Q3 + 5* IQR_value

# Filtra il dataset rimuovendo gli outlier
new_data2 <- subset(new_data, Discounts> lower_bound & Discounts < upper_bound)
View(new_data2) # è stato tolto un outlier
boxplot(new_data2$Discounts, col="lightgreen", main="Discounts")


# Calcola l'IQR per la variabile Taxes
Q1 <- quantile(new_data2$Taxes, 0.25)
Q3 <- quantile(new_data2$Taxes, 0.75)
IQR_value <- Q3 - Q1

# Identifica gli outlier
lower_bound <- Q1 - 30 * IQR_value
upper_bound <- Q3 + 30 * IQR_value

# Filtra il dataset rimuovendo gli outlier
new_data3 <- subset(new_data2, Taxes> lower_bound & Taxes < upper_bound)
View(new_data3) # la rimozione degli outlier per Taxes è già stata fatta rimuovendo outliers per altre features
boxplot(new_data3$Taxes, col="lightgreen", main="Taxes")


# Calcola l'IQR per la variabile Net Sales
Q1 <- quantile(new_data3$`Net Sales`, 0.25)
Q3 <- quantile(new_data3$`Net Sales`, 0.75)
IQR_value <- Q3 - Q1

# Identifica gli outlier
lower_bound <- Q1 - 25 * IQR_value
upper_bound <- Q3 + 25 * IQR_value

# Filtra il dataset rimuovendo gli outlier
new_data4 <- subset(new_data3, `Net Sales` > lower_bound & `Net Sales` < upper_bound)
View(new_data4) # la rimozione degli outlier per Net Sales è già stata fatta rimuovendo outliers per altre features
boxplot(new_data4$`Net Sales`, col="lightgreen", main="Net Sales")

# Non rimuoviamo record analizzando i valori della variabile Total Sales perchè non li consideriamo outliers

##### ANALISI TREND VENDITE PER MESE ########
library(dplyr)
# Creare una colonna con il mese
df <- new_data4 %>%
  mutate(Month = format(Day, "%Y-%m"))  # Estrae Anno-Mese

# Aggregare le vendite totali per mese
df_monthly <- df %>%
  group_by(Month) %>%
  summarise(`Total Sales` = sum(`Total Sales`)) %>%
  ungroup()

# Creare il grafico a barre
ggplot(df_monthly, aes(x = Month, y = `Total Sales`)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Grafico a barre con colore blu
  labs(title = "Vendite Totali per Mese", x = "Mese", y = "Vendite Totali (€)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ruota i mesi per leggibilità

#### CORRELAZIONE TRA LE VARIABILI

data_aux=new_data4[,-c(1:9)]
dev.new()

matrcorr=cor(data_aux)
#library(ggcorrplot)
#ggcorrplot(matrcorr)
library(corrplot)
corrplot(matrcorr, method = "color", 
         addCoef.col = "black", # colore dei coefficienti
         number.cex = 0.7)

# Gross Sales - Discounts: correlazione negativa (all'aumentare dello sconto diminuisce il fatturato sulla singola vendita)
# Per lo stesso motivo si ha correlazione negativa tra Net Sales e Discounts e tra Taxes e Discounts
# Net quantity e Ordered Item quantity: correlazione positiva poichè si tratta di quantità acquistate e ordinate
# Gross Sales essendo dato dalla somma di Net Sales e Taxes ha ovviamente correlazione positiva con esse
# Anche Net Sales e Taxes hanno correlazione positiva



################################### K-Means Cluster Analysis ###############################

## standardizzazione Z-score
data_cluster=data.frame(as.factor(new_data4$`Product Title`),as.factor(new_data4$`Product Type`),as.factor(new_data4$`Variant Title`),new_data4[-c(1:9)])
names(data_cluster)=names(new_data4[-c(1:3,7:9)])
View(data_cluster)

data_cluster[, sapply(data_cluster, is.numeric)] <- scale(data_cluster[, sapply(data_cluster, is.numeric)])
data_cluster=data_cluster[,-c(1:3)]

# Rimuoviamo Gross Sales, Taxes, Ordered item Quantity

data_cluster_ok <- data_cluster[,-c(2,6,9)]

### Elbow method

library(ggplot2)
library(factoextra)

# Calcolo del WCSS per diversi numeri di cluster
wcss <- vector()
for (k in 1:10) {
  kmeans_model <- kmeans(data_cluster_ok, centers = k, nstart = 10000)
  wcss[k] <- kmeans_model$tot.withinss
}

# Grafico del metodo del gomito
dev.new()
elbow_plot <- data.frame(Clusters = 1:10, WCSS = wcss)
ggplot(elbow_plot, aes(x = Clusters, y = WCSS)) +
  geom_point() +
  geom_line()

# da valutare numero di cluster da 3 a 5

## valuto la misura di silhouette al variare del numero di gruppi
library(cluster)
# Ciclo per i valori di k da 3 a 5
for(k in 3:5){
  dev.new()
  # Esegui K-Means
  km <- kmeans(data_cluster_ok, centers = k, nstart = 10000)
  # Calcola il silhouette
  sil <- silhouette(km$cluster, dist(data_cluster_ok))
  # Crea il grafico silhouette per il valore corrente di k
  plot(sil, main = paste("Silhouette Plot per k =", k), col = rainbow(k))
}

set.seed(124)
fit <- kmeans(data_cluster_ok, 4, nstart=10000) # 4 cluster solution
print(fit)
print(fit$centers)
library(gridExtra)

dev.new()
fviz_cluster(fit,data_cluster_ok, geom = "point", stand=FALSE, palette= c( "green","red","yellow","blue"))

new_set=data.frame(as.factor(new_data4$`Client ID`),as.factor(new_data4$`Product Type`),data_aux,as.factor(fit$cluster))
names(new_set)=c("Client ID","Product Type",names(data_cluster),"gruppo")
View(new_set)

gruppo1=new_set[which(new_set$gruppo==1),1:12]
View(gruppo1)

gruppo2=new_set[which(new_set$gruppo==2),1:12]
View(gruppo2)

gruppo3=new_set[which(new_set$gruppo==3),1:12]
View(gruppo3)

gruppo4=new_set[which(new_set$gruppo==4),1:12]
View(gruppo4)

summary(gruppo1)
# 84 elementi
# da 2 a 3 prodotti acquistati
# hanno usufruito di sconti per una media di 2900
# più della metà di coloro che appartengono a questo gruppo ha acquistato prodotto P

summary(gruppo2)
# 510 elementi
# Quasi tutti hanno acquistato un solo item
# la maggior parte ha acquistato prodotto A
# Hanno usufruito in media di uno sconto medio di 300

summary(gruppo3)
# 6 elementi
# tutti hanno acquistato e restituito un item 
# Hanno usufruito in media di uno sconto di 2700

summary(gruppo4)
# 2388 elementi
# tutti hanno acquistato ed ordinato uno/due item e nessuno ha effettuato resi
# La maggior parte di coloro che appartengono a questo gruppo hanno acquistato prodotto P
# Hanno usufruito in media di uno sconto di 2100

colnames(new_set) <- gsub(" ", "_", colnames(new_set))

# Aggreghiamo il dataset per Client ID
dataset_aggregato <- new_set %>%
  group_by(Client_ID) %>%
  summarise(
    Total_Net_Quantity = mean(Net_Quantity, na.rm = TRUE),
    Total_Gross_Sales = mean(Gross_Sales, na.rm = TRUE),
    Total_Discounts = mean(Discounts, na.rm = TRUE),
    Total_Returns = mean(Returns, na.rm = TRUE),
    Total_Net_Sales = mean(Net_Sales, na.rm = TRUE),
    Total_Sales = mean(Total_Sales, na.rm = TRUE),
    Total_Return_Item_Qty = mean(Returned_Item_Quantity, na.rm = TRUE),
    Total_Ordered_Item_Qty = mean(Ordered_Item_Quantity, na.rm = TRUE),
    Total_Taxes = mean(Taxes, na.rm = TRUE),
    Num_Transactions = n()  # Conta quante transazioni ha fatto il cliente
  ) %>%
  ungroup()

summary(dataset_aggregato)

# Merge tra il dataset aggregato e il dataset originale basandosi sul ClientID
data_aggregato_con_gruppi <- left_join(dataset_aggregato, new_set[, c("Client_ID", "gruppo")], by = "Client_ID")

# Rimuovere eventuali duplicati nel caso ci siano più transazioni per Cliente nel dataset originale
data_aggregato_con_gruppi <- data_aggregato_con_gruppi %>% distinct(Client_ID, .keep_all = TRUE)

library(writexl)
write_xlsx(new_set, "dataset_ass1.xlsx")
write_xlsx(data_aggregato_con_gruppi, "dataset_ass1_con_gruppi.xlsx")

gruppo1_agg=data_aggregato_con_gruppi[which(data_aggregato_con_gruppi$gruppo==1),1:12]
View(gruppo1)

gruppo2_agg=data_aggregato_con_gruppi[which(data_aggregato_con_gruppi$gruppo==2),1:12]
View(gruppo2)

gruppo3_agg=data_aggregato_con_gruppi[which(data_aggregato_con_gruppi$gruppo==3),1:12]
View(gruppo3)

gruppo4_agg=data_aggregato_con_gruppi[which(data_aggregato_con_gruppi$gruppo==4),1:12]
View(gruppo4)

summary(gruppo1)
summary(gruppo2)
summary(gruppo3)
summary(gruppo4)

library(ggplot2)
library(readxl)
library(dplyr)

# Grafico 1: Distribuzione dei cluster
ggplot(data_aggregato_con_gruppi, aes(x = as.factor(gruppo))) +
  geom_bar(fill = "#69b3a2") +
  labs(title = "Distribuzione dei Clienti nei Cluster",
       x = "Cluster", y = "Numero di Clienti") +
  theme_minimal()

# Grafico 2: Valore medio delle vendite per cluster
df_summary <- data_aggregato_con_gruppi %>%
  group_by(gruppo) %>%
  summarise(Vendite_Medie = mean(Total_Net_Sales, na.rm = TRUE))

ggplot(df_summary, aes(x = as.factor(gruppo), y = Vendite_Medie)) +
  geom_col(fill = "#FF9999") +
  labs(title = "Vendite Medie per Cluster",
       x = "Cluster", y = "Vendite Medie (€)") +
  theme_minimal()

# Grafico 3: Valore medio delle vendite per cluster
df_summary <- data_aggregato_con_gruppi %>%
  group_by(gruppo) %>%
  summarise(Discounts = mean(Total_Discounts, na.rm = TRUE))

ggplot(df_summary, aes(x = as.factor(gruppo), y = Discounts)) +
  geom_col(fill = "#FF9999") +
  labs(title = "Sconto medio applicato per Cluster",
       x = "Cluster", y = "Sconto Medio (€)") +
  theme_minimal()

# Grafico 4: Tipologia di prodotti per cluster
ggplot(new_set, aes(x = Product_Type, fill = as.factor(gruppo))) +
  geom_bar(position = "dodge") +
  labs(title = "Tipologia di Prodotti per Cluster",
       x = "Tipo di Prodotto", y = "Frequenza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



