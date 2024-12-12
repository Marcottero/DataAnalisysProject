rm(list=ls())
library(igraph)
library(tidyverse)
library(ggtext)

setwd("C:/Users/39334/OneDrive/Desktop/BDA Lezioni R")

Elecrtricity_Production <- read.csv("Electricity_Production_By_Source.csv")

Elecrtricity_Production1 <- filter(Elecrtricity_Production, Year == '2020') %>%
  filter(Entity %in% c('Austria', 'Belgium', 'Bulgaria', 'Croatia','Cyprus', 'Czechia', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy', 'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'EU-27'))

Elecrtricity.Production <-  filter(Elecrtricity_Production, Entity %in% c('Austria', 'Belgium', 'Bulgaria', 'Croatia','Cyprus', 'Czechia', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy', 'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'EU-27')) 

elec.prod <- subset(Elecrtricity_Production1, select = -c(Code, Year))
  
en.pulita <- rowSums(elec.prod[c("Electricity.from.hydro..TWh.", "Electricity.from.other.renewables..TWh.", "Electricity.from.solar..TWh.","Electricity.from.wind..TWh.", "Electricity.from.nuclear..TWh.")])#sommo quota di energia pulita nel vettore en.pulita
  
elec.prod1 <- cbind(elec.prod, en.pulita) #aggiungo il vettore en.pulita alla matrice 

en.inquinante <- rowSums(elec.prod1[c("Electricity.from.coal..TWh.", "Electricity.from.oil..TWh.", "Electricity.from.gas..TWh.")]) #sommo la quota di energia inquinante nel vettore en.inqinante

elec.prod1 <- cbind(elec.prod1, en.inquinante) #aggiungo il vettore en.inquinante alla matrice          

elec.prod2 <- subset(elec.prod1, select = -c(Electricity.from.coal..TWh., Electricity.from.gas..TWh., Electricity.from.wind..TWh., Electricity.from.hydro..TWh., Electricity.from.solar..TWh., Electricity.from.oil..TWh., Electricity.from.other.renewables..TWh., Electricity.from.nuclear..TWh.))

en.tot <- rowSums(elec.prod2[c("en.pulita", "en.inquinante")]) #valori di energia totale prodotta

elec.prod3 <- cbind(elec.prod2, en.tot) #aggiungo la colonna della somma 

elec.prod <- filter(elec.prod, Entity == "EU-27") %>%
  subset(select = -Entity)

elec.prod.t <- t(elec.prod)



# Disegnare il grafico a torta
fonte <- c("Carbone", "Gas", "Idroelettrico", "Altre fonti rinnovabili", "Fotovoltaico", "Petrolio", "Eolico", "Nucleare")
total <- sum(elec.prod.t)
percentage <- 100 * elec.prod.t/total 
new_percentage <- round(percentage, 1)
verifica <- sum(new_percentage)
diff <- 100 - verifica
new_percentage[8] <- new_percentage[8] + diff
new_fonte <- paste(new_percentage, "%")
colors <- c("black","purple","blue","orange", "yellow", "grey","red","green")
par(mar = c(3, 0, 3, 3))
pie(percentage, labels = new_fonte, col = colors, border = 1, clockwise = TRUE, cex = 0.9, radius = 1)
legend("topright", legend = fonte, cex = 0.9 , fill = colors, x = 1.7, y = 0.8)
title(expression(underline("Figura 1: Energia Elettrica prodotta dall'Unione Europea nel 2020")), cex.main=0.9, font = 1)
	
# Disegnare grafico andamento EU-27 energia pulita ed energia inquinante 2000-2020
Elecrtricity_Production3 <- filter(Elecrtricity_Production, Entity == 'EU-27')
en.pulita1 <- rowSums(Elecrtricity_Production3[c("Electricity.from.hydro..TWh.", "Electricity.from.other.renewables..TWh.", "Electricity.from.solar..TWh.","Electricity.from.wind..TWh.", "Electricity.from.nuclear..TWh.")])
en.inquinante1 <- rowSums(Elecrtricity_Production3[c("Electricity.from.coal..TWh.", "Electricity.from.oil..TWh.", "Electricity.from.gas..TWh.")])
Eu0020 <- cbind(en.pulita1, en.inquinante1)
Eu0020_df <- data.frame(year=(2000:2020), en.pulita1, en.inquinante1)
ggplot(Eu0020_df, aes(x = year, y = value, color = type)) +
	geom_line(aes(y = en.pulita1, color = "Energia Pulita"), linewidth = 0.5) +
	geom_line(aes(y = en.inquinante1, color = "Energia Inquinante"), linewidth = 0.5) +
	labs(title = expression(underline("Figura 2: Produzione di energia elettrica dell'Unione Europea dal 2000 al 2020")),
						x = "Anno",
						y = "TWh",
						color = "") +
 scale_color_manual(values = c("red", "green")) +
	theme(plot.margin = unit(c(1, 1, 1, 5), "cm")) +
	scale_y_continuous(limits = c(1000, 1800)) +
	scale_x_continuous(breaks = seq(2000, 2020, 2)) +
	theme(plot.title = element_text(size = 10, face = 0))

# Disegnare grafico andamento Italia dal 2000 al 2020
EP_italia <- filter(Elecrtricity_Production, Entity == 'Italy', Year >= 2000, Year <= 2020)
en.pulita2 <- rowSums(EP_italia[c("Electricity.from.hydro..TWh.", "Electricity.from.other.renewables..TWh.", "Electricity.from.solar..TWh.","Electricity.from.wind..TWh.", "Electricity.from.nuclear..TWh.")])
en.inquinante2 <- rowSums(EP_italia[c("Electricity.from.coal..TWh.", "Electricity.from.oil..TWh.", "Electricity.from.gas..TWh.")])
Italia0020_df <- data.frame(year=(2000:2020), en.pulita2, en.inquinante2)
ggplot(Italia0020_df, aes(x = year, y = value, color = type)) +
	geom_line(aes(y = en.pulita2, color = "Energia Pulita"), linewidth = 0.5) +
	geom_line(aes(y = en.inquinante2, color = "Energia Inquinante"), linewidth = 0.5) +
	labs(title = expression(underline("Figura 3: Produzione di energia elettrica dell'Italia dal 2000 al 2020")),
						x = "Anno",
						y = "TWh",
						color = "") +
	scale_color_manual(values = c("red", "green")) +
	theme(plot.margin = unit(c(1, 1, 1, 5), "cm")) +
	scale_y_continuous(limits = c(0, 300)) +
	scale_x_continuous(breaks = seq(2000, 2020, 2)) +
	theme(plot.title = element_text(size = 10, face = 0))

#Grafico energia elettrica prodotta da fonti pulite e inquinanti di ciascuna delle 27 nazioni europee nel 2020
EP27 <- Elecrtricity_Production1[-8,] %>%
	subset(select = -c(Year, Code))
en.pulita3 <- rowSums(EP27[c("Electricity.from.hydro..TWh.", "Electricity.from.other.renewables..TWh.", "Electricity.from.solar..TWh.","Electricity.from.wind..TWh.", "Electricity.from.nuclear..TWh.")])
en.inquinante3 <- rowSums(EP27[c("Electricity.from.coal..TWh.", "Electricity.from.oil..TWh.", "Electricity.from.gas..TWh.", "Electricity.from.hydro..TWh.", "Electricity.from.other.renewables..TWh.", "Electricity.from.solar..TWh.","Electricity.from.wind..TWh.", "Electricity.from.nuclear..TWh.")])
EP27_v2 <- cbind(EP27, en.pulita3, en.inquinante3) %>%
	subset(select = -c(Electricity.from.coal..TWh., Electricity.from.gas..TWh., Electricity.from.wind..TWh., Electricity.from.hydro..TWh., Electricity.from.solar..TWh., Electricity.from.oil..TWh., Electricity.from.other.renewables..TWh., Electricity.from.nuclear..TWh.))
ggplot(EP27_v2, aes(x = reorder(Entity, en.inquinante3), y = en.inquinante3, fill = "Energia Inquinante")) +
	geom_bar(stat = "identity", position = "identity", width = 0.5) +
	geom_bar(aes(y = en.pulita3, fill = "Energia Pulita"), stat = "identity", position = "identity", width = 0.5) +
	scale_fill_manual(values = c("Energia Inquinante" = "red", "Energia Pulita" = "green")) +
	coord_flip() +
	xlab("Paese") +
	ylab("TWh") +
	labs(fill = "", title = expression(underline("Figura 4: Produzione di energia elettrica dei singoli Paesi UE nel 2020"))) +
	theme_bw() +
	theme(plot.title = element_text(size = 10)) +
	scale_y_continuous(breaks = seq(0, 600, 200), limits = c(0, 600))


