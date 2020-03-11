library(ggplot2)
library(reshape2)

# In a terminal, run:
# git clone https://github.com/CSSEGISandData/COVID-19.git
# cd COVID-19/

data <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
                 quote="\"", sep=",", dec=".", na.strings=c("", ".."))

population <- c(65, 82, 60, 47, 59)
pp <- population / max(population)

sel <- c(37, 47, 52, 55, 13)
names <- paste(c("Francia", "Alemania", "Italia", "España", "Hubei (China)"),
               paste0("[",population,"M]"))

df <- data[sel, c(-2,-3,-4)]
df[,1] <- names
colnames(df)[2:50] <- 1:49

dfa <- df
th <- 150
for (i in 1:(nrow(dfa) - 1)) {
    l <- length(which(df[i, -1] > th))
    dfa[i, 2:50] <- NA
    dfa[i, 2:(l+1)] <- df[i, which(df[i, -1] > th) + 1]
    dfa[i, 2:(l+1)] <- dfa[i, 2:(l+1)] / pp[i]
}

h <- 15
melted <- melt(dfa[, 1:h])
colnames(melted) <- c("región","día","casos")

p <- ggplot(melted, aes(x=día,y=casos,colour=región,group=región)) +
    geom_line() +
    scale_x_discrete(breaks=seq(1,50,3)) +
    ggtitle("COVID-19: Casos confirmados",
            subtitle= "Fuente: Johns Hopkins University") +
    xlab(paste("Días desde que se alcanzaron", th, "casos")) +
    ylab("número de casos (corregido por población)") + 
    theme_bw()
p

ggsave("casosConfirmados.png")
