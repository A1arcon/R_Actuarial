my_dir <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/PAPER/ESTIMA_2021/rodriguez/"
my_dir2 <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/PAPER/ESTIMA_2021/REMESAS/REMESAS0400062235.txt"
library(data.table)
BD <- data.frame(fread(my_dir2))


######################
#      Grafica 1     #
######################
GEN_ID_EDO_DIST <- function(EDO, DIST){
  paste0(formatC(EDO, width = 2, flag = 0),
         formatC(DIST, width = 2, flag = 0))
}

t0 <- as.POSIXlt(t1 <- paste0("06/06/2021", " ", BD$HORA, ":", BD$MINUTOS),  format="%d/%m/%Y %H:%M")
o <- order(t0)
t0 <- t0[o]
estrat <- GEN_ID_EDO_DIST(BD$ID_ESTADO, BD$ID_DISTRITO_FEDERAL)[o]

tn <- as.POSIXlt("06/06/2021 22:35",  format="%d/%m/%Y %H:%M")
t <- seq.POSIXt(t0[4], tn, by = "5 min")
k <- length(t)
casillas <- rep(NA, k)
estrat2 <- rep(NA, k)
for (j in 1:k) {
  id <- t0 < t[j]
  casillas[j] <- sum(id)
  estrat2[j] <- length(unique(estrat[id]))
}

# 
tt0 <- min(t)
ttn <- max(t)
my_dir_latex <- "/Volumes/GoogleDrive/Mi unidad/IIMAS/PRESENTACIONES/FNE34/"
pdf(paste0(my_dir_latex, "graf0.pdf"), width = 10, height = 4, pointsize= 9, paper="special")
par(mfrow = c(1, 2), mar = c(4.5, 5, 0.8, 0.2), mgp=c(3.5, 1, 0)) #c(bottom, left, top, right)
casillas0 <- c(casillas[1], casillas[-1] - casillas[-k])
dd <- 5000
plot(t, casillas, type = "l", lwd = 2, 
     xlab = "", ylab = "Total de casillas", 
     ylim = c(0, dd), axes = FALSE)
axis.POSIXct(1, seq.POSIXt(tt0, ttn, by = "20 min"), 
             at = seq.POSIXt(tt0, ttn, by = "20 min"), 
             las = 2, cex.axis = 1.5)
axis(2, seq(0, dd, by = 500), las = 2, cex.axis = 1.1)
box(lwd = 2)
dd <- 300
plot(t, estrat2, type = "l", lwd = 2, 
     xlab = "", ylab = "Estratos con casillas", 
     ylim = c(0, 300), axes = FALSE)
axis.POSIXct(1, seq.POSIXt(tt0, ttn, by = "20 min"), 
             at = seq.POSIXt(tt0, ttn, by = "20 min"), 
             las = 2, cex.axis = 1.5)
axis(2, seq(0, dd, by = 30), las = 2, cex.axis = 1.1)
box(lwd = 2)
dev.off()

