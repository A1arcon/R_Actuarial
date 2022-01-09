my_dir1 <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/PAPER/ESTIMA_2021/REMESAS/REMESAS0400062235.txt"
my_dir2 <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/PAPER/ESTIMA_2021/rodriguez/"
my_dir3 <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/PAPER/ESTIMA_2021/nieto/"
library(data.table)
BD <- data.frame(fread(my_dir1))


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
my_dir_latex <- "/Volumes/GoogleDrive/Mi unidad/CR2021/9LATEX/PAPER/graf/"
pdf(paste0(my_dir_latex, "graf1.pdf"), width = 12, height = 3, pointsize= 9, paper="special")
par(mfrow = c(1, 3), mar = c(4.2, 5, 0.2, 0.2), mgp=c(3.5, 1, 0)) #c(bottom, left, top, right)
casillas0 <- c(casillas[1], casillas[-1] - casillas[-k])
dd <- 200
plot(t, casillas0, type = "l", lwd = 2, 
     xlab = "", ylab = "Numer of polling stations", 
     ylim = c(0, dd), axes = FALSE)
axis.POSIXct(1, seq.POSIXt(tt0, ttn, by = "20 min"), 
             at = seq.POSIXt(tt0, ttn, by = "20 min"), 
             las = 2, cex.axis = 1.5)
axis(2, seq(0, dd, by = 25), las = 2, cex.axis = 1.1)
box(lwd = 2)
dd <- 5000
plot(t, casillas, type = "l", lwd = 2, 
     xlab = "", ylab = "Accumulate number of polling stations", 
     ylim = c(0, dd), axes = FALSE)
axis.POSIXct(1, seq.POSIXt(tt0, ttn, by = "20 min"), 
             at = seq.POSIXt(tt0, ttn, by = "20 min"), 
             las = 2, cex.axis = 1.5)
axis(2, seq(0, dd, by = 500), las = 2, cex.axis = 1.1)
box(lwd = 2)
dd <- 300
plot(t, estrat2, type = "l", lwd = 2, 
     xlab = "", ylab = "Number of stratum", 
     ylim = c(0, 300), axes = FALSE)
axis.POSIXct(1, seq.POSIXt(tt0, ttn, by = "20 min"), 
             at = seq.POSIXt(tt0, ttn, by = "20 min"), 
             las = 2, cex.axis = 1.5)
axis(2, seq(0, dd, by = 30), las = 2, cex.axis = 1.1)
box(lwd = 2)
dev.off()


###########################
#      Graficas 2 y 3     #
###########################
my_ffiles1 <- list.files(paste0(my_dir2, "diputaciones_pef/"))[-1]
my_ffiles2 <- list.files(paste0(my_dir2, "pef/"))[-1]
vect_HORA <- substr(my_ffiles1, 17, 20)
k <- length(vect_HORA)
LINF_CONF <- NULL
LSUP_CONF <- NULL
LINF_VVE <- NULL
LSUP_VVE <- NULL
LINF_PART <- NULL
LSUP_PART <- NULL
for (j in 1:k) {
  BD1 <- t(read.csv(paste0(my_dir2, "diputaciones_pef/", my_ffiles1[j]))[,-c(1:3, 15)])
  LINF_CONF <- cbind(LINF_CONF, BD1[,1])
  LSUP_CONF <- cbind(LSUP_CONF, BD1[,3])  
  BD2 <- t(read.csv(paste0(my_dir2, "pef/", my_ffiles2[j]))[,-c(1:3, 16)])
  LINF_VVE <- cbind(LINF_VVE, BD2[-12,1])
  LSUP_VVE <- cbind(LSUP_VVE, BD2[-12,3])  
  LINF_PART <- c(LINF_PART, BD2[12,1])
  LSUP_PART <- c(LSUP_PART, BD2[12,3])  
}

max(LSUP_CONF)






dd <- 300
my_col <- c("blue", "darkgreen", "yellow", "red", "green", "orange", 
            "purple")
hora <- as.POSIXlt(paste0("06/06/2021 ", substr(vect_HORA, 1, 2), ":", substr(vect_HORA, 3, 4)),  format="%d/%m/%Y %H:%M")
r <- as.POSIXct(range(hora))
r[2] <- r[2] + 300
insect_x <- -0.17
my_dir_latex <- "/Volumes/GoogleDrive/Mi unidad/CR2021/9LATEX/PAPER/graf/"
pdf(paste0(my_dir_latex, "graf2_freq.pdf"), width = 8, height = 4, pointsize= 9, paper="special")
par(mar = c(5.1, 4.1, 0.15, 8.1), xpd=TRUE) #c(bottom, left, top, right)
plot(hora, LINF_CONF[1,], type = "l", 
     ylim = c(0, dd), col = "blue", lwd = 2, 
     ylab = "Number of seats", xlab = "time", 
     axes = FALSE)
for(j in 1:7){
  lines(hora, LINF_CONF[j,], lwd = 2, col = my_col[j])
  lines(hora, LSUP_CONF[j,], lwd = 2, col = my_col[j])
}
axis.POSIXct(1, seq.POSIXt(r[1], r[2], by = "10 min"), 
             at = seq.POSIXt(r[1], r[2], by = "10 min"), 
             las = 2, cex.axis = 0.9)
axis(2, seq(0, dd, by = 25), las = 2, cex = 0.8)
# TRUE
nh <- length(hora)
lines(hora, rep(197, nh), lwd = 2, lty = 3, col = "purple")
lines(hora, rep(114, nh), lwd = 2, lty = 3, col = "blue")
lines(hora, rep(71, nh), lwd = 2, lty = 3, col = "darkgreen")
lines(hora, rep(13, nh), lwd = 2, lty = 3, col = "yellow")
lines(hora, rep(23, nh), lwd = 2, lty = 3, col = "orange")
lines(hora, rep(43, nh), lwd = 2, lty = 3, col = "green")
lines(hora, rep(39, nh), lwd = 2, lty = 3, col = "red")
box(lwd = 2)
my_pp <- row.names(LINF_CONF)[1:7]
pos_legend <- "topright"
legend(pos_legend, my_pp, lwd = 2, col = my_col, 
       bty = "n", cex = 1, inset=c(insect_x, 0), y.intersp=2)
dev.off()



dd <- 50
my_col <- c("blue", "darkgreen", "yellow", "red", 
            "green", "orange", "purple")
my_pp <- row.names(LSUP_VVE)[1:7]
pdf(paste0(my_dir_latex, "graf3_freq.pdf"), width = 8, height = 4, pointsize= 9, paper="special")
par(mar = c(5.1, 4.1, 0.15, 8.1), xpd=TRUE) #c(bottom, left, top, right)
plot(hora, LINF_VVE[1,], type = "l", 
     ylim = c(0, dd), col = "blue", lwd = 2, 
     ylab = "% of votes with respect to the VVE", xlab = "time", 
     axes = FALSE)
for(j in 1:7){
  lines(hora, LINF_VVE[j,], lwd = 2, col = my_col[j])
  lines(hora, LSUP_VVE[j,], lwd = 2, col = my_col[j])
}

axis.POSIXct(1, seq.POSIXt(r[1], r[2], by = "10 min"), 
             at = seq.POSIXt(r[1], r[2], by = "10 min"), 
             las = 2, cex.axis = 0.9)
axis(2, seq(0, dd, by = 5), las = 2, cex = 0.8)
# TRUE
nh <- length(hora)
lines(hora, rep(35.2, nh), lwd = 2, lty = 3, col = "purple")
lines(hora, rep(18.9, nh), lwd = 2, lty = 3, col = "blue")
lines(hora, rep(18.4, nh), lwd = 2, lty = 3, col = "darkgreen")
lines(hora, rep(3.9, nh), lwd = 2, lty = 3, col = "yellow")
lines(hora, rep(7.2, nh), lwd = 2, lty = 3, col = "orange")
lines(hora, rep(5.7, nh), lwd = 2, lty = 3, col = "green")
lines(hora, rep(7.2, nh), lwd = 2, lty = 3, col = "red")
legend(pos_legend, my_pp, lwd = 2, col = my_col, 
       bty = "n", cex = 1, inset=c(insect_x, 0), y.intersp=2)
box(lwd = 2)
dev.off()





######################
#      Graficas 2 y 3     #
######################
my_ffiles1 <- list.files(paste0(my_dir3, "diputaciones_pef/"))[4:50]
my_ffiles2 <- list.files(paste0(my_dir3, "pef/"))[4:50]
vect_HORA <- substr(my_ffiles1, 13, 16)
k <- length(vect_HORA)
LINF_CONF <- NULL
LSUP_CONF <- NULL
LINF_VVE <- NULL
LSUP_VVE <- NULL
LINF_PART <- NULL
LSUP_PART <- NULL
for (j in 1:k) {
  BD1 <- t(read.csv(paste0(my_dir3, "diputaciones_pef/", my_ffiles1[j]))[,-c(1:3, 15)])
  LINF_CONF <- cbind(LINF_CONF, BD1[,1])
  LSUP_CONF <- cbind(LSUP_CONF, BD1[,3])  
  BD2 <- t(read.csv(paste0(my_dir3, "pef/", my_ffiles2[j]))[,-c(1:3, 16)])
  LINF_VVE <- cbind(LINF_VVE, BD2[-12,1])
  LSUP_VVE <- cbind(LSUP_VVE, BD2[-12,3])  
  LINF_PART <- c(LINF_PART, BD2[12,1])
  LSUP_PART <- c(LSUP_PART, BD2[12,3])  
}

max(LSUP_CONF)






dd <- 300
my_col <- c("blue", "darkgreen", "yellow", "red", "green", "orange", 
            "purple")
hora <- as.POSIXlt(paste0("06/06/2021 ", substr(vect_HORA, 1, 2), ":", substr(vect_HORA, 3, 4)),  format="%d/%m/%Y %H:%M")
r <- as.POSIXct(range(hora))
r[2] <- r[2] + 300
insect_x <- -0.17
my_dir_latex <- "/Volumes/GoogleDrive/Mi unidad/CR2021/9LATEX/PAPER/graf/"
pdf(paste0(my_dir_latex, "graf2_bayes.pdf"), width = 8, height = 4, pointsize= 9, paper="special")
par(mar = c(5.1, 4.1, 0.15, 8.1), xpd=TRUE) #c(bottom, left, top, right)
plot(hora, LINF_CONF[1,], type = "l", 
     ylim = c(0, dd), col = "blue", lwd = 2, 
     ylab = "Number of seats", xlab = "time", 
     axes = FALSE)
for(j in 1:7){
  lines(hora, LINF_CONF[j,], lwd = 2, col = my_col[j])
  lines(hora, LSUP_CONF[j,], lwd = 2, col = my_col[j])
}
axis.POSIXct(1, seq.POSIXt(r[1], r[2], by = "10 min"), 
             at = seq.POSIXt(r[1], r[2], by = "10 min"), 
             las = 2, cex.axis = 0.9)
axis(2, seq(0, dd, by = 25), las = 2, cex = 0.8)
# TRUE
nh <- length(hora)
lines(hora, rep(197, nh), lwd = 2, lty = 3, col = "purple")
lines(hora, rep(114, nh), lwd = 2, lty = 3, col = "blue")
lines(hora, rep(71, nh), lwd = 2, lty = 3, col = "darkgreen")
lines(hora, rep(13, nh), lwd = 2, lty = 3, col = "yellow")
lines(hora, rep(23, nh), lwd = 2, lty = 3, col = "orange")
lines(hora, rep(43, nh), lwd = 2, lty = 3, col = "green")
lines(hora, rep(39, nh), lwd = 2, lty = 3, col = "red")
box(lwd = 2)
my_pp <- row.names(LINF_CONF)[1:7]
pos_legend <- "topright"
legend(pos_legend, my_pp, lwd = 2, col = my_col, 
       bty = "n", cex = 1, inset=c(insect_x, 0), y.intersp=2)
dev.off()



dd <- 50
my_col <- c("blue", "darkgreen", "yellow", "red", 
            "green", "orange", "purple")
my_pp <- row.names(LSUP_VVE)[1:7]
pdf(paste0(my_dir_latex, "graf3_bayes.pdf"), width = 8, height = 4, pointsize= 9, paper="special")
par(mar = c(5.1, 4.1, 0.15, 8.1), xpd=TRUE) #c(bottom, left, top, right)
plot(hora, LINF_VVE[1,], type = "l", 
     ylim = c(0, dd), col = "blue", lwd = 2, 
     ylab = "% of votes with respect to the VVE", xlab = "time", 
     axes = FALSE)
for(j in 1:7){
  lines(hora, LINF_VVE[j,], lwd = 2, col = my_col[j])
  lines(hora, LSUP_VVE[j,], lwd = 2, col = my_col[j])
}

axis.POSIXct(1, seq.POSIXt(r[1], r[2], by = "10 min"), 
             at = seq.POSIXt(r[1], r[2], by = "10 min"), 
             las = 2, cex.axis = 0.9)
axis(2, seq(0, dd, by = 5), las = 2, cex = 0.8)
# TRUE
nh <- length(hora)
lines(hora, rep(35.2, nh), lwd = 2, lty = 3, col = "purple")
lines(hora, rep(18.9, nh), lwd = 2, lty = 3, col = "blue")
lines(hora, rep(18.4, nh), lwd = 2, lty = 3, col = "darkgreen")
lines(hora, rep(3.9, nh), lwd = 2, lty = 3, col = "yellow")
lines(hora, rep(7.2, nh), lwd = 2, lty = 3, col = "orange")
lines(hora, rep(5.7, nh), lwd = 2, lty = 3, col = "green")
lines(hora, rep(7.2, nh), lwd = 2, lty = 3, col = "red")
legend(pos_legend, my_pp, lwd = 2, col = my_col, 
       bty = "n", cex = 1, inset=c(insect_x, 0), y.intersp=2)
box(lwd = 2)
dev.off()






