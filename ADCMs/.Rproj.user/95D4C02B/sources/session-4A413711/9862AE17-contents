n <- 100
################################################################################
# source("./code/3_Simulation/ADCM/sim_ADCM_summary.R")
################################################################################
load(paste0("./data/G0_G1_PRE_", n, ".RData"))
################################################################################
################################################################################
cex <- 1.5; cex.lab <- 1.5; cex.axis <- 1.5
pdf(file = paste0("./figure/Fig7.pdf"),width = 18, height = 6.5)
par(mfrow = c(1, 2))
op <- par(mar = c(4, 4, 2, 1) + 0.1) 
par(cex = cex, cex.main = 1.5)
par(mgp = c(2, 1, 0))
col <- c("red", "blue")
pch <- 19; 
################################################################################
# sample size: 100
################################################################################
#g_o
time <- seq(0, 1, , 3e3)
plot(time, ADCM::g0(time) - mean(ADCM::g0(time)), xlab = "t", 
     ylab = latex2exp::TeX(r"($\tilde{g}_0(t)$)"), 
     col = col[1], cex.axis = cex.axis,  cex.lab = cex.lab, type = "l", 
     lwd = 7, ylim = c(-4, 6)); 
# title(TeX(r"($n = 100, \, N_t = 30$)"))
points(G0.PRE[, 1], apply(G0.PRE[, -c(1, 2)], 1, mean), col = col[2], cex = cex, pch = pch)
# text(0.001, 5.8, "(a)", cex = cex + 0.5)
legend(x = 0.16, y = -2.0, legend = c("True function", "Estimated value at sampled points"), 
       col = col, ncol = 1, 
       pch = as.numeric(c(".", pch)),
       lty = c("solid", "blank"),
       lwd = 3, bty = "n",
       cex = 1.3)
# g1
# G1.data <- data.frame(z = pred$X4, func = pred$G1, estimate = pred$G1_50) %>% setorder("z")
plot(G1.PRE[, 1], apply(G1.PRE[, -c(1, 2)], 1, mean), col = col[2], ylim = c(-4, 6),
     cex = cex, cex.axis = cex.axis, cex.lab = cex.lab, pch = pch, xlab = "z", 
     ylab = latex2exp::TeX(r"($\tilde{g}_1(z)$)")); 
# title(TeX(r("n = 100, Nt = 30")))
# title(TeX(r"($n = 100, \, N_t = 30$)"))
points(G1.PRE[, 1], G1.PRE[, 2],  col = col[1], type = "l", lwd = 7)
# text(0.001, 5.8, "(b)", cex = cex + 0.5)
legend(x = 0.05, y = -2.0, legend = c("True function", "Estimated value at sampled points"), 
       col = col, ncol = 1, 
       pch = as.numeric(c(".", pch)),
       lty = c("solid", "blank"),
       lwd = 3, bty = "n",
       cex = 1.3)
dev.off()

