# ## defining new sp list for correlation
# 
# sp_diam_c1 <- diameter_c1_results$species
# sp_depth_c1 <- depth_c1_results$species
# 
# sp_cor <- sp_depth_c1[sp_depth_c1 %in% sp_diam_c1]
# sp_cor <- sp_diam_c1[sp_diam_c1 %in% sp_cor]
# 
# diam_c1_cor <- diameter_c1_results[diameter_c1_results$species %in% sp_cor,]
# depth_c1_cor <- depth_c1_results[depth_c1_results$species %in% sp_cor,]
# 
# diam_c1_cor <- arrange(diam_c1_cor, diam_c1_cor$species)
# depth_c1_cor <- arrange(depth_c1_cor, depth_c1_cor$species)
# 
# diam_c1_cor <- left_join(diam_c1_cor, sp_complete_file, by = "species")
# 
# par(mfrow = c(dim(diam_c1_cor)[1],1))
# par(mar = c(0,0,0,0))
# par(oma = c(4,15,2,0)) 
# 
# plotx_comp <- c(-0.01, 0.04) # x range for crown volume values
# sepr <- 0.2
# clrs <- c("#2E8B57","#FFC000") # defining colors
# 
# for (i in 1:dim(diam_c1_cor)[1]) {
#   
#   plot(range(plotx_comp), c(1, 2), type = "n", bty = "n",
#        ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
#        ylab = "", yaxt = "n", xlab = "", xaxt = "n")
#   
#   if (diam_c1_cor[i,"group"] %in% "D") {
#     
#     polygon(x = c((diam_c1_cor[i, "mean_ba_plot"] - diam_c1_cor[i, "sd_ba_plot"]), (diam_c1_cor[i, "mean_ba_plot"] - diam_c1_cor[i, "sd_ba_plot"]), 
#                   (diam_c1_cor[i, "mean_ba_plot"] + diam_c1_cor[i, "sd_ba_plot"]), (diam_c1_cor[i, "mean_ba_plot"] + diam_c1_cor[i, "sd_ba_plot"])), 
#             y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#             col = clrs[1], border = clrs[1])
#     
#     mean = diam_c1_cor[i, "mean_ba_plot"]
#     mean_0 <- mean$mean_ba_plot
#     segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#     
#     mtext(diam_c1_cor[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#     
#   } else {
#     
#     polygon(x = c((diam_c1_cor[i, "mean_ba_plot"] - diam_c1_cor[i, "sd_ba_plot"]), (diam_c1_cor[i, "mean_ba_plot"] - diam_c1_cor[i, "sd_ba_plot"]), 
#                   (diam_c1_cor[i, "mean_ba_plot"] + diam_c1_cor[i, "sd_ba_plot"]), (diam_c1_cor[i, "mean_ba_plot"] + diam_c1_cor[i, "sd_ba_plot"])), 
#             y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#             col = clrs[2], border = clrs[2])
#     
#     mean = diam_c1_cor[i, "mean_ba_plot"]
#     mean_0 <- mean$mean_ba_plot
#     segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#     
#     mtext(diam_c1_cor[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#     
#   }
#   
# }  
# 
# axis(side = 1, at = c(-0.01, 0, 0.01, 0.02, 0.03, 0.04), 
#      labels = c("-0.01", "0", "0.01", "0.02", "0.03", "0.04"),
#      cex.axis = 0.8) # x-axis
# 
# 
# 
# 
# par(mfrow = c(dim(depth_c1_cor)[1],1))
# par(mar = c(0,0,0,0))
# par(oma = c(4,15,2,0)) 
# 
# plotx_comp <- c(-0.01, 0.04) # x range for crown volume values
# sepr <- 0.2
# clrs <- c("#2E8B57","#FFC000") # defining colors
# 
# for (i in 1:dim(depth_c1_cor)[1]) {
#   
#   plot(range(plotx_comp), c(1, 2), type = "n", bty = "n",
#        ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
#        ylab = "", yaxt = "n", xlab = "", xaxt = "n")
#   
#   if (diam_c1_cor[i,"group"] %in% "D") {
#     
#     polygon(x = c((depth_c1_cor[i, "mean_ba_plot"] - depth_c1_cor[i, "sd_ba_plot"]), (depth_c1_cor[i, "mean_ba_plot"] - depth_c1_cor[i, "sd_ba_plot"]), 
#                   (depth_c1_cor[i, "mean_ba_plot"] + depth_c1_cor[i, "sd_ba_plot"]), (depth_c1_cor[i, "mean_ba_plot"] + depth_c1_cor[i, "sd_ba_plot"])), 
#             y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#             col = clrs[1], border = clrs[1])
#     
#     mean = depth_c1_cor[i, "mean_ba_plot"]
#     mean_0 <- mean$mean_ba_plot
#     segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#     
#     mtext(depth_c1_cor[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#     
#   } else {
#     
#     polygon(x = c((depth_c1_cor[i, "mean_ba_plot"] - depth_c1_cor[i, "sd_ba_plot"]), (depth_c1_cor[i, "mean_ba_plot"] - depth_c1_cor[i, "sd_ba_plot"]), 
#                   (depth_c1_cor[i, "mean_ba_plot"] + depth_c1_cor[i, "sd_ba_plot"]), (depth_c1_cor[i, "mean_ba_plot"] + depth_c1_cor[i, "sd_ba_plot"])), 
#             y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#             col = clrs[2], border = clrs[2])
#     
#     mean = depth_c1_cor[i, "mean_ba_plot"]
#     mean_0 <- mean$mean_ba_plot
#     segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#     
#     mtext(depth_c1_cor[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#     
#   }
#   
# }  
# 
# axis(side = 1, at = c(-0.01, 0, 0.01, 0.02, 0.03, 0.04), 
#      labels = c("-0.01", "0", "0.01", "0.02", "0.03", "0.04"),
#      cex.axis = 0.8) # x-axis
# 
# 
# depth_c1_cor_b <- depth_c1_cor %>% select(species, mean_ba_plot) %>% dplyr::rename(ba_depth = mean_ba_plot)
# diam_c1_cor_b <- diam_c1_cor %>% select(species, mean_ba_plot) %>% dplyr::rename(ba_diam = mean_ba_plot)
# 
# correlation_file <- left_join(depth_c1_cor_b, diam_c1_cor_b, by = "species")
# all_sp_file_cor <- all_species_file %>% dplyr::select(species, mean_Hmax, mean_CR, mean_CD, mean_CV)
# correlation_file <- left_join(correlation_file, all_sp_file_cor, by = "species")
# 
# correlation_file <- left_join(correlation_file, sp_complete_file, by = "species")
# 
# corr_file_A <- correlation_file[correlation_file$group %in% "A",]
# corr_file_D <- correlation_file[correlation_file$group %in% "D",]
# 
# dat_cor_A = cor(corr_file_A[,2], corr_file_A[,4:7], method = "pearson")
# dat_cor_A_b = cor(corr_file_A[,3], corr_file_A[,4:7], method = "pearson")
# 
# dat_cor_D = cor(corr_file_D[,2], corr_file_D[,4:7], method = "pearson")
# dat_cor_D_b = cor(corr_file_D[,3], corr_file_D[,4:7], method = "pearson")
# 
# par(mar = c(0,0,0,0))
# par(mfrow = c(1,1))
# par(oma = c(2,2,2,2))
# 
# corrplot(as.matrix(dat_cor_A), method = "circle", type = "upper", 
#          col = brewer.pal(n = 10, name = "RdYlBu"),
#          tl.col = "black", tl.srt = 45)
# corrplot(as.matrix(dat_cor_A_b), method = "circle", type = "upper", 
#          col = brewer.pal(n = 10, name = "RdYlBu"),
#          tl.col = "black", tl.srt = 45)
# 
# corrplot(as.matrix(dat_cor_D), method = "circle", type = "upper", 
#          col = brewer.pal(n = 10, name = "RdYlBu"),
#          tl.col = "black", tl.srt = 45)
# corrplot(as.matrix(dat_cor_D_b), method = "circle", type = "upper", 
#          col = brewer.pal(n = 10, name = "RdYlBu"),
#          tl.col = "black", tl.srt = 45)
# 
# 
# corrplot(mean_kendall_matrix_A, method = "circle", type = "upper", 
#          col = brewer.pal(n = 10, name = "RdYlBu"),
#          tl.col = "black", tl.srt = 45)
# 
# 
# 
# 
# 
# 
# 
# 
# sp_cor_complete <- read.csv(file = "output/sp_complete_datafile.csv")
# 
# 
# 
# ## Running correlations - Deciduous Angiosperm
# rows = 4
# cols = 4
# n_rep = 100
# cor_results_pearson_A <- array(0,c(rows, cols, n_rep))   
# cor_results_kendall_A <- array(0,c(rows, cols, n_rep))
# cor_sign <- array(0,c(rows, cols, n_rep))   
# 
# 
# 
# for (j in 1:n_rep) {
#   
#   data <- as.data.frame(matrix(nrow = dim(deciduous_angiosperm)[1], ncol = 4))
#   colnames(data) <- c("Hmax", "CR", "CD", "CV")
#   
#   for (i in 1:dim(deciduous_angiosperm)[1]) {
#     
#     mean_Hmax = deciduous_angiosperm[i,"mean_Hmax"]$mean_Hmax
#     sd_Hmax = deciduous_angiosperm[i,"sd_Hmax"]$sd_Hmax
#     
#     mean_CR = deciduous_angiosperm[i,"mean_CR"]$mean_CR
#     sd_CR = deciduous_angiosperm[i,"sd_CR"]$sd_CR
#     
#     mean_CD = deciduous_angiosperm[i,"mean_CD"]$mean_CD
#     sd_CD = deciduous_angiosperm[i,"sd_CD"]$sd_CD
#     
#     mean_CV = deciduous_angiosperm[i,"mean_CV"]$mean_CV
#     sd_CV = deciduous_angiosperm[i,"sd_CV"]$sd_CV
#     
#     
#     data[i,"Hmax"] <- rnorm(1, mean = mean_Hmax, sd = sd_Hmax)
#     data[i,"CR"] <- rnorm(1, mean = mean_CR, sd = sd_CR)
#     data[i,"CD"] <- rnorm(1, mean = mean_CD, sd = sd_CD)
#     data[i,"CV"] <- rnorm(1, mean = mean_CV, sd = sd_CV)
#     
#   }
#   
#   cor_results_pearson_A[,,j] <- cor(data, method = "pearson")
#   cor_results_kendall_A[,,j] <- cor(data, method = "kendall")
#   
# }
# 
# 
# mean_pearson_matrix_A <- matrix(nrow = rows, ncol = cols)
# mean_kendall_matrix_A <- matrix(nrow = rows, ncol = cols)
# 
# 
# for (i in 1:4) {
#   for (j in 1:4) {
#     
#     mean_pearson_matrix_A[i,j] <- mean(cor_results_pearson_A[i,j,])
#     mean_kendall_matrix_A[i,j] <- mean(cor_results_kendall_A[i,j,])
#     
#   }
# }
# 
# 
# 
# par(mar = c(0,0,0,0))
# par(mfrow = c(1,1))
# par(oma = c(2,2,2,2))
# 
# corrplot(mean_pearson_matrix_A, method = "circle", type = "upper", 
#          col = brewer.pal(n = 10, name = "RdYlBu"),
#          tl.col = "black", tl.srt = 45)
# corrplot(mean_kendall_matrix_A, method = "circle", type = "upper", 
#          col = brewer.pal(n = 10, name = "RdYlBu"),
#          tl.col = "black", tl.srt = 45)
# 
# 
# 
# ## Running correlations - Evergreen Gymnosperm
# rows = 4
# cols = 4
# n_rep = 100
# cor_results_pearson_D <- array(0,c(rows, cols, n_rep))   
# cor_results_kendall_D <- array(0,c(rows, cols, n_rep))
# cor_sign <- array(0,c(rows, cols, n_rep))   
# 
# 
# 
# for (j in 1:n_rep) {
#   
#   data <- as.data.frame(matrix(nrow = dim(evergreen_gymnosperm)[1], ncol = 4))
#   colnames(data) <- c("Hmax", "CR", "CD", "CV")
#   
#   for (i in 1:dim(evergreen_gymnosperm)[1]) {
#     
#     mean_Hmax = evergreen_gymnosperm[i,"mean_Hmax"]$mean_Hmax
#     sd_Hmax = evergreen_gymnosperm[i,"sd_Hmax"]$sd_Hmax
#     
#     mean_CR = evergreen_gymnosperm[i,"mean_CR"]$mean_CR
#     sd_CR = evergreen_gymnosperm[i,"sd_CR"]$sd_CR
#     
#     mean_CD = evergreen_gymnosperm[i,"mean_CD"]$mean_CD
#     sd_CD = evergreen_gymnosperm[i,"sd_CD"]$sd_CD
#     
#     mean_CV = evergreen_gymnosperm[i,"mean_CV"]$mean_CV
#     sd_CV = evergreen_gymnosperm[i,"sd_CV"]$sd_CV
#     
#     
#     data[i,"Hmax"] <- rnorm(1, mean = mean_Hmax, sd = sd_Hmax)
#     data[i,"CR"] <- rnorm(1, mean = mean_CR, sd = sd_CR)
#     data[i,"CD"] <- rnorm(1, mean = mean_CD, sd = sd_CD)
#     data[i,"CV"] <- rnorm(1, mean = mean_CV, sd = sd_CV)
#     
#   }
#   
#   cor_results_pearson_D[,,j] <- cor(data, method = "pearson")
#   cor_results_kendall_D[,,j] <- cor(data, method = "kendall")
#   
# }
# 
# 
# mean_pearson_matrix_D <- matrix(nrow = rows, ncol = cols)
# mean_kendall_matrix_D <- matrix(nrow = rows, ncol = cols)
# 
# 
# for (i in 1:4) {
#   for (j in 1:4) {
#     
#     mean_pearson_matrix_D[i,j] <- mean(cor_results_pearson_D[i,j,])
#     mean_kendall_matrix_D[i,j] <- mean(cor_results_kendall_D[i,j,])
#     
#   }
# }
# 
# 
# 
# par(mar = c(0,0,0,0))
# par(mfrow = c(1,1))
# par(oma = c(2,2,2,2))
# 
# corrplot(mean_pearson_matrix_D, method = "circle", type = "upper", 
#          col = brewer.pal(n = 10, name = "RdYlBu"),
#          tl.col = "black", tl.srt = 45)
# corrplot(mean_kendall_matrix_D, method = "circle", type = "upper", 
#          col = brewer.pal(n = 10, name = "RdYlBu"),
#          tl.col = "black", tl.srt = 45)
# 
# 
# 
# 
