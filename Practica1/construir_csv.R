# files <- list.files("/prac1/measures_csv_bi", full.names = TRUE)
# df_bi_13 <- data.frame()
# for (file in files) {
#   df <- read.csv(file, sep = ";", header = TRUE, fileEncoding = "iso-8859-1")
#   df_bi_13 <- rbind(df_bi_13, df)
# }
# write.csv(df_bi_13, "/prac1/df_bi_13.csv", row.names = FALSE)

# files <- list.files("/prac1/measures_csv_bi23", full.names = TRUE)
# df_bi_23 <- data.frame()
# for (file in files) {
#   df <- read.csv(file, sep = ";", header = TRUE, fileEncoding = "iso-8859-1")
#   df_bi_23 <- rbind(df_bi_23, df)
# }
# write.csv(df_bi_23, "/prac1/df_bi_23.csv", row.names = FALSE)

# files <- list.files("/prac1/measures_csv_fq", full.names = TRUE)
# df_fq_13 <- data.frame()
# for (file in files) {
#   df <- read.csv(file, sep = ";", header = TRUE, fileEncoding = "iso-8859-1")
#   df_fq_13 <- rbind(df_fq_13, df)
# }
# write.csv(df_fq_13, "/prac1/df_fq_13.csv", row.names = FALSE)

# files <- list.files("/prac1/measures_csv_fq23", full.names = TRUE)
# df_fq_23 <- data.frame()
# for (file in files) {
#   df <- read.csv(file, sep = ";", header = TRUE, fileEncoding = "iso-8859-1")
#   df_fq_23 <- rbind(df_fq_23, df)
# }
# write.csv(df_fq_23, "/prac1/df_fq_23.csv", row.names = FALSE)

#cambiar separadores de los csv a , de los archivos samplePoints13.csv y samplePoints23.csv
# primer archivo samplePoints13.csv
# sample_points_13 <- read.csv("/prac1/samplePoints13.csv", sep = ";", header = TRUE, fileEncoding = "iso-8859-1")
# write.csv(sample_points_13, "/prac1/samplePoints13.csv", row.names = FALSE)

# segundo archivo samplePoints23.csv
# sample_points_23 <- read.csv("/prac1/samplePoints23.csv", sep = ";", header = TRUE, fileEncoding = "iso-8859-1")
# write.csv(sample_points_23, "/prac1/samplePoints23.csv", row.names = FALSE)