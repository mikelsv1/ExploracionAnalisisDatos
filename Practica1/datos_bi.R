dat23_bi <- read.csv("df_bi_23.csv")

dat23_bi_abundancia <- dat23_bi[dat23_bi$Parameter %in% c("Abundancia (biomasa)"), ]
dat23_bi_diversidad <- dat23_bi[dat23_bi$Parameter %in% c("Diversidad (biomasa)"), ]

setdiff(dat23$Code, dat23_bi_abundancia$Sample.Point.Code)
setdiff(dat23_bi_abundancia$Sample.Point.Code, dat23$Code)

setdiff(dat23$Code, dat23_bi_diversidad$Sample.Point.Code)
setdiff(dat23_bi_diversidad$Sample.Point.Code, dat23$Code)


dat23_bi_abundancia$Value
dat23_bi_diversidad$Value


# sdas --------------------------------------------------------------------

dat13_bi <- read.csv("df_bi_13.csv")

dat13_bi_abundancia <- dat13_bi[dat13_bi$Parameter %in% c("Abundancia (biomasa)"), ]

setdiff(dat13$Code, dat13_bi_abundancia$Sample.Point.Code)
setdiff(dat13_bi_abundancia$Sample.Point.Code, dat13$Code)

dat13_bi_abundancia$Value





