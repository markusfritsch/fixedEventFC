
############################################################
###	Basics
############################################################

###
###	Load packages, clear workspace, assign work directory, load data
###

#	install.packages("plm")
library(plm)
#	install.packages("pdynmc")
library(pdynmc)
#	install.packages("moments")
library(moments)
#	install.packages("quantreg")
library(quantreg)
#	install.packages("devtools")
library(devtools)

install_github("markusfritsch/fixedEventFC")


rm(list = ls())



#	setwd("C:/Work/Research/Papers/Wahlen/R")
#	setwd("C:/Work/Research/Papers/Wahlen/R/pre_2024-02-14")	# for code checks
	setwd("D:/Work/20_Projekte/400_Elections/R")






data(datRes)
datE		<- datRes
data(datPolls)
datP		<- datPolls

#datE		<- readRDS("datRes.rds")
#datP		<- readRDS("datPolls.rds")















############################################################
###	Figure 1: AR(1) scatter plots with estimated LS-regression lines
############################################################

###
###	Select configuration
###

BL.set		<- sort(unique(datP$BL))


party.set.orig		<- c("CDU_CSU", "SPD", "Gruene", "FDP", "DIE_LINKE", "AfD")
name.set.orig		<- c("Union", "SPD", "GRE", "FDP", "LIN", "AfD")
col.set.orig		<- c("#000000", "#E3000F", "#64A12D", "#FFD600", "#6D00CC", "#009EE0")






BL.temp		<- "BY"; 	elCycle.temp	<- 4

lambda_max		<- 365
lambda1_gt_max	<- min(datP[datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl >= lambda_max, "daysToEl"])





###
###	Create data set of selected configuration
###

dat	<- datP[
  datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl <= lambda1_gt_max, 
  c("BL", "PollPublished", "PollYear", "PollElCycle", "daysToEl", party.set.orig)
]

dat[dat$daysToEl == max(dat$daysToEl), "daysToEl"]	<- lambda_max

sum.na	<- function(vec){sum(is.na(vec))}
party.set.log	<- apply(dat[, party.set.orig], 2, sum.na) == 0

party.set	<- party.set.orig[party.set.log]
name.set	<- name.set.orig[party.set.log]
col.set	<- col.set.orig[party.set.log]


dat[-1, paste(party.set, ".rev", sep = "")]		<- dat[2:nrow(dat), party.set] - dat[1:(nrow(dat) - 1), party.set]
















###
###	Plots
###

dat.input	<- dat[-1, paste(party.set, ".rev", sep = "")]

dat.plot	<- dat.input
for(co in 1:ncol(dat.input)){
  dat.plot[, co]	<- jitter(dat.input[, co])
}




datLo		<- NULL

for(el in 1:length(party.set)){
  datLo.temp	<- data.frame(
    i			= as.numeric(which(party.set.log)[el]),
    i.name		= party.set[el],
#    t			= dat$t,
    party		= dat[, party.set][, el],
    party.rev	= dat[, paste(party.set, ".rev", sep = "")][, el],
    party.L1	= c(NA, dat[-nrow(dat), party.set][, el]),
    party.rev.L1	= c(NA, dat[-nrow(dat), paste(party.set, ".rev", sep = "")][, el]),
    party.L2	= c(NA, NA, dat[-(nrow(dat)-0:1), party.set][, el])

  )

  datLo		<- rbind(datLo, datLo.temp)
}

reg.within	<- lm(formula = party.rev ~ party.rev.L1 + i.name - 1, data = datLo)
coe	<- coef(reg.within)


reg.slope	<- lm(formula = party.rev ~ party.rev.L1*i.name - party.rev.L1 - 1, data = datLo)
coe.s	<- coef(reg.slope)


reg.EQ4	<- lm(formula = party.rev ~ party.L1 + party.L2, data = datLo)		# EQ.(4) of Clements (2007)

reg.DF	<- lm(formula = party.rev ~ party.L1, data = datLo)		# Dickey-Fuller-Test



#	pdf(paste("AR1_revisions_LS_", BL.temp, "-", elCycle.temp, ".pdf", sep = ""), height = 6, width = 8)

#	range(dat.plot)
x.limits	<- c(-1, 1)*0.06
y.limits	<- x.limits



par(mfrow = c(2, 2), mgp = c(2, 1, 0), mai = c(0.5, 0.5, 0.2, 0.05))

#	for(LA in 1:4){
LA	<- 1		# which lag



matplot(
  x		= dat.plot[-(nrow(dat.plot) + 1 - 1:LA), ],
  y		= dat.plot[-(1:LA), ],
  col		= col.set, type = "p", pch = 20, cex = 0.9, bty = "n", 
  xlab = expression(z[t-1]), ylab = expression(z[t]), 
  xlim = x.limits, ylim = y.limits, 
  main = paste("(Bavaria (", BL.temp, "), ", dat[nrow(dat), "PollYear"], ")", sep = "")#	paste("k =", LA)
)
abline(h = 0, v = 0, col = "grey70", lty = 3)
legend(x = "topright", legend = name.set, col = col.set, pch = 20, bty = "n")







###	Equation (18), party-specific intercept & party-specific slope
eq.number	<- 18

matplot(
  x		= dat.plot[-(nrow(dat.plot) + 1 - 1:LA), ],
  y		= dat.plot[-(1:LA), ],
  col		= col.set, type = "p", pch = 20, cex = 0.9, bty = "n", 
  xlab = expression(z[t-1]), ylab = expression(z[t]), 
  xlim = x.limits, ylim = y.limits, 
  main = paste("Equation (", eq.number, ")", sep = "")
)
abline(h = 0, v = 0, col = "grey70", lty = 3)

for(co in 1:ncol(dat.input)){
  abline(c(coe.s[paste("i.name", party.set[co], sep = "")], coe.s[paste("party.rev.L1:i.name", party.set[co], sep = "")]), col = col.set[co], lwd = 2)
}
	




###	Equation (19), party-specific intercept, identical slope

matplot(
  x		= dat.plot[-(nrow(dat.plot) + 1 - 1:LA), ],
  y		= dat.plot[-(1:LA), ],
  col		= col.set, type = "p", pch = 20, cex = 0.9, bty = "n", 
  xlab = expression(z[t-1]), ylab = expression(z[t]), 
  xlim = x.limits, ylim = y.limits, 
  main = paste("Equation (", eq.number + 1, ")", sep = "")	# paste("k =", LA)
)
abline(h = 0, v = 0, col = "grey70", lty = 3)

for(co in 1:ncol(dat.input)){
  abline(c(coe[paste("i.name", party.set[co], sep = "")], coe["party.rev.L1"]), lwd = 2, col = col.set[co])
}
	



###	Equation (20), Pooled (i.e., identical intercept, identical slope)

reg.temp	<- lm(unlist(dat.input[-(1:LA), ]) ~ unlist(dat.input[-(nrow(dat.input) + 1 - 1:LA), ]))

matplot(
  x		= dat.plot[-(nrow(dat.plot) + 1 - 1:LA), ],
  y		= dat.plot[-(1:LA), ],
  col		= "darkorange", type = "p", pch = 20, cex = 0.9, bty = "n", 
  xlab = expression(z[t-1]), ylab = expression(z[t]), 
  xlim = x.limits, ylim = y.limits, 
  main = paste("Equation (", eq.number + 2, ")", sep = "")	# paste("k =", LA)
)
abline(h = 0, v = 0, col = "grey70", lty = 3)

abline(reg.temp, lwd = 2, col = "darkorange")

#	}

#	dev.off()

















############################################################
###	Figure 2: Plot stream of polls
############################################################

BL.set	<- sort(unique(datP$BL))

range(datP$PollPublished)

date.vec	<- seq(as.Date("2000-01-01"), as.Date("2024-12-31"), by="days")





#	pdf(paste("Pollstream.pdf", sep = ""), height = 5, width = 10)
par(mgp = c(2.4,1,0), mai = c(0.7, 0.7, 0.1, 0.1))

plot(
  x	= date.vec,
  y	= rep(c(1, 16), length.out = length(date.vec)),
  ylim = c(16, 1),
  type = "n",
#  xaxt = "n", 
  yaxt = "n", 
#  xaxs = "i",
  frame.plot = FALSE,
  xlab = "Time", ylab = "Federal state"
)
axis.temp	<- as.Date(paste(2000 + 5*0:5, "-01-01", sep = ""))
axis(side = 1, at = axis.temp, label = paste(2000 + 5*0:5), las = 1)
axis(side = 2, at = 1:16, label = BL.set, las = 1)
#axis(side = 1, at = as.Date("2024-01-01"), label = 2024, las = 1)




for(BL.id in 1:length(BL.set)){
#	BL.temp	<- BL.set[1]

  dat.temp	<- datP[datP$BL == BL.set[BL.id], "PollPublished"]

  points(
    x		= dat.temp,
    y		= rep(BL.id, length.out = length(dat.temp)),
    pch	= "|", col = "grey70", cex = 0.7
  )


  datE.temp	<- datE[datE$BL == BL.set[BL.id], "date"]

  points(
    x		= datE.temp,
    y		= rep(BL.id, length.out = length(datE.temp)),
    pch	= "|", col = "navy", cex = 1.5
  )

}

#	dev.off()










































############################################################
###	Tables 1 & 3
############################################################

###
###	Table 1
###

nrow(datP)								#all available election polls
datP.tmp		<- datP[datP$daysToEl <= 365, ]	#all election polls at most 365 days ahead of election
nrow(datP.tmp)

datP.tmp$Institut[grepl(datP.tmp$Institut, pattern = "IfM")]							<- "IfM"
datP.tmp$Institut[grepl(datP.tmp$Institut, pattern = "GESS")]							<- "GESS"
datP.tmp$Institut[datP.tmp$Institut == "INFO GmbH"]								<- "Info GmbH"
datP.tmp$Institut[grepl(datP.tmp$Institut, pattern = "Aproxima")]						<- "Aproxima"
datP.tmp$Institut[grepl(datP.tmp$Institut, pattern = "FGW")]							<- "FGW"
datP.tmp$Institut[grepl(datP.tmp$Institut, pattern = "Uni") |
		grepl(datP.tmp$Institut, pattern = "TU")]							<- "University"
datP.tmp$Institut[grepl(datP.tmp$Institut, pattern = "Infratest") |
		grepl(datP.tmp$Institut, pattern = "TNS") |
		grepl(datP.tmp$Institut, pattern = "dimap")]							<- "Infratest"
datP.tmp$Institut[datP.tmp$Institut == "Emnid"]									<- "Verian"
datP.tmp$Institut[grepl(datP.tmp$Institut, pattern = "Forschungsgruppe") |
		grepl(datP.tmp$Institut, pattern = "Forschungs-gruppe") |
		grepl(datP.tmp$Institut, pattern = "Forsch’gr.")]						<- "FGW"
datP.tmp$Institut[datP.tmp$Institut %in% names(table(datP.tmp$Institut))[table(datP.tmp$Institut) < 20] ]	<- "Other"

mat	<- data.frame(matrix(NA, nrow = length(unique(datP.tmp$Institut)), ncol = 4))
colnames(mat)	<- c("Acronym", "Opinion research institution/company", "Number of polls", "Average respondents")

for(j in 1:length(unique(datP.tmp$Institut))){

  mat[j, 1]	<- sort(unique(datP.tmp$Institut))[j]
  mat[, 2]	<- c("Forschungsgruppe Wahlen e.V.", "Forsa Gesellschaft für Sozialforschung und statistische Analysen GmbH",
			"GMS Dr. Jung GmbH",
			"Leipziger Institut für Marktforschung", "Infratest dimap Gesellschaft für Trend- und Wahlforschung mbH",
			"INSA-Consulere GmbH", "Other", "Institut für Wahlforschung und Sozialwissenschaft GmbH", "Kantar GmbH",
			"YouGov Deutschland GmbH")
  mat[j, 3]	<- sum(datP.tmp$Institut == unique(datP.tmp$Institut)[j])
  mat[j, 4]	<- round(mean(datP.tmp[datP.tmp$Institut == unique(datP.tmp$Institut)[j], "PollParticipants"], na.rm = TRUE), digits = 0)
#  mat[j, 5]	<- min(datP.tmp[datP.tmp$Institut == unique(datP.tmp$Institut)[j], "date"])
#  mat[j, 6]	<- max(datP.tmp[datP.tmp$Institut == unique(datP.tmp$Institut)[j], "date"])

}
mat




###
###	Table 3
###

colSums(!is.na(datE[,c(5:10)] >= 5.00))		# Elections participated
colSums(datE[,c(5:10)] >= 5.00, na.rm = TRUE)	# Threshold exceeded











############################################################
###	Tables 4 & 5
############################################################

###
###	Table 4
###

res.tmp	<- rbind(
  c(datE[datE$BL == "BY" & datE$elCycle == 4, c("BL", "date")], nrow(datP[datP$BL == "BY" & datP$PollElCycle == 4 & datP$daysToEl <= 720, ]), nrow(datP[datP$BL == "BY" & datP$PollElCycle == 4 & datP$daysToEl <= 365, ]), nrow(datP[datP$BL == "BY" & datP$PollElCycle == 4 & datP$daysToEl <= 180, ]), nrow(datP[datP$BL == "BY" & datP$PollElCycle == 4 & datP$daysToEl <= 90, ])),
  c(datE[datE$BL == "BE" & datE$elCycle == 3, c("BL", "date")], nrow(datP[datP$BL == "BE" & datP$PollElCycle == 3 & datP$daysToEl <= 720, ]), nrow(datP[datP$BL == "BE" & datP$PollElCycle == 3 & datP$daysToEl <= 365, ]), nrow(datP[datP$BL == "BE" & datP$PollElCycle == 3 & datP$daysToEl <= 180, ]), nrow(datP[datP$BL == "BE" & datP$PollElCycle == 3 & datP$daysToEl <= 90, ])),
  c(datE[datE$BL == "NW" & datE$elCycle == 1, c("BL", "date")], nrow(datP[datP$BL == "NW" & datP$PollElCycle == 1 & datP$daysToEl <= 720, ]), nrow(datP[datP$BL == "NW" & datP$PollElCycle == 1 & datP$daysToEl <= 365, ]), nrow(datP[datP$BL == "NW" & datP$PollElCycle == 1 & datP$daysToEl <= 180, ]), nrow(datP[datP$BL == "NW" & datP$PollElCycle == 1 & datP$daysToEl <= 90, ]))
)
colnames(res.tmp)[3:6]	<- paste("lambda.max.", c(720, 365, 180, 90), sep = "")
res.tmp




###
###	Table 5
###

party.set.orig		<- c("CDU_CSU", "SPD", "Gruene", "FDP", "DIE_LINKE", "AfD")

rbind(
  datE[datE$BL == "BY" & datE$elCycle == 4, c("BL", "date", paste(party.set.orig, ".Z", sep = ""), "WB")],
  datE[datE$BL == "BE" & datE$elCycle == 3, c("BL", "date", paste(party.set.orig, ".Z", sep = ""), "WB")],
  datE[datE$BL == "NW" & datE$elCycle == 1, c("BL", "date", paste(party.set.orig, ".Z", sep = ""), "WB")]
)














############################################################
###	Table 6 & Figures 3 to 5
############################################################

###
###	Select configuration
###

party.set.orig		<- c("CDU_CSU", "SPD", "Gruene", "FDP", "DIE_LINKE", "AfD")
name.set.orig		<- c("Union", "SPD", "GRE", "FDP", "LIN", "AfD")
col.set.orig		<- c("#000000", "#E3000F", "#64A12D", "#FFD600", "#6D00CC", "#009EE0")








	BL.temp		<- "BY"; 	elCycle.temp	<- 4		# results for Bavaria
#	BL.temp		<- "BE";  	elCycle.temp	<- 3		# results for Berlin
#	BL.temp		<- "NW"; 	elCycle.temp	<- 1		# results for Northrhine-Westphalia





	lambda_max		<- 365
#	lambda_max		<- 180
lambda1_gt_max	<- min(datP[datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl >= lambda_max, "daysToEl"])





###
###	Create data set of selected configuration
###

dat	<- datP[
  datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl <= lambda1_gt_max, 
  c("BL", "PollPublished", "PollYear", "PollElCycle", "daysToEl", party.set.orig)
]

dat[dat$daysToEl == max(dat$daysToEl), "daysToEl"]	<- lambda_max

sum.na	<- function(vec){sum(is.na(vec))}
party.set.log	<- apply(dat[, party.set.orig], 2, sum.na) == 0

party.set	<- party.set.orig[party.set.log]
name.set	<- name.set.orig[party.set.log]
col.set	<- col.set.orig[party.set.log]


dat[-1, paste(party.set, ".rev", sep = "")]		<- dat[2:nrow(dat), party.set] - dat[1:(nrow(dat) - 1), party.set]









###
###	Table 6
###

res.tmp	<- rbind(
  round(apply(X = dat[, paste(party.set, ".rev", sep = "")], MARGIN = 2, FUN = min, na.rm = TRUE), digits = 4),
  round(apply(X = dat[, paste(party.set, ".rev", sep = "")], MARGIN = 2, FUN = mean, na.rm = TRUE), digits = 4),
  round(apply(X = dat[, paste(party.set, ".rev", sep = "")], MARGIN = 2, FUN = median, na.rm = TRUE), digits = 4),
  round(apply(X = dat[, paste(party.set, ".rev", sep = "")], MARGIN = 2, FUN = max, na.rm = TRUE), digits = 4),
  round(apply(X = dat[, paste(party.set, ".rev", sep = "")], MARGIN = 2, FUN = sd, na.rm = TRUE), digits = 4),
  round(apply(X = dat[, paste(party.set, ".rev", sep = "")], MARGIN = 2, FUN = skewness, na.rm = TRUE), digits = 4),
  round(apply(X = dat[, paste(party.set, ".rev", sep = "")], MARGIN = 2, FUN = kurtosis, na.rm = TRUE), digits = 4)
)
rownames(res.tmp)	<- c("Minimum", "Mean", "Median", "Maximum", "Std. Dev.", "Skewness", "Kurtosis")
res.tmp









###
###	Create daily dataset for plots
###

dat.day	<- data.frame(
  daysToEl		= lambda_max:0
)
for(ro in 1:nrow(dat)){
  if(ro < nrow(dat)){
    dat.day[dat.day$daysToEl %in% dat$daysToEl[ro]:(dat$daysToEl[ro + 1] + 1), party.set]	<- dat[ro, party.set]
  } else {
    dat.day[dat.day$daysToEl %in% dat$daysToEl[ro]:0, party.set]	<- dat[ro, party.set]
  }
}
dat.day[2:nrow(dat.day), paste(party.set, ".rev", sep = "")]	<- dat.day[2:nrow(dat.day), party.set] - dat.day[1:(nrow(dat.day) - 1), party.set]












###
###	Figure 3 to 5 (Plots equivalent to Figure 7 of Regnier (2018))
###

#	pdf(paste(BL.temp, "_", elCycle.temp, ".pdf", sep = ""), height = length(party.set) + 3, width = 6)

layout(matrix(c(1, 1, 1, 2:(length(party.set) + 1)), length(party.set) + 3, 1, byrow = TRUE))

par(mgp = c(2,1,0), mai = c(0.4, 0.4, 0.1, 0.1))



###	Plot of probability forecasts

matplot(
  x		= matrix(data = dat.day$daysToEl, nrow = nrow(dat.day), ncol = length(party.set), byrow = FALSE), 
  y		= dat.day[, party.set],
  type = "l", lty = 1, lwd = 2, col = col.set,
  xlim = c(lambda_max, 0), 
  xlab = "Lead time (days)", 
  ylab = "Forecast probability", 
  ylim = c(0, max(dat.day[, party.set]) + 0.1),
  xaxs = "i", yaxs = "i", frame.plot = FALSE	
)
legend(x = "topright", legend = name.set, col = col.set, lwd = 2, ncol = 6, bty = "n")



###	Plot(s) of forecast revisions

#	par(mfrow = c(6, 1), mgp = c(2,1,0), mai = c(0.4, 0.4, 0.1, 0.1))
par(mgp = c(2,1,0), mai = c(0.1, 0.4, 0.1, 0.1))

for(pa in 1:length(party.set)){

plot(
  x		= dat.day$daysToEl, 
  y		= dat.day[, paste(party.set[pa], ".rev", sep = "")],
  col		= col.set[pa],
  type = "l", lwd = 2,
  xlim = c(lambda_max, 0), ylim = c(-1, 1)*0.06,
  xlab = "",
  xaxt = "n",
  ylab = "Forecast revision", 
  xaxs = "i", yaxs = "i", frame.plot = FALSE, yaxt = "n"
)
pos.temp	<- 0.05*c(-1, 0, 1)
axis(side = 2, at = pos.temp, labels = TRUE)

}

#	dev.off()
















############################################################
###	Table 7
############################################################

###
###	Select configuration
###

	BL.temp		<- "BY"; 	elCycle.temp	<- 4		# results for Bavaria
#	BL.temp		<- "BE";  	elCycle.temp	<- 3		# results for Berlin
#	BL.temp		<- "NW"; 	elCycle.temp	<- 1		# results for Northrhine-Westphalia


	lambda_max		<- 365
#	lambda_max		<- 180
lambda1_gt_max	<- min(datP[datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl >= lambda_max, "daysToEl"])





###
###	Create data set of selected configuration
###

dat	<- datP[
  datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl <= lambda1_gt_max, 
  c("BL", "PollPublished", "PollYear", "PollElCycle", "daysToEl", party.set.orig)
]

dat[dat$daysToEl == max(dat$daysToEl), "daysToEl"]	<- lambda_max

sum.na	<- function(vec){sum(is.na(vec))}
party.set.log	<- apply(dat[, party.set.orig], 2, sum.na) == 0

party.set	<- party.set.orig[party.set.log]
name.set	<- name.set.orig[party.set.log]
col.set	<- col.set.orig[party.set.log]


dat[-1, paste(party.set, ".rev", sep = "")]		<- dat[2:nrow(dat), party.set] - dat[1:(nrow(dat) - 1), party.set]




###
###	Wide to long format for estimation
###

datLo		<- NULL

for(el in 1:length(party.set)){
  datLo.temp	<- data.frame(
    i			= as.numeric(which(party.set.log)[el]),
    i.name		= party.set[el],
    party		= dat[, party.set][, el],
    party.rev	= dat[, paste(party.set, ".rev", sep = "")][, el],
    party.L1	= c(NA, dat[-nrow(dat), party.set][, el]),
    party.rev.L1	= c(NA, dat[-nrow(dat), paste(party.set, ".rev", sep = "")][, el])
  )

  datLo		<- rbind(datLo, datLo.temp)
}



indslope.temp	<- lm(formula = party.rev ~ party.rev.L1*i.name - 1 - party.rev.L1, data = datLo)
within.temp		<- lm(formula = party.rev ~ party.rev.L1 + i.name - 1, data = datLo)
pooled.temp		<- lm(formula = party.rev ~ party.rev.L1, data = datLo)

lm.i		<- summary(indslope.temp)
lm.w		<- summary(within.temp)
lm.p		<- summary(pooled.temp)

res.tmp	<- as.data.frame(lm.i$coefficients[paste("party.rev.L1:i.name", party.set, sep = ""), 1:2])
rownames(res.tmp)	<- paste("EQ18.", party.set, sep = "")

res.tmp["EQ19", ]	<- as.numeric(lm.w$coefficients["party.rev.L1", 1:2])
res.tmp["EQ20", ]	<- as.numeric(lm.p$coefficients["party.rev.L1", 1:2])

round(res.tmp, digits = 3)
























############################################################
###	Figure 6 (Estimated quantile regression coefficients)
############################################################

#	pdf(paste("RQ.pdf", sep = ""), height = 6, width = 9)
par(mfcol = c(2, 3), mgp = c(2,1,0), mai = c(0.6, 0.6, 0.2, 0.1))




###
###	Select configuration
###

for(fcs in 1:3){

switch(
  fcs,
  {
    BL.temp		<- "BY"
    elCycle.temp	<- 4
    plot.title	<- "(BY, 2018)"
  },
  {
    BL.temp		<- "BE"
    elCycle.temp	<- 3
    plot.title	<- "(BE, 2011)"
  },
  {
    BL.temp		<- "NW"
    elCycle.temp	<- 1
    plot.title	<- "(NW, 2005)"
  }
)



	lambda_max		<- 365
#	lambda_max		<- 180
lambda1_gt_max	<- min(datP[datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl >= lambda_max, "daysToEl"])





###
###	Create data set of selected configuration
###

dat	<- datP[
  datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl <= lambda1_gt_max, 
  c("BL", "PollPublished", "PollYear", "PollElCycle", "daysToEl", party.set.orig)
]

dat[dat$daysToEl == max(dat$daysToEl), "daysToEl"]	<- lambda_max

sum.na	<- function(vec){sum(is.na(vec))}
party.set.log	<- apply(dat[, party.set.orig], 2, sum.na) == 0

party.set	<- party.set.orig[party.set.log]
name.set	<- name.set.orig[party.set.log]
col.set	<- col.set.orig[party.set.log]


dat[-1, paste(party.set, ".rev", sep = "")]		<- dat[2:nrow(dat), party.set] - dat[1:(nrow(dat) - 1), party.set]




###
###	Wide to long format for estimation
###

datLo		<- NULL

for(el in 1:length(party.set)){
  datLo.temp	<- data.frame(
    i			= as.numeric(which(party.set.log)[el]),
    i.name		= party.set[el],
    party		= dat[, party.set][, el],
    party.rev	= dat[, paste(party.set, ".rev", sep = "")][, el],
    party.L1	= c(NA, dat[-nrow(dat), party.set][, el]),
    party.rev.L1	= c(NA, dat[-nrow(dat), paste(party.set, ".rev", sep = "")][, el])
  )

  datLo		<- rbind(datLo, datLo.temp)
}



taus	<- 2:18/20



pooled.temp	<- rq(formula = party.rev ~ party.rev.L1, data = datLo, tau = taus)
within.temp	<- rq(formula = party.rev ~ party.rev.L1 + i.name - 1, data = datLo, tau = taus)


set.seed(1234)
rqs.p		<- summary(pooled.temp, se = "boot")
rqs.w		<- summary(within.temp, se = "boot")

rq.res	<- NULL
tau.id	<- 1
for(tau.id in 1:length(taus)){
  rq.res	<- rbind(
    rq.res, c(
      taus[tau.id], 
      as.numeric(rqs.p[[tau.id]]$coefficients["party.rev.L1", 1:2]),
      as.numeric(rqs.w[[tau.id]]$coefficients["party.rev.L1", 1:2])
    )
  )
}
colnames(rq.res)	<- c("tau", "coef.p", "se.p", "coef.w", "se.w")

quant		<- 1.96





###
###	Plots
###

col.set	<- c("grey80", "navy")

plot(
  x		= taus,
  y		= rq.res[, "coef.w"],
  type	= "n", frame.plot = FALSE,
  ylim	= c(-1, 0.5),
  col		= col.set[1], xaxt = "n",
  xlab	= expression(alpha), ylab = expression(Eq. (19): ~~ hat(theta)), main = plot.title
)
axis(side = 1, at = taus, label = taus)
polygon(
  x		= c(taus, rev(taus)),
  y		= c(rq.res[, "coef.w"] - quant*rq.res[, "se.w"], rev(rq.res[, "coef.w"] + quant*rq.res[, "se.w"])),
  border	= NA, col = col.set[1]
)
lines(
  x		= taus,
  y		= rq.res[, "coef.w"],
  type = "b", lwd = 1, pch = 20, cex = 1, lty = 1, col = col.set[2]
)
abline(h = 0)



plot(
  x		= taus,
  y		= rq.res[, "coef.p"],
  type	= "n", frame.plot = FALSE,
  ylim	= c(-1, 0.5),
  col		= col.set[1], xaxt = "n",
  xlab	= expression(alpha), ylab = expression(Eq. (20): ~~ hat(theta)), main = plot.title
)
axis(side = 1, at = taus, label = taus)
polygon(
  x		= c(taus, rev(taus)),
  y		= c(rq.res[, "coef.p"] - quant*rq.res[, "se.p"], rev(rq.res[, "coef.p"] + quant*rq.res[, "se.p"])),
  border	= NA, col = col.set[1]
)
lines(
  x		= taus,
  y		= rq.res[, "coef.p"],
  type = "b", lwd = 1, pch = 20, cex = 1, lty = 1, col = col.set[2]
)
abline(h = 0)

}

#	dev.off()










############################################################
###	Tables A.8 & A.9
############################################################

###
###	Select configuration
###

pres				<- NULL

BL.set		<- sort(unique(datP$BL))


party.set.orig		<- c("CDU_CSU", "SPD", "Gruene", "FDP", "DIE_LINKE", "AfD")
name.set.orig		<- c("Union", "SPD", "GRE", "FDP", "LIN", "AfD")
col.set.orig		<- c("#000000", "#E3000F", "#64A12D", "#FFD600", "#6D00CC", "#009EE0")




for(BL.temp in BL.set){
elCycle.set		<- sort(unique(datP[datP$BL == BL.temp, "PollElCycle"]))
	for(elCycle.temp in elCycle.set){

#	BL.temp		<- "BY"; 	elCycle.temp	<- 4
#	BL.temp		<- "BE";  	elCycle.temp	<- 3
#	BL.temp		<- "NW"; 	elCycle.temp	<- 1
#	BL.temp		<- "HB";	elCycle.temp	<- 2


	lambda_max		<- 365
#	lambda_max		<- 180
lambda1_gt_max	<- min(datP[datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl >= lambda_max, "daysToEl"])



###
###	Create data set of selected configuration
###

dat	<- datP[
  datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl <= lambda1_gt_max, 
  c("BL", "PollPublished", "PollYear", "PollElCycle", "daysToEl", party.set.orig)
]

dat[dat$daysToEl == max(dat$daysToEl), "daysToEl"]	<- lambda_max

sum.na	<- function(vec){sum(is.na(vec))}
party.set.log	<- apply(dat[, party.set.orig], 2, sum.na) == 0

party.set	<- party.set.orig[party.set.log]
name.set	<- name.set.orig[party.set.log]
col.set	<- col.set.orig[party.set.log]


dat[-1, paste(party.set, ".rev", sep = "")]		<- dat[2:nrow(dat), party.set] - dat[1:(nrow(dat) - 1), party.set]






###
###	Wide to long format for estimation
###

datLo		<- NULL

for(el in 1:length(party.set)){
  datLo.temp	<- data.frame(
    i			= as.numeric(which(party.set.log)[el]),
    i.name		= party.set[el],
    party		= dat[, party.set][, el],
    party.rev	= dat[, paste(party.set, ".rev", sep = "")][, el],
    party.L1	= c(NA, dat[-nrow(dat), party.set][, el]),
    party.rev.L1	= c(NA, dat[-nrow(dat), paste(party.set, ".rev", sep = "")][, el])
  )

  datLo		<- rbind(datLo, datLo.temp)
}






###
###	Compute results
###

pres.orig		<- data.frame(
  BL			= NA, 
  elCycle		= NA,
  year		= NA,
  numParties	= NA,
  numPolls		= NA, 
  coe.EQ19		= NA,
  se.EQ19		= NA,
  coe.EQ20		= NA,
  se.EQ20		= NA,
  int.EQ20		= NA,
  se.int.EQ20	= NA
)

pres.temp	<- pres.orig

pres.temp[, 1:5]	<- list(BL.temp, elCycle.temp, datE[datE$BL == BL.temp & datE$elCycle == elCycle.temp, "year"], sum(party.set.log), nrow(dat) - 1)


if(nrow(dat) >= 5){

within.temp		<- lm(formula = party.rev ~ party.rev.L1 + i.name - 1, data = datLo)
pooled.temp		<- lm(formula = party.rev ~ party.rev.L1, data = datLo)

lm.w		<- summary(within.temp)
lm.p		<- summary(pooled.temp)

pres.temp[, 6:7]		<- as.numeric(lm.w$coefficients["party.rev.L1", 1:2])
pres.temp[, 8:9]		<- as.numeric(lm.p$coefficients["party.rev.L1", 1:2])
pres.temp[, 10:11]	<- as.numeric(lm.p$coefficients["(Intercept)", 1:2])

}

  pres	<- rbind(pres, pres.temp)


}}	# end of for-loops



cbind(pres[, c(1,3:5)], round(pres[, 6:9], digits = 3))











############################################################
###	Tables A.10 & A.11
############################################################

###
###	Select configuration
###

tau.set	<- 1:3/4


qres				<- NULL

BL.set		<- sort(unique(datP$BL))


party.set.orig		<- c("CDU_CSU", "SPD", "Gruene", "FDP", "DIE_LINKE", "AfD")
name.set.orig		<- c("Union", "SPD", "GRE", "FDP", "LIN", "AfD")
col.set.orig		<- c("#000000", "#E3000F", "#64A12D", "#FFD600", "#6D00CC", "#009EE0")




for(BL.temp in BL.set){
elCycle.set		<- sort(unique(datP[datP$BL == BL.temp, "PollElCycle"]))
	for(elCycle.temp in elCycle.set){

#	BL.temp		<- "BY"; 	elCycle.temp	<- 4
#	BL.temp		<- "BE";  	elCycle.temp	<- 3
#	BL.temp		<- "NW"; 	elCycle.temp	<- 1
#	BL.temp		<- "HB";	elCycle.temp	<- 2


	lambda_max		<- 365
#	lambda_max		<- 180
lambda1_gt_max	<- min(datP[datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl >= lambda_max, "daysToEl"])



###
###	Create data set of selected configuration
###

dat	<- datP[
  datP$BL == BL.temp & datP$PollElCycle == elCycle.temp & datP$daysToEl <= lambda1_gt_max, 
  c("BL", "PollPublished", "PollYear", "PollElCycle", "daysToEl", party.set.orig)
]

dat[dat$daysToEl == max(dat$daysToEl), "daysToEl"]	<- lambda_max

sum.na	<- function(vec){sum(is.na(vec))}
party.set.log	<- apply(dat[, party.set.orig], 2, sum.na) == 0

party.set	<- party.set.orig[party.set.log]
name.set	<- name.set.orig[party.set.log]
col.set	<- col.set.orig[party.set.log]


dat[-1, paste(party.set, ".rev", sep = "")]		<- dat[2:nrow(dat), party.set] - dat[1:(nrow(dat) - 1), party.set]






###
###	Wide to long format for estimation
###

datLo		<- NULL

for(el in 1:length(party.set)){
  datLo.temp	<- data.frame(
    i			= as.numeric(which(party.set.log)[el]),
    i.name		= party.set[el],
    party		= dat[, party.set][, el],
    party.rev	= dat[, paste(party.set, ".rev", sep = "")][, el],
    party.L1	= c(NA, dat[-nrow(dat), party.set][, el]),
    party.rev.L1	= c(NA, dat[-nrow(dat), paste(party.set, ".rev", sep = "")][, el])
  )

  datLo		<- rbind(datLo, datLo.temp)
}






###
###	Compute results
###

qres.orig		<- data.frame(
  BL			= NA, 
  elCycle		= NA,
  year		= NA,
  numParties	= NA,
  numPolls		= NA
)

qres.orig[paste(rep(c("coe.", "se."), times = length(tau.set)), rep(tau.set*100, each = 2), sep = "")]	<- NA


qres.temp	<- qres.orig

qres.temp[, 1:5]	<- list(BL.temp, elCycle.temp, datE[datE$BL == BL.temp & datE$elCycle == elCycle.temp, "year"], sum(party.set.log), nrow(dat) - 1)


if(nrow(dat) >= 5){

within.temp		<- rq(formula = party.rev ~ party.rev.L1 + i.name - 1, data = datLo, tau = tau.set)

set.seed(1234)
rqs.w		<- summary(within.temp, se = "boot")

  tau.id	<- 1
  for(tau.id in 1:length(tau.set)){
    qres.temp[paste(c("coe.", "se."), tau.set[tau.id]*100, sep = "")]	<- as.numeric(rqs.w[[tau.id]]$coefficients["party.rev.L1", 1:2])
  }

}

  qres	<- rbind(qres, qres.temp)


}}	# end of for-loops



cbind(qres[, c(1,3:5)], round(qres[, 6:11], digits = 3))
















