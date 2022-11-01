library(agricolae)
library(car)
library(ggplot2)
library(ggtern)
library(patchwork)
library(reshape2)

#########################
### pollinator visits ###
#########################

st.err <- function(x, ...) {
  sd(x, ...)/sqrt(length(x))
}

dat <- read.csv("visitors.csv", header = T)
dat$Antophora_tot <- dat$Anthophora_sp1_grey + 
  dat$Anthophora_sp2_yellow + dat$Anthophora_sp3_black
vr <- dat[,4:17]/dat$flowers
colnames(vr) <- c(colnames(dat)[4:17])
DAT <- cbind.data.frame(dat[, 1:2], vr)
rm(dat, vr)

M <- aggregate(DAT[, 3:16], by = list(DAT$sp, DAT$hour), FUN = mean)
SD <- aggregate(DAT[, 3:16], by = list(DAT$sp, DAT$hour), FUN = sd)
SE <- aggregate(DAT[, 3:16], by = list(DAT$sp, DAT$hour), FUN = st.err)

# from wide to long format 
M2 <- melt(M, id.vars=c("Group.1", "Group.2"))
SD2 <- melt(SD, id.vars=c("Group.1", "Group.2"))
SE2 <- melt(SE, id.vars=c("Group.1", "Group.2"))

M2$SD <- SD2$value
M2$SE <- SE2$value
rm(SD2, SE2, DAT, SD, SE)

colnames(M2) <- c("sp", "hour", "visitor", "mean", "sd", "se")
M2$hour <- factor(M2$hour, levels = c("7:00-8:00", "10:00-11:00", "12:00-13:00", "16:00-17:00"))

SH <- subset(M2, M2$sp == "Salvia haenkei" & is.na(M2$mean) == F)
g1 <- ggplot(SH, aes(x=hour, y=mean, group=visitor, 
                 shape=visitor)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1) +
  scale_shape_manual(values = rep(c(1, 19), 7),
                     labels = c("Amazilia chionogaster", "Neocorynura sp")) +
  geom_line() + theme_bw() + ylab("visitation·flowers-1·hour-1") + 
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8),
        legend.position = c(0.75, 0.75),
        legend.title=element_blank())

SO <- subset(M2, M2$sp == "Salvia orbignaei" & is.na(M2$mean) == F)
g2 <- ggplot(SO, aes(x=hour, y=mean, group=visitor, 
                     shape=visitor)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1) +
  scale_shape_manual(values = rep(c(1, 19), 7),
                   labels = c("Chlorostilbon lucidus", "Bombus pauloensis")) +
  geom_line() + theme_bw() + ylab("visitation·flowers-1·hour-1") + 
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8),
        legend.position = c(0.75, 0.75),
        legend.title=element_blank())

SS <- subset(M2, M2$sp == "Salvia stachydifolia" & is.na(M2$mean) == F & 
               M2$visitor %in% c("Antophora_tot", "Bombus_pauloensis"))
g3 <- ggplot(SS, aes(x=hour, y=mean, group=visitor, shape=visitor)) + geom_point(size = 3) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1) + 
  scale_shape_manual(values = rep(c(1, 19), 7), 
                     labels = c("Bombus pauloensis", "Anthophora spp.")) +
  geom_line() + theme_bw() + ylab("visitation·flowers-1·hour-1") + 
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8),
        legend.position = c(0.75, 0.75),
        legend.title=element_blank())

SP <- subset(M2, M2$sp == "Salvia personata" & is.na(M2$mean) == F)
g4 <- ggplot(SP, aes(x=hour, y=mean, group=visitor, shape=visitor)) + geom_point(size = 3) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1) + 
  scale_shape_manual(values = rep(c(1, 19), 7), 
                     labels = c("Apis mellifera", "Toxomerus sp.")) +
  geom_line() + theme_bw() + ylab("visitation·flowers-1·hour-1") + 
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8),
        legend.position = c(0.75, 0.75),
        legend.title=element_blank())

# figure 1 (further details were edited in Inkscape)
(g1 | g2) / (g3 | g4) 


##################################
### pollinators' effectiveness ###
##################################

eff <- read.csv("effectivenss.csv", header = T, stringsAsFactors = T)

# Salvia haenkei
sh <- subset(eff, eff$sp == "Salvia haenkei")
fit_sh_t <- glm(pollen.tot ~ pollinator, data = sh, family = quasipoisson())
summary(fit_sh_t)
anova(fit_sh_t, test = "F")

fit_sh_g <- glm(pollen.germ ~ pollinator, data = sh, family = quasipoisson())
summary(fit_sh_g)
anova(fit_sh_g, test = "F")

# Salvia orbignaei
so <- subset(eff, eff$sp == "Salvia orbignaei")
fit_so_t <- glm(pollen.tot ~ pollinator, data = so, family = quasipoisson())
summary(fit_so_t)
anova(fit_so_t, test = "F")

fit_so_g <- glm(pollen.germ ~ pollinator, data = so, family = quasipoisson())
summary(fit_so_g)
anova(fit_so_g, test = "F")

###########################
### Summary for Table 1 ###
###########################

visitor = c("Amazilia chionogaster", "Neocorynura sp.", 
            "Chlorostilbon lucidus", "Bombus pauloensis",
            "Anthophora spp.", "Toxomerus sp.")
eff$pollinator <- factor(eff$pollinator, levels = visitor)
eff$sp <- factor(eff$sp, levels = c("Salvia haenkei", "Salvia orbignaei", 
                                    "Salvia stachydifolia", "Salvia personata"))

table_data <- data.frame(
  plant_sp = c("SH", "SH", "SO", "SO", "SS", "SP"),
  visitor = visitor,
  pollen.tot.mean = round(aggregate(eff$pollen.tot, by = list(eff$pollinator, eff$sp), FUN = mean)[,"x"], 2),
  pollen.tot.sd = round(aggregate(eff$pollen.tot, by = list(eff$pollinator, eff$sp), FUN = sd)[,"x"], 2),
  pollen.ger.mean = round(aggregate(eff$pollen.germ, by = list(eff$pollinator, eff$sp), FUN = mean)[,"x"], 2),
  pollen.ger.sd = round(aggregate(eff$pollen.germ, by = list(eff$pollinator, eff$sp), FUN = sd)[,"x"], 2),
  visits.mean = round(c(colMeans(M[M$Group.1 == "Salvia haenkei", 3:16], na.rm = T)[1:2],
                 colMeans(M[M$Group.1 == "Salvia orbignaei", 3:16], na.rm = T)[3:4],
                 colMeans(M[M$Group.1 == "Salvia stachydifolia", 3:16], na.rm = T)[14],
                 colMeans(M[M$Group.1 == "Salvia personata", 3:16], na.rm = T)[6]), 3),
  visits.sd = round(c(apply(M[M$Group.1 == "Salvia haenkei", 3:16], 2, sd, na.rm = T)[1:2],
               apply(M[M$Group.1 == "Salvia orbignaei", 3:16],  2, sd, na.rm = T)[3:4],
               apply(M[M$Group.1 == "Salvia stachydifolia", 3:16],  2, sd, na.rm = T)[c(14)],
               apply(M[M$Group.1 == "Salvia personata", 3:16],  2, sd, na.rm = T)[6]), 3)
)

table_data$effectivenss <- round(table_data$pollen.ger.mean*table_data$visits.mean, 3)

table_data

######################################################
### Nectar volume, concentration and sugar content ###
######################################################

nec <- read.csv("nectar_vol_con.csv", header = T, stringsAsFactors = T)
vars <- c("sp", "type", "hour", "vol.ul", "brix", "sugar")
nec$sp <- factor(nec$sp, levels = c("S. haenkei", "S. orbignaei", "S. stachydifolia", "S. personata"))

hae <- subset(nec, nec$sp == "S. haenkei")
HAE.m <- aggregate(hae[, c(5,6,8)], by = list(hae$type, hae$hour), FUN = mean, na.rm = T)
HAE.s <- aggregate(hae[, c(5,6,8)], by = list(hae$type, hae$hour), FUN = st.err, na.rm = T)
names(HAE.m) <- paste(names(HAE.m), "m", sep = ".")
names(HAE.s) <- paste(names(HAE.s), "s", sep = ".")
HAE <- cbind.data.frame(HAE.m, HAE.s[, 3:5])
rm(hae, HAE.m, HAE.s)

orb <- subset(nec, nec$sp == "S. orbignaei")
ORB.m <- aggregate(orb[, c(5,6,8)], by = list(orb$type, orb$hour), FUN = mean, na.rm = T)
ORB.s <- aggregate(orb[, c(5,6,8)], by = list(orb$type, orb$hour), FUN = st.err, na.rm = T)
names(ORB.m) <- paste(names(ORB.m), "m", sep = ".")
names(ORB.s) <- paste(names(ORB.s), "s", sep = ".")
ORB <- cbind.data.frame(ORB.m, ORB.s[, 3:5])
rm(orb, ORB.m, ORB.s)

sta <- subset(nec, nec$sp == "S. stachydifolia")
STA.m <- aggregate(sta[, c(5,6,8)], by = list(sta$type, sta$hour), FUN = mean, na.rm = T)
STA.s <- aggregate(sta[, c(5,6,8)], by = list(sta$type, sta$hour), FUN = st.err, na.rm = T)
names(STA.m) <- paste(names(STA.m), "m", sep = ".")
names(STA.s) <- paste(names(STA.s), "s", sep = ".")
STA <- cbind.data.frame(STA.m, STA.s[, 3:5])
rm(sta, STA.m, STA.s)

per <- subset(nec, nec$sp == "S. personata")
PER.m <- aggregate(per[, c(5,6,8)], by = list(per$type, per$hour), FUN = mean, na.rm = T)
PER.s <- aggregate(per[, c(5,6,8)], by = list(per$type, per$hour), FUN = st.err, na.rm = T)
names(PER.m) <- paste(names(PER.m), "m", sep = ".")
names(PER.s) <- paste(names(PER.s), "s", sep = ".")
PER <- cbind.data.frame(PER.m, PER.s[, 3:5])
rm(per, PER.m, PER.s)

# volume
v1 <- ggplot(HAE, aes(x=Group.2.m, y=vol.ul.m, group=Group.1.m, shape=Group.1.m)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=vol.ul.m-vol.ul.s, ymax=vol.ul.m+vol.ul.s), width = 0.1) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line() + theme_bw() + ylab("volume (ul)") + xlab("hour") +
  guides(shape = "none") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8))

v2 <- ggplot(ORB, aes(x=Group.2.m, y=vol.ul.m, group=Group.1.m, shape=Group.1.m)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=vol.ul.m-vol.ul.s, ymax=vol.ul.m+vol.ul.s), width = 0.1) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line() + theme_bw() + ylab("volume (ul)") + xlab("hour") +
  guides(shape = "none") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8))

v3 <- ggplot(STA, aes(x=Group.2.m, y=vol.ul.m, group=Group.1.m, shape=Group.1.m)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=vol.ul.m-vol.ul.s, ymax=vol.ul.m+vol.ul.s), width = 0.1) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line() + theme_bw() + ylab("volume (ul)") + xlab("hour") +
  guides(shape = "none") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8))

v4 <- ggplot(PER, aes(x=Group.2.m, y=vol.ul.m, group=Group.1.m, shape=Group.1.m)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=vol.ul.m-vol.ul.s, ymax=vol.ul.m+vol.ul.s), width = 0.1) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line() + theme_bw() + ylab("volume (ul)") + xlab("hour") +
  guides(shape = "none") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8))

### brix
b1 <- ggplot(HAE, aes(x=Group.2.m, y=brix.m, group=Group.1.m, shape=Group.1.m)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=brix.m-brix.s, ymax=brix.m+brix.s), width = 0.1) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line() + theme_bw() + ylab("concentration (°Brix)") + xlab("hour") +
  guides(shape = "none") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8))

b2 <- ggplot(ORB, aes(x=Group.2.m, y=brix.m, group=Group.1.m, shape=Group.1.m)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=brix.m-brix.s, ymax=brix.m+brix.s), width = 0.1) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line() + theme_bw() + ylab("concentration (°Brix)") + xlab("hour") +
  guides(shape = "none") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8))

b3 <- ggplot(STA, aes(x=Group.2.m, y=brix.m, group=Group.1.m, shape=Group.1.m)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=brix.m-brix.s, ymax=brix.m+brix.s), width = 0.1) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line() + theme_bw() + ylab("concentration (°Brix)") + xlab("hour") +
  guides(shape = "none") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8))

### sugar
a1 <- ggplot(HAE, aes(x=Group.2.m, y=sugar.m, group=Group.1.m, shape=Group.1.m)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=sugar.m-sugar.s, ymax=sugar.m+sugar.s), width = 0.1) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line() + theme_bw() + ylab("sugar content (ug)") + xlab("hour") +
  guides(shape = "none") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8))

a2 <- ggplot(ORB, aes(x=Group.2.m, y=sugar.m, group=Group.1.m, shape=Group.1.m)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=sugar.m-sugar.s, ymax=sugar.m+sugar.s), width = 0.1) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line() + theme_bw() + ylab("sugar content (ug)") + xlab("hour") +
  guides(shape = "none") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8))

a3 <- ggplot(STA, aes(x=Group.2.m, y=sugar.m, group=Group.1.m, shape=Group.1.m)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin=sugar.m-sugar.s, ymax=sugar.m+sugar.s), width = 0.1) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line() + theme_bw() + ylab("sugar content (ug)") + xlab("hour") +
  guides(shape = "none") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8))

# figure 2 (further details were edited in Inkscape)
(v1 + b1 + a1) / (v2 + b2 + a2) / (v3 + b3 + a3) / (v4 + plot_spacer() + plot_spacer())

################################
### nectar sugar composition ###
################################

dat <- read.csv("nectar_comp.csv", header = T, stringsAsFactors = T)
dat$tot <- dat$sucrose + dat$glucose + dat$fructose
dat$sucrose.ratio <- (dat$sucrose/dat$tot)/((dat$glucose/dat$tot) + (dat$fructose/dat$tot))

# figure 3a
p1 <- ggtern(dat, aes(sucrose, glucose, fructose, shape = sp, alpha = 0.7)) + 
  geom_point(size = 3) +
  theme_bw() + 
  scale_shape_manual(values = c(16, 2, 17, 1)) + 
  guides(alpha = "none", shape = "none")
p1

#  figure 3b
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

D2 <- data_summary(data = dat, varname = "sucrose.ratio", groupnames = c("sp"))
p2 <- ggplot(D2) + 
  geom_bar(aes(x=sp, y=sucrose.ratio), 
           stat = "identity", fill = "grey60") +
  geom_errorbar(aes(x= sp, ymin=sucrose.ratio-sd, ymax=sucrose.ratio+sd), width=.2,
                position=position_dodge(.9)) + #theme(axis.title.y = element_blank()) +
  coord_flip() + ylab("sucrose ratio") + theme_linedraw()
p2

# ANOVA on sucrose ratio
fit <- lm(sucrose.ratio~ sp, data = dat)
summary(fit)
Anova(fit, type = "II")

tuk <- HSD.test(fit, trt = "sp")
tuk

# MANOVA on sugar composition 
res <- manova(cbind(glucose, fructose, sucrose) ~ sp, data = dat)
summary(res, test = "Wilks")

###########
### END ###
###########
