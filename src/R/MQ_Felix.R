library(lme4)
library(plyr)
library(ggplot2)

#dat <- read.table("../../data/MQ.tab", header=TRUE)
dat <- read.table("../../data/FS_MOCO.tab", header=TRUE)

## ======================================================================
## Compute additional variables
## ======================================================================

dat$text <- as.character(dat$text)

# compute sentence count on person level: sc
sc <- ddply(dat, .(pid), function(df) {nrow(df)})
colnames(sc)[2] <- "sc"
dat <- merge(dat, sc, by="pid")

# compute word count on sentence level: wc.s
dat$wc.s <- sapply(strsplit(dat$text, " ", fixed=TRUE), length)

# compute word count on person level: wc.p
wc.p <- ddply(dat, .(pid), function(df) sum(df$wc.s))
colnames(wc.p)[2] <- "wc.p"
dat <- merge(dat, wc.p, by="pid")

# correlations of word and sentence counts
cor(dat[, c("wc.s", "wc.p", "sc")])

## ======================================================================
## IRT-Analyses
## ======================================================================

# function computes Andrich reliability and plots theta with SE
getRel <- function(formula, data, plot=TRUE) {
	library(arm)
	library(lme4)
	
	g1 <- glmer(formula, data, family=binomial(link="logit"))
	cat("Picture pulls:\n---------------\n")
	print(ranef(g1)$pic)
	tau <- VarCorr(g1)$pid[1]	# true score variance
	epsilon <- mean(se.ranef(g1)$pid^2)
	rel <- tau/(tau+epsilon)
	cat(paste("\nAndrich reliability =", round(rel, 3), "\n"))

	if (plot==TRUE) {
		est <- data.frame(theta=ranef(g1)$pid[, 1], se=se.ranef(g1)$pid)
		est <- est[order(est$theta), ]
		est$id <- 1:nrow(est)
		print(ggplot(est, aes(x=id, y=theta, ymin=theta-se, ymax=theta+se)) + geom_pointrange() + theme_bw())
	}
	invisible(list(model=g1, rel=rel))
}

# get reliability, picture pulls, and person plot for achievement
getRel(ach ~ 1 + (1|pic) + (1|pid), dat, plot=TRUE)

# collect reliabilities from all motives
REL <- matrix(c(
	getRel(pow ~ 1 + (1|pic) + (1|pid), dat, plot=FALSE)$rel,
	getRel(pow2 ~ 1 + (1|pic) + (1|pid), dat, plot=FALSE)$rel,
	getRel(ach ~ 1 + (1|pic) + (1|pid), dat, plot=FALSE)$rel,
	getRel(ach2 ~ 1 + (1|pic) + (1|pid), dat, plot=FALSE)$rel,
	getRel(aff ~ 1 + (1|pic) + (1|pid), dat, plot=FALSE)$rel,
	getRel(aff2 ~ 1 + (1|pic) + (1|pid), dat, plot=FALSE)$rel),
	ncol=2, byrow=TRUE,
)
rownames(REL) <- c("pow", "ach", "aff")
colnames(REL) <- c("ohne 2-Satz", "mit 2-Satz")
round(REL, 3)



## collect reliabilities from all motives, using also random effects for interactions of pid:pic
## todo: use rbind?
REL <- matrix(c(
  getRel(pow ~ 1 + (1|pic) + (1|pid) + (1|interaction(pid,pic)),
         dat, plot=FALSE)$rel,
  getRel(pow2 ~ 1 + (1|pic) + (1|pid) + (1|interaction(pid,pic)),
         dat, plot=FALSE)$rel,
  getRel(ach ~ 1 + (1|pic) + (1|pid) + (1|interaction(pid,pic)),
         dat, plot=FALSE)$rel,
  getRel(ach2 ~ 1 + (1|pic) + (1|pid) + (1|interaction(pid,pic)),
         dat, plot=FALSE)$rel,
  getRel(aff ~ 1 + (1|pic) + (1|pid) + (1|interaction(pid,pic)),
         dat, plot=FALSE)$rel,
  getRel(aff2 ~ 1 + (1|pic) + (1|pid) + (1|interaction(pid,pic)),
         dat, plot=FALSE)$rel),
              ncol=2, byrow=TRUE,
)
rownames(REL) <- c("pow", "ach", "aff")
colnames(REL) <- c("ohne 2-Satz", "mit 2-Satz")
round(REL, 3)

	
# ---------------------------------------------------------------------
# Effect of word count, sentence count

glmer(ach ~ sc + (1|pic) + (1|pid), dat, family=binomial(link="logit"))
glmer(aff ~ sc + (1|pic) + (1|pid), dat, family=binomial(link="logit"))
glmer(pow ~ sc + (1|pic) + (1|pid), dat, family=binomial(link="logit"))

glmer(ach ~ wc.p + (1|pic) + (1|pid), dat, family=binomial(link="logit"))
glmer(aff ~ wc.p + (1|pic) + (1|pid), dat, family=binomial(link="logit"))
glmer(pow ~ wc.p + (1|pic) + (1|pid), dat, family=binomial(link="logit"))

glmer(ach ~ wc.s + (1|pic) + (1|pid) + (1|unit), dat, family=binomial(link="logit"))
glmer(aff ~ wc.s + (1|pic) + (1|pid) + (1|unit), dat, family=binomial(link="logit"))
glmer(pow ~ wc.s + (1|pic) + (1|pid) + (1|unit), dat, family=binomial(link="logit"))

glmer(ach ~ wc.p + wc.s + sc + (1|pic) + (1|pid) + (1|unit), dat, family=binomial(link="logit"))
glmer(aff ~ wc.p + wc.s + sc + (1|pic) + (1|pid) + (1|unit), dat, family=binomial(link="logit"))
glmer(pow ~ wc.p + wc.s + sc + (1|pic) + (1|pid) + (1|unit), dat, family=binomial(link="logit"))

## ======================================================================
## IRT: Tuerlinckx-Style (dichotomize on picture level)
## ======================================================================

# dat2: aggregate over sentences (sum scores for each picture)
dat2 <- ddply(dat, .(pid, pic), function(df) {colSums(df[, c("pow", "ach", "aff", "pow2", "ach2", "aff2")])})

# dat3: dichotomize on picture level
dat3 <- dat2
dat3[, -c(1:2)] <- as.numeric(dat3[, -c(1:2)] >= 1)

(g2 <- glmer(ach ~ 1 + (1|pic) + (1|pid), dat3, family=binomial(link="logit")))

getRel(pow ~ 1 + (1|pic) + (1|pid), dat3, plot=TRUE)
getRel(ach ~ 1 + (1|pic) + (1|pid), dat3, plot=TRUE)
getRel(aff ~ 1 + (1|pic) + (1|pid), dat3, plot=TRUE)


## ======================================================================
## Compare person estimates
## ======================================================================

m1 <- glmer(ach ~ 1 + (1|pic) + (1|pid), dat, family=binomial(link="logit"))
t1 <- ranef(m1)$pid[, 1]

m2 <- glmer(ach2 ~ 1 + (1|pic) + (1|pid), dat, family=binomial(link="logit"))
t2 <- ranef(m2)$pid[, 1]

SUM <- ddply(dat, .(pid), function(df) {colSums(df[, c("pow", "ach", "aff", "pow2", "ach2", "aff2")])})

scores <- data.frame(t1, t2, SUM[, c("ach", "ach2")])
round(cor(scores), 3)
