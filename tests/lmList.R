## PR#13788

library(nlme)
packageDescription("nlme")# gets the .Library one even in R CMD checking < FIXME !

qm <-  lmList(height ~ age | Subject, data = Oxboys)
nd <- with(Oxboys,
           expand.grid(age = seq(min(age),max(age),length=50),
                       Subject = levels(Subject))
           )

## failed in 3.1-92
res <- predict(qm, nd, se=TRUE)
stopifnot(is.data.frame(res), dim(res) == c(1300, 3),
          identical(names(res), c("Subject", "fit", "se.fit")))

req <- ranef(qm)
(p.req <- plot(req, xlab = "R.Eff.", ylab = "Subj"))
# Fails (p.re2 <- plot(req, age ~ fitted(.)))
iqm <- intervals(qm)
stopifnot(is.array(iqm), dim(iqm) == c(26,3,2))
p.iq <- plot(iqm, ylab = "Subject [factor]")
## Error: formal argument "ylab" matched by multiple .. in 3.1.137
stopifnot(inherits(p.iq, "trellis"),
          inherits(p.req, "trellis"),
          identical( unclass(p.req)[c("xlab","ylab")],
                    list(xlab = "R.Eff.", ylab = "Subj")),
          formula(p.iq) == (group ~ intervals | what))
p.iq


## bug #16452
oo <- subset(Orthodont, age<= 12)
oo <- oo[-(1:2),]
fm6 <- nlme::lmList(distance ~ age | Subject, data= oo)
stopifnot(identical(unlist(coef(fm6)["M01",]),
                    c(`(Intercept)` = 29, age = NA_real_))
          )
ss <- suppressWarnings(summary(fm6))

fm2Pixel.lis<-lmList(pixel~day+I(day^2)|Dog, Pixel)
cc <- coef(fm2Pixel.lis)
stopifnot(all.equal(cc["9", "I(day^2)"], NA_real_),
          identical(dim(cc), c(10L, 3L)))

