d = read.csv('survey_data.tsv', sep='\t')

# Component Analysis
library(homals)
names(d[20:(20+11)])
png('component_analysis.png')
comp = homals(data=d[19:(19+11)], level="ordinal")
plot(comp)


dd <- d

# Next step: regressions for gold-soci, gold-use/tin-use/gold-env, the rest
dd$PCA1 = as.numeric(factor(d$GOLD.SOCI))
dd$PCA2 = as.numeric(factor(d$GOLD.USE + d$TIN.USE + d$GOLD.ENV))
dd$PCA3 = as.numeric(factor(d$TIN.SOCI + d$TIN.ENV + d$TUNGSTEN.SOCI + d$TUNGSTEN.ENV + d$TUNGSTEN.USE + d$COLTAN.USE + d$COLTAN.SOCI +
 d$COLTAN.ENV))


dd$age = factor(d$AGE.GROUP)
dd$occ = factor(d$OCCUPATION)
# Merged education levels 0 and 1 together since there were only 1 in each
dd$educ = d$EDUCATION
dd$educ[dd$educ==0] <- 1
dd$educ <- factor(dd$educ)

# dependent variables
dd$hardware = d$Hardware # nominal
dd$systems = d$Computer.systems.organization # nominal
dd$networks = d$Networks.and.web.mobile.computing
dd$software = d$Software.and.its.engineering
dd$theory = d$Theory.of.computation
dd$math = d$Mathematics.of.computing
dd$info_syst = d$Information.systems
dd$security = d$Security.and.privacy
dd$hcc = d$Human.centred.computing
dd$methodologies = d$Artificial.intelligence..machine.learning..graphics..and.other.computing.methodologies
dd$social_prof = d$Social.and.professional.issues.in.computing

dd$nowhere = d$Nowhere # nominal
dd$survey = d$This.survey # nominal
dd$news = d$News.media
dd$social = d$Social.Media
dd$friends = d$Friends..general.knowledge
dd$documentaries = d$Documentaries..Movies
dd$church = d$Church
dd$course = d$Course
dd$organisation = d$Organisations
dd$conference = d$Conference..technical.papers.reports


# Regression for gold-soci
library(MASS)
f1 = polr(as.factor(PCA1) ~ age + educ + occ + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,  Hess=TRUE, method="logistic",data=dd)
summary(f1)

(ctable <- coef(summary(f1)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

f2 = polr(as.factor(PCA2) ~ age + educ + occ + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,  Hess=TRUE, method="logistic",data=dd)
summary(f2)

(ctable2 <- coef(summary(f2)))
p <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2
(ctable2 <- cbind(ctable2, "p value" = p))

#f3 = polr(as.factor(PCA3) ~ age + educ + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,  Hess=TRUE, method="logistic", data=dd)
#summary(f3)

#(ctable3 <- coef(summary(f3)))
#p <- pnorm(abs(ctable3[, "t value"]), lower.tail = FALSE) * 2
#(ctable3 <- cbind(ctable3, "p value" = p))

# Polr did not work for PCA3, switched to Bayesian approach
library(brms)

# Look for credible interval doesn't cross 0
f1_brms = brm(PCA1 ~ relevel(age,'5') + relevel(educ,'4') + occ + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,
  family=cumulative('logit'),data=dd,cores=4,chains=4)
summary(f1_brms)

f2_brms = brm(PCA2 ~ relevel(age,'5') + relevel(educ,'4') + occ + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,
  family=cumulative('logit'),data=dd,cores=4,chains=4)
summary(f2_brms)

f3_brms = brm(PCA3 ~ relevel(age,'5') + relevel(educ,'4') + occ + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,
  family=cumulative('logit'),data=dd,cores=4,chains=4)
summary(f3_brms)

# what about treating age and education as monotonic ordinal variables?
f1_brms_mo = brm(PCA1 ~ mo(as.numeric(age)) + mo(as.numeric(educ)) + occ + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,
  family=cumulative('logit'),data=dd,cores=4,chains=4)
summary(f1_brms_mo)

f2_brms_mo = brm(PCA2 ~ mo(as.numeric(age)) + mo(as.numeric(educ)) + occ + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,
  family=cumulative('logit'),data=dd,cores=4,chains=4)
summary(f2_brms_mo)

f3_brms_mo = brm(PCA3 ~ mo(as.numeric(age)) + mo(as.numeric(educ)) + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,
  family=cumulative('logit'),data=dd,cores=4,chains=4)
summary(f3_brms_mo)
# the "moas.numericas.characterage" and "moas.numericas.charactereduc" parameters
# give 'overall' effects of age and education


# Plot results (confidence intervals)
ciplot <- function(m){
    fe <- fixef(m)
    xlim <- range(fe[,-2])
    names <- row.names(fe)
    
    plot(NA,xlim=xlim,ylim=c(1,nrow(fe)),xlab='',ylab='',yaxt="n")
    for(i in 1:nrow(fe)){
        lines(fe[i,3:4],c(i,i),lwd=3,lend='butt',col='#555555')
        points(fe[i,1],i,pch=18,cex=2)
        text(x=fe[i,4]+1, y=i, names[i], cex=0.5, col='blue')
        axis(2, at=i, labels=names[i], las=2, cex.axis=0.5)
    }
    title(paste('95% Credible Intervals for ', deparse(substitute(m))))
    abline(v=0,lty=3)
}

png("f1_brms.png")
ciplot(f1_brms)

png("f2_brms.png")
ciplot(f2_brms)

png("f3_brms.png")
ciplot(f3_brms)

png("f1_brms_mo.png")
ciplot(f1_brms_mo)

png("f2_brms_mo.png")
ciplot(f2_brms_mo)

png("f3_brms_mo.png")
ciplot(f3_brms_mo)
