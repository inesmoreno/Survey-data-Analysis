d = read.csv('survey_data.tsv', sep='\t')

attach(d)

library(homals)
names(d[20:(20+11)])
png('component_analysis.png')
comp = homals(data=d[19:(19+11)], level="ordinal")
plot(comp)

# next step: regressions for gold-soci, gold-use/tin-use/gold-env, the rest
PCA1 = factor(d$GOLD.SOCI)
PCA2 = factor(d$GOLD.USE + d$TIN.USE + d$GOLD.ENV)
PCA3 = factor(d$TIN.SOCI + d$TIN.ENV + d$TUNGSTEN.SOCI + d$TUNGSTEN.ENV + d$TUNGSTEN.USE + d$COLTAN.USE + d$COLTAN.SOCI +
 d$COLTAN.ENV)


# dependent variables
age = factor(d$AGE.GROUP)
educ = factor(d$EDUCATION)

hardware = d$Hardware # nominal
systems = d$Computer.systems.organization # nominal
networks = d$Networks.and.web.mobile.computing
software = d$Software.and.its.engineering
theory = d$Theory.of.computation
math = d$Mathematics.of.computing
info_syst = d$Information.systems
security = d$Security.and.privacy
hcc = d$Human.centred.computing
methodologies = d$Artificial.intelligence..machine.learning..graphics..and.other.computing.methodologies
social_prof = d$Social.and.professional.issues.in.computing

nowhere = d$Nowhere # nominal
survey = d$This.survey # nominal
news = d$News.media
social = d$Social.Media
friends = d$Friends..general.knowledge
documentaries = d$Documentaries..Movies
church = d$Church
course = d$Course
organisation = d$Organisations
conference = d$Conference..technical.papers.reports


# regression for gold-soci
library(MASS)
f1 = polr(PCA1 ~ age + educ + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,  Hess=TRUE, method="logistic")
#f1 = lm(PCA1 ~ age + educ + hardware + systems + nowhere + survey)
summary(f1)

(ctable <- coef(summary(f1)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

library(pscl)
pR2(f1)

f2 = polr(PCA2 ~ age + educ + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,  Hess=TRUE, method="logistic")
summary(f2)

(ctable2 <- coef(summary(f2)))
p <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2
(ctable2 <- cbind(ctable2, "p value" = p))

f3 = polr(PCA3 ~ age + educ + hardware + systems + networks + software + theory + math + info_syst + security + hcc + methodologies + social_prof  + nowhere + survey + news + social + friends + documentaries + church + course + organisation + conference,  Hess=TRUE, method="logistic")
summary(f3)

(ctable3 <- coef(summary(f3)))
p <- pnorm(abs(ctable3[, "t value"]), lower.tail = FALSE) * 2
(ctable3 <- cbind(ctable3, "p value" = p))




