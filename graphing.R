
d = read.csv('survey_data.tsv', sep='\t')

attach(d)
names(d)

# https://colorbrewer2.org/#type=sequential&scheme=GnBu&n=5
hues = c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")

# Age
png("age-groups.png")
par(mar=c(1.1, 1.1, 1.1, 1.1), xpd=TRUE)
a = table(d$AGE.GROUP)
# https://bookdown.org/ndphillips/YaRrr/plot-margins.html
a_lb = c("18-25", "26-35", "36-45", "56-65", "66+")
pie(a, labels=a_lb, main="Age breakdown of participants", col=hues)

# Education
png("education.png")
par(mar=c(1.1, 4.1, 4.1, 12.1), xpd=TRUE)
e = table(d$EDUCATION)
e_lb = c("Secondary School", "Certification programme", "Bachelor's", "Master's", "Doctorate")
pie(e, labels=e_lb, main="Education breakdown of participants", col=hues)

# Specialty
png("specialties.png")
par(mar=c(4.1, 14.1, 4.1, 4.1), xpd=TRUE) # whitespace for legend
s_lb = c("Hardware", "Computer systems", "Networks", "Software", "Theory of comp", "Mathematics", "Info systems", "Security/privacy", "HCC", "Computing methodologies", "Applied computing", "Social/professional issues")
s = cbind(sum(d[7]), sum(d[8]), sum(d[9]), sum(d[10]), sum(d[11]), sum(d[12]), sum(d[13]), sum(d[14]), sum(d[15]), sum(d[16]), sum(d[17]), sum(d[18]))
barplot(s, names.arg=s_lb, horiz=TRUE, main="Specialty breakdown of participants", las=2, col=hues)

# Use
png("use.png")
u = cbind(table(d$GOLD.USE), table(d$TIN.USE), table(d$TUNGSTEN.USE), table(d$COLTAN.USE))
minerals = c("Gold", "Tin", "Tungsten", "Tantalum")
u_lbs = c("Didn't know it's used", "Know it's used", "Know a bit about use", "Familiar", "Expert on this")
par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE) # whitespace for legend
barplot(u, names.arg=minerals, col=hues, main="Knowledge of mineral's use")
legend("right", legend=u_lbs, inset=c(-0.6,0), fill=hues)

# Env
png("enviro.png")
e = cbind(table(d$GOLD.ENV), table(d$TIN.ENV), table(d$TUNGSTEN.ENV), table(d$COLTAN.ENV))
i_lbs = c("Unfamiliar", "Vaguely familiar", "Somewhat familiar", "Familiar", "Expert on this")
par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE) # whitespace for legend
barplot(e, names.arg=minerals, col=hues, main="Knowledge of mineral's environmental impacts")
legend("right", legend=i_lbs, inset=c(-0.6,0), fill=hues)


# Soc
# Problem: the one expert on gold soci
png("socio.png")
s = cbind(table(d$GOLD.SOCI), table(d$TIN.SOCI), table(d$TUNGSTEN.SOCI), table(d$COLTAN.SOCI))
s[5, 2] = 0
s[5, 3] = 0 
s[5, 4] = 0 
par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE) # whitespace for legend
barplot(s, names.arg=minerals, col=hues, main="Knowledge of mineral's social impacts")
legend("right", legend=i_lbs, inset=c(-0.6,0), fill=hues)
