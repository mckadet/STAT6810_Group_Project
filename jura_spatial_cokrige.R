library(gstat)
library(sp)
library(lattice)

# pull in data
data(jura)
summary(prediction.dat)
summary(validation.dat)
summary(transect.dat)
summary(juragrid.dat)

# rename pred and grid
jura.pred = prediction.dat
jura.grid = juragrid.dat

# give labels to integer values 
jura.pred$Landuse = factor(prediction.dat$Landuse, 
                           labels=levels(juragrid.dat$Landuse))
jura.pred$Rock = factor(prediction.dat$Rock, 
                        labels=levels(juragrid.dat$Rock))

# convert to spatial dataframes
coordinates(jura.pred) = ~Xloc+Yloc
coordinates(jura.grid) = ~Xloc+Yloc
gridded(jura.grid) = TRUE

# sample plots of target variables
spplot(jura.pred, 'Cu')
spplot(jura.pred, 'Cd')
spplot(jura.pred, 'Pb')

# using inverse distance weighting method to predict values for target variables
cop.raw <- idw(Cu ~ 1, jura.pred, jura.grid)
spplot(cop.raw['var1.pred'])

cad.raw <- idw(Cd ~ 1, jura.pred, jura.grid)
spplot(cad.raw['var1.pred'])

lead.raw <- idw(Pb ~ 1, jura.pred, jura.grid)
spplot(lead.raw['var1.pred'])


# display variograms for each of the target variables
cop.vgm <- variogram(Cu ~ 1, jura.pred)
plot(cop.vgm)

cad.vgm <- variogram(Cd ~ 1, jura.pred)
plot(cad.vgm)

lead.vgm <- variogram(Pb ~ 1, jura.pred)
plot(lead.vgm)


# looking for correlations between target variables and predictor variables


# look at other kinds of metals against target variables, I focused on cadmium
# tried a few different options for transformations

# nickel
plot(Cd ~ Ni, jura.pred)
plot(Cd ~ sqrt(Ni), jura.pred)
abline(lm(Cd ~ sqrt(Ni), jura.pred))
plot(log(Cd) ~ log(Ni), jura.pred)
plot(log(Cd) ~ sqrt(Ni), jura.pred)
abline(lm(log(Cd) ~ log(Ni), jura.pred))
# Cr
plot(Cd ~ Cr, jura.pred)
plot(log(Cd) ~ log(Cr), jura.pred)
# Co
plot(Cd ~ Co, jura.pred)
# Zinc
plot(Cd ~ Zn, jura.pred)
plot(Cd ~ log(Zn), jura.pred)
plot(log(Cd) ~ sqrt(Zn), jura.pred)
plot(log(Cd) ~ log(Zn), jura.pred)

# plot variogram of log(Cd) and give it a model, i chose exponential
lcd.vgm <- variogram(log(Cd) ~ 1, jura.pred, cutoff = 1.6)
lcd.fit <- fit.variogram(lcd.vgm, model = vgm("Exp"))
plot(lcd.vgm, lcd.fit)

# plot variogram of residuals 
lcdr.vgm <- variogram(log(Cd) ~ log(Zn), jura.pred, cutoff = 1.6)
lcdr.fit <- fit.variogram(lcdr.vgm, model = vgm("Exp"))
plot(lcdr.vgm, lcdr.fit)

# kriging

lcd.kriged <- krige(log(Cd) ~ 1, jura.pred, jura.grid, model = lcd.fit)
lcdr.kriged <- krige(log(Cd) ~ log(Zn), jura.pred, jura.grid, model = lcdr.fit)

spplot(lcd.kriged["var1.pred"], main = "Kriging Prediction of Cadmium without Regression")
spplot(lcdr.kriged["var1.pred"], main = "Kriging Prediction of Cadmium with Regression")


# cokriging

xyplot(jura.pred$Cd ~ jura.pred$Zn, jura.pred)
xyplot(log(jura.pred$Cd) ~ log(jura.pred$Zn), jura.pred)

jura.g <- gstat(NULL, id = "Cd", formula = log(Cd) ~ 1, data = jura.pred, nmax = 10)
jura.g <- gstat(jura.g, id = "Zn", formula = log(Zn) ~ 1, data = jura.pred, nmax = 10)
x <- variogram(jura.g)
plot(x)
x <- variogram(jura.g, cutoff = 1.3)
jura.fit <- fit.lmc(x, jura.g, vgm("Exp"))
plot(x, jura.fit)

jura.g <- gstat(jura.g, model = vgm(1, "Exp", 1.2, 1), fill.all = T)
jura.g <- fit.lmc(x, jura.g)
cz <- predict(jura.g, jura.grid)


# plot the prediction results
pl1=spplot(cz["Cd.pred"],main="log-cadmium prediction")
print(pl1,split=c(1,1,2,2),more=T)
pl2=spplot(cz["Zn.pred"],main="log-zinc prediction")
print(pl2,split=c(1,2,2,2),more=T)
cz$Cd.se=sqrt(cz$Cd.var)
cz$Zn.se=sqrt(cz$Zn.var)
pl3=spplot(cz["Cd.se"],main="log-cadmium standard error")
print(pl3,split=c(2,1,2,2),more=T)
pl4=spplot(cz["Zn.se"],main="log-zinc standard error")
print(pl4,split=c(2,2,2,2),more=T)

