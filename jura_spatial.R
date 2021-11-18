library(gstat)
library(sp)

data(jura)
summary(prediction.dat)
summary(validation.dat)
summary(transect.dat)
summary(juragrid.dat)

jura.pred = prediction.dat
jura.grid = juragrid.dat

jura.pred$Landuse = factor(prediction.dat$Landuse, 
                           labels=levels(juragrid.dat$Landuse))
jura.pred$Rock = factor(prediction.dat$Rock, 
                        labels=levels(juragrid.dat$Rock))


coordinates(jura.pred) = ~Xloc+Yloc
coordinates(jura.val) = ~Xloc+Yloc
coordinates(jura.grid) = ~Xloc+Yloc
gridded(jura.grid) = TRUE

spplot(jura.pred, 'Cu')
spplot(jura.pred, 'Cd')
spplot(jura.pred, 'Pb')

cop.raw <- idw(Cu ~ 1, jura.pred, jura.grid)
spplot(cop.raw['var1.pred'])

cad.raw <- idw(Cd ~ 1, jura.pred, jura.grid)
spplot(cad.raw['var1.pred'])

lead.raw <- idw(Pb ~ 1, jura.pred, jura.grid)
spplot(lead.raw['var1.pred'])

cop.vgm <- variogram(Cu ~ 1, jura.pred)
plot(cop.vgm)

cad.vgm <- variogram(Cd ~ 1, jura.pred)
plot(cad.vgm)

lead.vgm <- variogram(Pb ~ 1, jura.pred)
plot(lead.vgm)

plot(Cu ~ Landuse, jura.pred)
plot(Cd ~ Landuse, jura.pred)
plot(Pb ~ Landuse, jura.pred)



plot(Cd ~ Ni, jura.pred)
plot(Cd ~ sqrt(Ni), jura.pred)
abline(lm(Cd ~ sqrt(Ni), jura.pred))
plot(log(Cd) ~ log(Ni), jura.pred)
abline(lm(log(Cd) ~ log(Ni), jura.pred))

plot(Cd ~ Cr, jura.pred)
plot(log(Cd) ~ log(Cr), jura.pred)

plot(Cd ~ Co, jura.pred)

plot(Cd ~ Zn, jura.pred)
plot(Cd ~ log(Zn), jura.pred)
plot(log(Cd) ~ log(Zn), jura.pred)



