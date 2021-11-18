library(gstat)
library(sp)

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


# looking for correlations between target variables and preditor variables

# first look at landuse against target variables
plot(Cu ~ Landuse, jura.pred)
plot(Cd ~ Landuse, jura.pred)
plot(Pb ~ Landuse, jura.pred)

# now look at other kinds of metals against target variables, I focused on cadmium
# tried a few different options for transformations

# nickel
plot(Cd ~ Ni, jura.pred)
plot(Cd ~ sqrt(Ni), jura.pred)
abline(lm(Cd ~ sqrt(Ni), jura.pred))
plot(log(Cd) ~ log(Ni), jura.pred)
abline(lm(log(Cd) ~ log(Ni), jura.pred))
# Cr
plot(Cd ~ Cr, jura.pred)
plot(log(Cd) ~ log(Cr), jura.pred)
# Co
plot(Cd ~ Co, jura.pred)
# Zinc
plot(Cd ~ Zn, jura.pred)
plot(Cd ~ log(Zn), jura.pred)
plot(log(Cd) ~ log(Zn), jura.pred)


# now plot variogram of residuals and give it a model, i chose exponential
cad.vgm.2 <- variogram(log(Cd) ~ 1, jura.pred)
plot(cad.vgm.2)
cad.fit.2 = fit.variogram(cad.vgm.2, model = vgm("Exp"))
cad.fit.2
plot(cad.vgm.2, cad.fit.2)

# this is the case if we give the mean a function
cad.vgm.3 <- variogram(log(Cd) ~ log(Zn), jura.pred)
plot(cad.vgm.3)
cad.fit.3 = fit.variogram(cad.vgm.3, model = vgm("Exp"))
cad.fit.3
plot(cad.vgm.3, cad.fit.3)

# kriging
cad.kriged = krige(log(Cd)~1, jura.pred, 
                   jura.grid, model = cad.fit)
spplot(cad.kriged["var1.pred"])



