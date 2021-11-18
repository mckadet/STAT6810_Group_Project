library(gstat)
library(sp)
library(ggplot2)
library(gridExtra)
library(nlme)
library(car)

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

prediction.dat$Landuse <- as.factor(prediction.dat$Landuse)
prediction.dat$Rock <- as.factor(prediction.dat$Rock)


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





## EDA
unlogged <- ggplot(prediction.dat,aes(x=Zn, y=Cd)) + geom_point() + geom_smooth(method="loess") + 
  theme_minimal() + ggtitle("Cd and Zinc (Unlogged)")

logged <- ggplot(prediction.dat,aes(x=log(Zn), y=log(Cd))) + geom_point() + geom_smooth(method="loess") + 
  theme_minimal() + ggtitle("Cd and Zinc (Logged")

grid.arrange(unlogged, logged, nrow = 2)

ggplot(prediction.dat, aes(x=Xloc, y = Yloc, col = Cd)) + 
  geom_point(alpha = 5, size = 2) + geom_tile() +
  scale_color_distiller(palette="Spectral",na.value=NA) +
  theme_bw() + ggtitle("Cd by Location")

## Modeling Spaptial Structure
# Linear Model
cd.lm <- lm(Cd ~ Zn + Landuse + Rock, data = prediction.dat)
cd.res <- resid(cd.lm)

df <- data.frame(cd.res)
df$index <- 1:nrow(df)

ggplot(prediction.dat, aes(x=Xloc, y = Yloc, col = cd.res)) + 
  geom_point(alpha = 8, size = 2) + geom_tile() +
  scale_color_distiller(palette="Spectral",na.value=NA) +
  theme_bw() + ggtitle("Map of lm Resids")

myVario <- variogram(object = Cd ~ Zn, locations=~Xloc + Yloc, data=prediction.dat)
plot(myVario,main="Figure 4: Variogram of Independent MLR Resids")


## Try out other models for variogram
cdExp <- gls(model = Cd ~ Zn + Landuse + Rock, 
             data=prediction.dat, 
             correlation = corExp(form = ~ Xloc + Yloc, nugget=TRUE), method="ML")
cdSphere <- gls(model = Cd ~ Zn + Landuse + Rock, 
                data = prediction.dat,
                correlation = corSpher(form = ~ Xloc + Yloc, nugget=TRUE), method="ML")
cdGaus <- gls(model = Cd ~ Zn + Landuse + Rock, 
              data = prediction.dat,
              correlation=corGaus(form = ~ Xloc + Yloc, nugget=TRUE), method="ML")

# Compare models using AIC
AIC(cd.lm)
AIC(cdExp)
AIC(cdSphere)
AIC(cdGaus)


## Check conditions
# Linearity
avPlots(lm(Cd ~ Zn + Landuse + Rock, data=prediction.dat))

# Independence
sres <- stdres.gls(cdGaus)

residDF <- with(prediction.dat, data.frame(Xloc = Xloc, Yloc = Yloc, decorrResid=sres))
residVariogram <- variogram(object=decorrResid~1, locations= ~ Xloc + Yloc, data=residDF)
plot(residVariogram)

# Normality
ggplot() + geom_histogram(mapping=aes(x=sres)) + ggtitle("Figure 5: Hist of De-Correlated Residuals")

# Equal Variance
ggplot(prediction.dat, aes(x=fitted(cdGaus), y=sres)) + geom_point() + 
  ggtitle("Figure 6: Fitted Values for WHC Against Standardized Resids")



## Predict using validation df
source('predictgls.R') # Note you'll need to set your working directory to the current directory to source this
cd_preds <- predictgls(glsobj=cdGaus, newdframe=validation.dat,level=.95)

# Plot Predictions
ggplot(prediction.dat, aes(x=Xloc,y = Yloc,col=Cd)) + 
  geom_point(alpha = 8, size = 2) + geom_tile() +
  geom_tile(aes(x = Xloc, y = Yloc, col = Prediction), data = cd_preds) +
  scale_color_distiller(palette = "Spectral", na.value = NA) +
  theme_bw() + ggtitle("Figure 7: WHC Values by Location with Predicted Values for Missing WHC")

ggplot(cd_preds, aes(x = Xloc, y = Yloc)) +
  geom_tile(aes(x = Xloc, y = Yloc, col = Prediction, size = 1)) +
  scale_color_distiller(palette = "Spectral", na.value = NA) +
  theme_bw() + ggtitle("Figure 7: WHC Values by Location with Predicted Values for Missing WHC")
