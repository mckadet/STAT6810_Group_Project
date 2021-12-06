library(gstat)
library(sp)
library(lattice)
library(ggplot2)
library(gridExtra)

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

# sample plot of Cd
spplot(jura.pred, 'Cd', main = "Cd Concentration Levels")

# using inverse distance weighting method to predict values for Cd
cad.raw <- idw(Cd ~ 1, jura.pred, jura.grid)
spplot(cad.raw['var1.pred'], main = "Predicted Cd Concentration (using inverse distance)")

# looking for correlations between Cd and predictor variables
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

# cokriging of Cd with Zn
# visualize the feature-space correlation 
ggplot(as.data.frame(jura.pred), aes(x = Zn, y = Cd)) + 
  geom_point(col = "blue", alpha = 0.5, size = 2.5) + 
  geom_smooth(col = "grey", se = F, method = "lm") + 
  theme_bw() + 
  ggtitle("Unlogged") + 
  xlab("Zinc") +
  ylab("Cadmium")

ggplot(as.data.frame(jura.pred), aes(x = log(Zn), y = log(Cd))) + 
  geom_point(col = "blue", alpha = 0.5, size = 2.5) + 
  geom_smooth(col = "grey", se = F, method = "lm") + 
  theme_bw() + 
  ggtitle("Logged") + 
  xlab("log(Zinc)") +
  ylab("log(Cadmium)")

# create a gstat object for cokriging
# add data
jura.g <- gstat(NULL, id = "Cd", formula = log(Cd) ~ 1, data = jura.pred, nmax = 10)
jura.g <- gstat(jura.g, id = "Zn", formula = log(Zn) ~ 1, data = jura.pred, nmax = 10)

# compute the experimental variograms and cross-variogram
x <- variogram(jura.g)
plot(x)
x <- variogram(jura.g, cutoff = 1.3)
jura.fit <- fit.lmc(x, jura.g, vgm(1, "Exp", 1.2, 1))
plot(x, jura.fit)

# add model
jura.g <- gstat(jura.g, model = vgm(1, "Exp", 1.2, 1), fill.all = T)

# cokriging
jura.g <- fit.lmc(x, jura.g)
cz <- predict(jura.g, jura.grid)

# plot the prediction results
cz$Cd.se = sqrt(cz$Cd.var)
cz$Zn.se = sqrt(cz$Zn.var)
pl1 = spplot(cz["Cd.pred"], main = "log-cadmium prediction")
pl2 = spplot(cz["Zn.pred"], main = "log-zinc prediction")
pl3 = spplot(cz["Cd.se"], main = "log-cadmium standard error")
pl4 = spplot(cz["Zn.se"], main = "log-zinc standard error")

grid.arrange(pl1,pl2,pl3,pl4, nrow = 2)


# mean square error 
mean(cad.raw$var1.pred^2)
mean(cz$Cd.pred^2)


