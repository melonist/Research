getwd()
rm(list = ls())
setwd('~/Dropbox/R/2017_Branching_HA_Imprinting/')
exposure.switch = FALSE
load('H5N1_model_inputs.RData')
ls()

##############################################
## Visualize different model predictions
##############################################
## Set up plotting scheme
countries = c('China', 'Cambodia', 'Egypt', 'Indonesia', 'Vietnam', 'Thailand')
c.length = sapply(countries, function(cc) length(grep(cc, x = rownames(p0.master))))
base.cols = c('darkred', 'blue', 'darkgreen', 'turquoise', 'goldenrod', 'purple3')
cols = 'start'
for(ii in 1:6){
ramp <- colorRamp(c(base.cols[ii], "white"))
cols = c(cols, rgb( ramp(seq(0, .8, length = c.length[ii])), max = 255))
}
cols = cols[-1]

## normalize
p0.master = p0.master/rowSums(p0.master)

## Set layout
layout(mat = matrix(c(1, 1, 1, 2), nrow = 1))

## 1. p0.master describes demographic age structure for each country, with a specific country-year on each row and a birth year on each column
p0.master # Look at the matrix
barplot(p0.master, col = cols, border = cols)
legend('topright', rownames(p0.master)[1:23], col = cols[1:15], border = NA, bty = 'n', pch = 15, cex = .75)
plot.new()
legend('topright', rownames(p0.master)[24:48], col = cols[16:48], border = NA, bty = 'n', pch = 15, cex = .75)



## 2. Visualize age-specific risk
## Ue master - takes a 1 if the birth year is over 65 and a 0 otherwise
## Um master - takes a 1 if the birth year is between 5 and 65 and 0 otherwise
## Uc master - takes a 1 if the birth year is under 5 and a 0 otherwise

## Say that children and the elderly have twice the risk of developing a severe infection as middle-aged adults and older children:
age.risk = 2*Uc.master+Um.master+2*Ue.master
dev.off()
barplot(age.risk, col = cols, border = cols)


## 3. Combine age-speceific risk with demography
DA = age.risk*p0.master
barplot(DA, col = cols, border = cols)


## Compare all three side-by-side
par(mfrow = c(3, 1)) # arrange plots in 3 rows and 1 column
barplot(p0.master, col = cols, border = cols, main = 'Demography only')
barplot(age.risk, col = cols, border = cols, main = 'Age risk only')
barplot(DA, col = cols, border = cols, main = 'DA = Demography * Age risk')


## 4. Look at probabilities of imprinting to group 1 HA
dev.off()
barplot(weights.master.1, col = cols, border = cols, main = 'Group 1 imprinting probs only')
## Now let's say protection isn't perfect and people with HA imprinting protection are .25 times as likely to come down with severe infection as unprotected people. Now, the overall risk for the whole populaiton based on imprinting alone looks like this:
HAimprinting.risk = (weights.master.1*.25+weights.master.2+weights.master.3+weights.master.naiive)
par(mfrow = c(3, 1))
barplot(p0.master, col = cols, border = cols, main = 'Demography only')
barplot(weights.master.1, col = cols, border = cols, main = 'Group 1 imprinting probs only')
barplot(HAimprinting.risk, col = cols, border = cols, main = 'DH = Demography * HA imprinting risk')
