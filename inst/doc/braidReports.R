## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(braidReports)
set.seed(20240805)

## -----------------------------------------------------------------------------
surface <- synergisticExample
synergisticAnalysis <- runBraidAnalysis(measure ~ concA + concB,
										surface,
										defaults = c(0,2))

names(synergisticAnalysis)

## -----------------------------------------------------------------------------
otherSurface <- antagonisticExample
antagonisticFit <- braidrm(measure ~ concA + concB,
						   otherSurface, model="kappa2")
antagonisticAnalysis <- basicBraidAnalysis(antagonisticFit)

names(antagonisticAnalysis)


## ----fig.width=8,fig.height=10------------------------------------------------
report <- makeBraidReport(synergisticAnalysis,c("A Drug","B Drug"),
						  c(0.5,0.9),c(5,5))
print(report)

## ----fig.width=8, fig.height=10-----------------------------------------------
syncontrol <- list(abbs=c("A","B"),units=c("\u00B5M"),leveltext=c("50","90"),
				   xscale=scale_x_log10(breaks=c(0.1,0.5,2,10),
				   					 labels=as.character),
				   fillscale=scale_fill_viridis_c(option="A"),
				   colorscale=scale_color_brewer(palette="Set1"),
				   title="Example Analysis")
nextReport <- makeBraidReport(synergisticAnalysis,c("A Drug","B Drug"),
						  c(0.5,0.9),c(5,5),control=syncontrol)
print(nextReport)

## ----fig.width=8, fig.height=10-----------------------------------------------
concs <- cbind(otherSurface$concA,otherSurface$concB)
act <- otherSurface$measure

otherSurface$bliss <- deviationSurface(concs,act,"Bliss",range=c(0,1))
otherSurface$zip <- deviationSurface(concs,act,"ZIP",range=c(0,1))
comboRows <- otherSurface$concA>0 & otherSurface$concB>0

ufit <- fitUrsaModel(measure ~ concA + concB, otherSurface)

metrics <- character()
metrics[["V[Bliss]"]] <- signif(mean(otherSurface$bliss[comboRows]),3)
metrics[["ZIP~delta"]] <- signif(mean(otherSurface$zip[comboRows]),3)
metrics[["URSA~alpha"]] <- signif(coef(ufit)[["alpha"]],3)

finalReport <- makeBraidReport(antagonisticAnalysis,
							   compounds=c("First Drug","Second Drug"),
							   levels = c(0.5,0.9), limits=c(8,8),
							   control=list(metrics=metrics,layout="simple"))
print(finalReport)

