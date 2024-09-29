## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(braidReports)
set.seed(20240828)

## -----------------------------------------------------------------------------
concentrations <- c(0,2^(-3:3))
surface <- data.frame(
	concA = rep(rep(concentrations,each=length(concentrations)),each=3),
	concB = rep(rep(concentrations,times=length(concentrations)),each=3),
	replicate = rep(c(1,2,3),times=(length(concentrations)^2))
)
surface$actual <- evalBraidModel(
	surface$concA,
	surface$concB,
	c(1, 1, 3, 3, 2, 0, 100, 100, 100)
)
surface$measure <- surface$actual + rnorm(nrow(surface),sd=7)

head(surface, 12)

## ----warning=FALSE------------------------------------------------------------
ggplot(surface,aes(x=concA,y=measure,colour=factor(concB)))+
	geom_point()+
	stat_summary(geom="line",fun.data=mean_se)+
	scale_x_log10()+
	labs(x="Drug A",y="Effect",colour="Drug B")

## ----warning=FALSE------------------------------------------------------------
ggplot(surface, aes(x=concA,y=concB))+
	stat_summary_2d(aes(z=measure), fun="mean")+
	scale_x_log10()+
	scale_y_log10()+
	scale_fill_distiller(palette="RdYlBu")+
	coord_equal()+
	labs(x="Drug A",y="Drug B",fill="Effect")

## ----warning=FALSE------------------------------------------------------------
ggplot(surface,aes(x=concA,y=concB))+
	geom_braid(aes(fill=measure))+
	scale_x_log10()+
	scale_y_log10()+
	scale_fill_distiller(palette="RdYlBu")+
	coord_equal()+
	labs(x="Drug A",y="Drug B",fill="Effect")

## ----warning=FALSE------------------------------------------------------------
ggplot(surface,aes(x=concA,y=concB))+
	geom_braid(aes(fill=measure))+
	geom_point(colour="black")+
	scale_x_log10("Drug A")+
	scale_y_log10("Drug B")+
	scale_fill_viridis_c("Effect",option="A")+
	coord_equal()+
	facet_wrap(vars(replicate))

## ----warning=FALSE------------------------------------------------------------
surface$tilewidth <- log10(2)*0.9
surface$tilewidth[surface$concA==0] <- log10(2)/2

surface$tileheight <- log10(2)*0.9
surface$tileheight[surface$concB==0] <- log10(2)/2

ggplot(surface,aes(x=concA,y=concB))+
	geom_braid(aes(fill=measure,width=tilewidth,height=tileheight),space=3)+
	scale_x_log10("Drug A")+
	scale_y_log10("Drug B")+
	scale_fill_distiller("Effect",palette="RdYlBu")+
	coord_equal()

## -----------------------------------------------------------------------------
glassSurface <- surface
glassSurface$concA[glassSurface$replicate==2] <- 
	glassSurface$concA[glassSurface$replicate==2]*1.25
glassSurface$concB[glassSurface$replicate==3] <- 
	glassSurface$concB[glassSurface$replicate==3]*1.25

glassSurface$actual <- evalBraidModel(
	glassSurface$concA,
	glassSurface$concB,
	c(1, 1, 3, 3, -0.5, 0, 60, 100, 100)
)
glassSurface$measure <- glassSurface$actual+rnorm(nrow(glassSurface),sd=7)

head(glassSurface, 12)

## ----warning=FALSE------------------------------------------------------------
ggplot(glassSurface,aes(x=concA,y=concB))+
	geom_braid(aes(fill=measure))+
	geom_point(colour="black")+
	scale_x_log10("Drug A")+
	scale_y_log10("Drug B")+
	scale_fill_distiller("Effect",palette="RdYlBu")+
	coord_equal()

## ----warning=FALSE------------------------------------------------------------
ggplot(glassSurface,aes(x=concA,y=concB))+
	geom_braid_glass(aes(fill=measure))+
	geom_point(colour="black")+
	scale_x_log10("Drug A")+
	scale_y_log10("Drug B")+
	scale_fill_distiller("Effect",palette="RdYlBu")+
	coord_equal()

## ----warning=FALSE------------------------------------------------------------
ggplot(glassSurface,aes(x=concA,y=concB))+
	geom_braid_glass(aes(fill=measure,width=tilewidth,height=tileheight),space=2)+
	scale_x_log10("Drug A")+
	scale_y_log10("Drug B")+
	scale_fill_distiller("Effect",palette="RdYlBu")+
	coord_equal()

## ----warning=FALSE------------------------------------------------------------
ggplot(surface,aes(x=concA,y=concB))+
	geom_braid_smooth(aes(fill=measure))+
	scale_x_log10()+
	scale_y_log10()+
	scale_fill_distiller(palette="RdYlBu")+
	coord_equal()+
	labs(x="Drug A",y="Drug B",fill="Effect")

## ----warning=FALSE------------------------------------------------------------
ggplot(glassSurface,aes(x=concA,y=concB))+
	geom_braid_smooth(aes(fill=measure))+
	geom_point(colour="black")+
	scale_x_log10("Drug A")+
	scale_y_log10("Drug B")+
	scale_fill_distiller("Effect",palette="RdYlBu")+
	coord_equal()

ggplot(glassSurface,aes(x=concA,y=concB))+
	geom_braid_smooth(aes(fill=measure,width=log10(2),height=log10(2)))+
	scale_x_log10("Drug A")+
	scale_y_log10("Drug B")+
	scale_fill_distiller("Effect",palette="RdYlBu")+
	coord_equal()

ggplot(glassSurface,aes(x=concA,y=concB))+
	geom_braid_smooth(aes(fill=measure,width=tilewidth,height=tileheight),space=2)+
	scale_x_log10("Drug A")+
	scale_y_log10("Drug B")+
	scale_fill_distiller("Effect",palette="RdYlBu")+
	coord_equal()

## ----warning=FALSE------------------------------------------------------------
ggplot(surface,aes(x=concA,y=concB))+
	geom_braid_smooth(aes(fill=measure))+
	geom_braid_contour(aes(z=measure),breaks=10*(1:9),colour="black",linetype=2)+
	scale_x_log10()+
	scale_y_log10()+
	scale_fill_distiller(palette="RdYlBu")+
	coord_equal()+
	labs(x="Drug A",y="Drug B",fill="Effect")

## ----warning=FALSE------------------------------------------------------------
ggplot(glassSurface,aes(x=concA,y=concB))+
	geom_braid_smooth(aes(fill=measure))+
	geom_point(colour="black")+
	geom_braid_contour(aes(z=measure),breaks=10*(1:9),colour="black",linetype=2)+
	scale_x_log10("Drug A")+
	scale_y_log10("Drug B")+
	scale_fill_distiller("Effect",palette="RdYlBu")+
	coord_equal()

ggplot(glassSurface,aes(x=concA,y=concB))+
	geom_braid_smooth(aes(fill=measure,width=tilewidth,height=tileheight),space=2)+
	geom_braid_contour(aes(z=measure,width=tilewidth,height=tileheight),space=2,
					   breaks=10*(1:9),colour="black",linetype=2)+
	scale_x_log10("Drug A")+
	scale_y_log10("Drug B")+
	scale_fill_distiller("Effect",palette="RdYlBu")+
	coord_equal()

## ----warning=FALSE------------------------------------------------------------
surface$type <- "Synergy"
glassSurface$type <- "Antagonism"
allSurface <- rbind(surface,glassSurface)
allSurface$type <- factor(allSurface$type,c("Synergy","Antagonism"))

ggplot(allSurface,aes(x=concA,y=concB,colour=type))+
	geom_point()+
	geom_braid_contour(aes(z=measure,width=tilewidth,height=tileheight),
					   breaks=c(50,90), tight=TRUE)+
	scale_x_log10()+
	scale_y_log10()+
	scale_color_brewer("Surface Type",palette="Set1")+
	coord_equal()+
	labs(x="Drug A",y="Drug B")

## -----------------------------------------------------------------------------
# With warnings enabled...
ggplot(surface,aes(x=concA,y=concB))+
	geom_braid(aes(fill=measure))+
	scale_x_log10()+
	scale_y_log10()+
	scale_fill_distiller(palette="RdYlBu")+
	coord_equal()+
	labs(x="Drug A",y="Drug B",fill="Effect")

