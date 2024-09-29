## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(braidReports)
set.seed(20240901)

## -----------------------------------------------------------------------------
ggplot(merckValues_unstable,aes(x=kappa,y=factor(1)))+
	geom_jitter()+
	geom_violin(fill="#f3aaa9")+
	scale_x_continuous("BRAID kappa")

## -----------------------------------------------------------------------------
ggplot(merckValues_unstable,aes(x=kappa,y=factor(1)))+
	geom_jitter()+
	geom_violin(fill="#f3aaa9")+
	scale_x_kappa("BRAID kappa")

## -----------------------------------------------------------------------------
ggplot(merckValues_stable,aes(x=kappa,y=factor(1)))+
	geom_jitter()+
	geom_violin(fill="#9db2cb")+
	scale_x_kappa("BRAID kappa")

## ----fig.height=7, fig.width=8------------------------------------------------
merckValues_full <- merckValues_stable
merckValues_full[,c("drugA","drugB")] <- merckValues_stable[,c("drugB","drugA")]
merckValues_full <- rbind(merckValues_stable,merckValues_full)

ggplot(merckValues_full,aes(x=kappa,y=IAE))+
	geom_density2d()+
	geom_vline(xintercept = 0,colour="black",linetype=2)+
	scale_x_kappa("BRAID kappa",labels=as.character)+
	scale_y_log10("IAE")+
	facet_wrap(vars(drugB),ncol=6)

