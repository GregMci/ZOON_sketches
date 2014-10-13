x1 <- c(10,30,50,70,90)		# lower corner for box (Xaxis)
x2 <- c(20,40,60,80,100)	# top corner for box (Xaxis)
xG = x2[1]-x1[1]			# gap between boxes (Xaxis)

yM = 50	# Y val for middle line 
yL = 105	# Y val for title labels, e.g. 'x.module'
yH = 10	# height of boxes (Y axis)
yG = 5	# gap betwen boxes (Yaxis)
yLG = 0	# gap between lines (Yaxis)

# module labels
	ModuleLabels <- function(x1, yL, colr){
		segments(x1[1], yL+8, x2[5]+yG*0.5, yL+8, col=colr)
		text(x1[1], yL, "occurrence.module", 	col=colr, adj=0, font = 2)
		text(x1[2], yL, "covariate.module", 	col=colr, adj=0, font = 2)
		text(x1[3], yL, "process.module", 		col=colr, adj=0, font = 2)
		text(x1[4], yL, "model.module", 		col=colr, adj=0, font = 2)
		text(x1[5], yL, "output.module", 		col=colr, adj=0, font = 2)
		for(k in 1:5) {
			segments(x1[k], yL-5, x2[k]+yG*0.5, yL-5, col=colr)
			if(IsChain[k]==T) text(x1[k], yL-10, "Chain", col=colr, adj=0, font=3)
			if(IsList[k]==T) 	text(x1[k], yL-10, "List", col=colr, adj=0, font=3)
		}
		segments(x1[1], -10, x2[5]+yG*0.5, -10, col=colr)
	}

Boxed <- function( NoOfModules, InModuleList, IsList, x1, x2, xG, yM, yL, yH, yG, yLG, clrs, lW){

ListAlready = 0
ModuleInc    = 0
ColCode      = 1
clrsTXT = "black"
wInc = -35

for(i in 1:5){
	if( i!=1)	segments(x1[i]-xG*0.66, yM, x1[i]-xG*0.33, yM, lwd=lW, col=clrs[ColCode])
	yStart = yM - ( yH*0.5*NoOfModules[i] )  - ( yG*0.5*(NoOfModules[i]-1) ) 
	yNow = yStart 
	yLineStart = yM-(yLG*0.5*(NoOfModules[i]-1))
	yLnow = yLineStart
	for(N in 1:NoOfModules[i]){
		ModuleInc = ModuleInc +1
		if( IsList[i] == T && ListAlready == 1) ListAlready=2
		if(ListAlready != 2){
			if(InModuleList[ModuleInc] == T ) ColCode=1 else ColCode=2
		}else{
			if(InModuleList[ModuleInc] == T ) ColCode=3 else ColCode=4
			clrsTXT = "white"			
		}
		if(IsList[i]==T){
			segments(	x1[i]-(xG*0.33), yStart+0.5*yH, 
					x1[i]-xG*0.33,   yStart+(0.5*yH)+((NoOfModules[i]-1)*(yH+yG)), 
					lwd=lW, col=clrs[ColCode])
			segments(	x2[i]+(xG*0.33), yStart+0.5*yH, 
					x2[i]+xG*0.33,   yStart+(0.5*yH)+((NoOfModules[i]-1)*(yH+yG)), 
					lwd=lW, col=clrs[ColCode])
			segments(	x1[i]-xG*0.33, yNow+0.5*yH, 
					x1[i], yNow+0.5*yH, lwd=lW, col=clrs[ColCode])
			segments(	x2[i], yNow+0.5*yH, 
					x2[i]+xG*0.33, yNow+0.5*yH, lwd=lW, col=clrs[ColCode])
			if(ListAlready==2){
					segments( x1[i]-(xG*0.5), 5, x1[i]-xG*0.5, yM, 
					lwd=1.5, lty= 3, col="cornsilk4")
					segments(x1[1]+0.5*xG, 5, x1[i]-xG*0.5, 5, lwd=1.5, lty= 3, col="cornsilk4")
					text (x1[1]+0.5*xG, 0, "*, only 1 list permitted per workflow", col="cornsilk4", adj=0, cex=0.8)
				ListAlready=2
			}
		}else{
			if(IsChain[i]==T){
				if(i!=1 && N==NoOfModules[i]) segments(x1[i]-xG*0.33, yLnow, x1[i], yNow+0.5*yH, lwd=lW, col=clrs[ColCode])
				if(i!=5 && N==1) segments(x2[i], yNow+0.5*yH, x2[i]+xG*0.33, yLnow, lwd=lW, col=clrs[ColCode])			
				if(N!=NoOfModules[i]) segments(	0.5*(x1[i]+x2[i]), 
										yNow+yH, 0.5*(x1[i]+x2[i]), yNow+yH+yG, lwd=lW, col=clrs[ColCode])			
			}else{
				if(i!=1 ) segments(x1[i]-xG*0.33, yLnow, x1[i], yNow+0.5*yH, lwd=lW, col=clrs[ColCode])
				if(i!=5 ) segments(x2[i], yNow+0.5*yH, x2[i]+xG*0.33, yLnow, lwd=lW, col=clrs[ColCode])			
			}
		}
		rect(x1[i], yNow , x2[i], yNow+yH, col=clrs[ColCode], border=clrs[ColCode], lwd=lW)
		text (x1[i], yNow+0.5*yH, "moduleName.module", col=clrsTXT, adj=0, cex=0.75)

		if( ColCode%%2==0){
			text(x1[1]+(0.5*(x2[1]-x1[1])), -18, "** Modules not found in the repoistory", col="cornsilk4", adj=0, cex=0.8)
			rect(x1[1]+(0.5*(x2[1]-x1[1])), wInc , x2[1]+(0.5*(x2[1]-x1[1])), wInc+yH, col=clrs[ColCode], border=clrs[ColCode], lwd=lW)
			text (x1[1]+(0.5*(x2[1]-x1[1])), wInc+0.5*yH, "moduleName.module", col=clrsTXT, adj=0, cex=0.75)
			wInc = wInc-(yH+yG)
		}

		yNow = yNow + yG + yH
		yLnow = yLnow + yLG
	}
	if( IsList[i] == T && ListAlready == 0) ListAlready=1
}
} # end function 'Boxed'


# fake a number of modules per workflow stage
	NoOfModules  <- c(1,4,2,5,2)
# fake a list for if the module is in repository
	InModuleList <- rep(T, sum(NoOfModules)) 
	InModuleList [c(4,8,9,12)] = F
# fake a list of if workflow stage is a chain (>1 chain allowed)
	IsChain <- c( F, F, T, F, T)
# fake a list of if workflow stage is a list (only 1 list allowed per worfkflow)
	IsList <- c( F, T, F, T, F)

#x11()

par(mar=c(0,0,0,0))
plot(-99,-99, xlim=c(0,110), ylim=c(-100,125),xlab="", ylab="", axes=F )

clrs <- c("deepskyblue2", "tomato", "deepskyblue4", "tomato3")
clrs1 <- c("cornsilk", "tomato", "cornsilk4", "tomato3")
clrs2 <- c("black", NA, "black", NA)
clrs3 <- c("cornsilk", NA, "cornsilk", NA)

rect(-200,-200,200,200, col="cornsilk1", border=F)
text(x1[1], 120, "Your.Workflow/doi: 999/lipsum/xxx", font=3, cex=1.5, col="cornsilk4", adj=0)
ModuleLabels(x1, yL, colr="wheat4")
lW=3
Boxed(NoOfModules, InModuleList, IsList, x1, x2, xG, yM, yL, yH, yG, yLG, clrs2, lW)
lW=3
Boxed(NoOfModules, InModuleList, IsList, x1, x2, xG, yM, yL, yH, yG, yLG, clrs, lW)


