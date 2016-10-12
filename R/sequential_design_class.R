alphaspendf= function(t) 0.025*t^4
betaspendf=function(t) .15*t^3


sequential.design<-setClass("sequential.design",representation(lower.boundary="numeric",	
                                            upper.boundary="numeric",
                                            times="numeric",
                                            noncentrality="numeric",
                                            base.alpha.spend="function",
                                            base.beta.spend="function",
                                            base.alpha.spend.string="character",
                                            base.beta.spend.string="character",
					    current.look="numeric",
					    current.alpha.spend="numeric",
              current.beta.spend="numeric",
					    times.history="numeric",
					    alpha.spent.history="numeric",
					    beta.spent.history="numeric",
					    alpha.func.history="numeric",
					    beta.func.history="numeric",
                                            date.stamp="POSIXct"),
prototype=list(lower.boundary=0, 
		upper.boundary=0,
		times=c(0.33,0.67,1),
		noncentrality=(qnorm(0.975)+qnorm(.80)),
		base.alpha.spend=alphaspendf,
		base.beta.spend=betaspendf,
		base.alpha.spend.string="",
    base.beta.spend.string="",
		current.look=0,
		current.alpha.spend=1,
    current.beta.spend=1,
		times.history=0,
		alpha.spent.history=0,
		beta.spent.history=0,
		alpha.func.history=1,
		beta.func.history=1,
		power=0.8,
		date.stamp=Sys.time()
                ),

validity=function(object)
        {
                
		if(!all(object@times>=0)) {
                        return("Negative time.")
                }
		if((!all(object@base.alpha.spend(seq(0,1,by=0.01))<=1))|(!all(object@base.alpha.spend(seq(0,1,by=0.01))>=0))) {
                        return("Alpha spending function error")
                }
		if((!all(object@base.beta.spend(seq(0,1,by=0.01))<=1))|(!all(object@base.beta.spend(seq(0,1,by=0.01))>=0))) {
                        return("Beta spending function error")
                }
		return(TRUE)
        }

)

setGeneric(name="setTimes",
                       def=function(theObject,time0)
                       {
                               standardGeneric("setTimes")
                       }
                       )

setMethod(f="setTimes",
                      signature="sequential.design",
                      definition=function(theObject,time0)
                      {
                              theObject@times <- time0
                              return(theObject)
                      }
                      )


setGeneric(name="setNoncentrality",
                       def=function(theObject,noncent)
                       {
                               standardGeneric("setNoncentrality")
                       }
                       )

setMethod(f="setNoncentrality",
                      signature="sequential.design",
                      definition=function(theObject,noncent)
                      {
                              theObject@noncentrality <- noncent
                              return(theObject)
                      }
                      )

setGeneric(name="setBaseAlphaspendf",
                       def=function(theObject,funct0)
                       {
                               standardGeneric("setBaseAlphaspendf")
                       }
                       )

setMethod(f="setBaseAlphaspendf",
                      signature="sequential.design",
                      definition=function(theObject,funct0)
                      {
                              theObject@base.alpha.spend <- funct0
                              return(theObject)
                      }
                      )


setGeneric(name="setAlphaspendfString",
                       def=function(theObject,string0)
                       {
                               standardGeneric("setAlphaspendfString")
                       }
                       )

setMethod(f="setAlphaspendfString",
                      signature="sequential.design",
                      definition=function(theObject,string0)
                      {
                              theObject@base.alpha.spend.string <- string0
                              return(theObject)
                      }
                      )


setGeneric(name="setBaseBetaspendf",
                       def=function(theObject,funct0)
                       {
                               standardGeneric("setBaseBetaspendf")
                       }
                       )

setMethod(f="setBaseBetaspendf",
                      signature="sequential.design",
                      definition=function(theObject,funct0)
                      {
                              theObject@base.beta.spend <- funct0
                              return(theObject)
                      }
                      )

setGeneric(name="setBetaspendfString",
                       def=function(theObject,string0)
                       {
                               standardGeneric("setBetaspendfString")
                       }
                       )

setMethod(f="setBetaspendfString",
                      signature="sequential.design",
                      definition=function(theObject,string0)
                      {
                              theObject@base.beta.spend.string <- string0
                              return(theObject)
                      }
                      )

setGeneric(name="setDatestamp",
                       def=function(theObject,date0)
                       {
                               standardGeneric("setDatestamp")
                       }
                       )

setMethod(f="setDatestamp",
                      signature="sequential.design",
                      definition=function(theObject,date0)
                      {
                              theObject@date.stamp <- date0
                              return(theObject)
                      }
                      )

setGeneric(name="setCurrentLook",
                       def=function(theObject,look0)
                       {
                               standardGeneric("setCurrentLook")
                       }
                       )

setMethod(f="setCurrentLook",
                      signature="sequential.design",
                      definition=function(theObject,look0)
                      {
			if (look0<=theObject@current.look) return(theObject)
			normalizedTimes<-theObject@times/max(theObject@times)
			
			a1<-theObject@base.alpha.spend(1)
			b1<-theObject@base.beta.spend(1)
			if (theObject@current.look>0) 
				{a2<-theObject@current.alpha.spend
				b2<-theObject@current.beta.spend
				}
				
				
			alpha_spend<-function(z) {
				if (theObject@current.look>0) return(a2*theObject@base.alpha.spend(z)+a1*(1-a2))
				else return(theObject@base.alpha.spend(z))}

			beta_spend<-function(z) {if (theObject@current.look>0) return (b2*theObject@base.beta.spend(z)+b1*(1-b2))
					else return(theObject@base.beta.spend(z))}
			
			if (theObject@current.look>0)                        
				{theObject@alpha.spent.history<-c(theObject@alpha.spent.history,alpha_spend(normalizedTimes[(theObject@current.look+1):look0]))
				theObject@beta.spent.history<-c(theObject@beta.spent.history,beta_spend(normalizedTimes[(theObject@current.look+1):look0]))
			
				theObject@alpha.func.history<-c(theObject@alpha.func.history,rep(theObject@current.alpha.spend,(look0-theObject@current.look)))
				theObject@beta.func.history<-c(theObject@beta.func.history,rep(theObject@current.beta.spend,(look0-theObject@current.look)))
				}
				else if (theObject@current.look==0){
				theObject@alpha.spent.history<-alpha_spend(normalizedTimes[(theObject@current.look+1):look0])
				theObject@beta.spent.history<-beta_spend(normalizedTimes[(theObject@current.look+1):look0])
			
				theObject@alpha.func.history<-rep(theObject@current.alpha.spend,(look0-theObject@current.look))
				theObject@beta.func.history<-rep(theObject@current.beta.spend,(look0-theObject@current.look))
				}
	
			theObject@times.history<-theObject@times[1:look0]
			theObject@current.look <- look0
			return(theObject)
                      }
                      )

setGeneric(name="getProbabilities",
                       def=function(theObject)
                       {
                               standardGeneric("getProbabilities")
                       }
                       )

setMethod(f="getProbabilities",
                      signature="sequential.design",
                      definition=function(theObject)
                      {
				normalizedTimes<-theObject@times/max(theObject@times)
				a1<-theObject@base.alpha.spend(1)
				b1<-theObject@base.beta.spend(1)
				if (theObject@current.look>0) 
					{a2<-theObject@alpha.func.history[theObject@current.look]
					b2<-theObject@beta.func.history[theObject@current.look]
					}
				
				
		
				alpha_spend<-function(z) {
					if (theObject@current.look>0) return(a2*theObject@base.alpha.spend(z)+a1*(1-a2))
					else return(theObject@base.alpha.spend(z))}

				beta_spend<-function(z) {if (theObject@current.look>0) return (b2*theObject@base.beta.spend(z)+b1*(1-b2))
					else return(theObject@base.beta.spend(z))}
				n.int<-1000*rep(1,length(normalizedTimes))
				theObject@upper.boundary<-alphaspend(alpha_spend(normalizedTimes),t=normalizedTimes,int=n.int)
				theObject@lower.boundary<-betaspend(beta_spend(normalizedTimes),theObject@upper.boundary,t=normalizedTimes,noncent=theObject@noncentrality)
				under_null<-seqmon(theObject@lower.boundary,theObject@upper.boundary,normalizedTimes)
				under_alt<-seqmon(theObject@lower.boundary-theObject@noncentrality*sqrt(normalizedTimes),theObject@upper.boundary-theObject@noncentrality*sqrt(normalizedTimes),normalizedTimes)
				probs_null<-matrix(under_null,ncol=2)
				colnames(probs_null)<-c("futility","efficacy")
				probs_alt<-matrix(under_alt,ncol=2)
				colnames(probs_alt)<-c("futility","efficacy")
				P.vals<-matrix(c((1-pnorm(theObject@lower.boundary,0,1)),(1-pnorm(theObject@upper.boundary,0,1))),ncol=2)
				colnames(P.vals)<-c("futility","efficacy")
				return(list(cumProb.null=probs_null,cumProb.alt=probs_alt,pVal=P.vals))
                      }
                      )

setGeneric(name="calcBoundaries",
                       def=function(theObject)
                       {
                               standardGeneric("calcBoundaries")
                       }
                       )

setMethod(f="calcBoundaries",
                      signature="sequential.design",
                      definition=function(theObject)
                      {
				
				normalizedTimes<-theObject@times/max(theObject@times)
				a1<-theObject@base.alpha.spend(1)
				b1<-theObject@base.beta.spend(1)
				
				if (theObject@current.look>0) 
					{a2<-theObject@alpha.func.history[theObject@current.look]
					b2<-theObject@beta.func.history[theObject@current.look]
					}
				
				
		
				alpha_spend<-function(z) {
					if (theObject@current.look>0) return(a2*theObject@base.alpha.spend(z)+a1*(1-a2))
					else return(theObject@base.alpha.spend(z))}

				beta_spend<-function(z) {if (theObject@current.look>0) return (b2*theObject@base.beta.spend(z)+b1*(1-b2))
					else return(theObject@base.beta.spend(z))}
				n.int<-1000*rep(1,length(normalizedTimes))
				theObject@upper.boundary<-alphaspend(alpha_spend(normalizedTimes),t=normalizedTimes,int=n.int)
				theObject@lower.boundary<-betaspend(beta_spend(normalizedTimes),theObject@upper.boundary,t=normalizedTimes,noncent=theObject@noncentrality)
				
				return(theObject)
                      }
                      )

setGeneric(name="plotBoundaries",
                       def=function(theObject)
                       {
                               standardGeneric("plotBoundaries")
                       }
                       )

setMethod(f="plotBoundaries",
                      signature="sequential.design",
                      definition=function(theObject)
                      { n_looks<-length(theObject@lower.boundary)
			dat<-matrix(cbind(theObject@upper.boundary,theObject@lower.boundary),ncol=2)
			matplot(dat,main="Efficacy and Futility Boundaries",xlab="Look",ylab="Z",type = c("b"),
			ylim=c(min(dat)-0.5,max(dat)+0.5),xlim=c(1,(n_looks+0.5)),pch=1,col = 1:2,xaxt="n")
			axis(1, at=1:n_looks)
			legend("topright", legend = c("Efficacy","Futility"), col=1:2, pch=1)	
				
                      }
                      )

setGeneric(name="printSummary",
                       def=function(theObject)
                       {
                               standardGeneric("printSummary")
                       }
                       )

setMethod(f="printSummary",
                      signature="sequential.design",
                      definition=function(theObject)
                      { normalizedTimes<-theObject@times/max(theObject@times)
			n.int<-1000*rep(1,length(normalizedTimes))
			
			under_null<-matrix(seqmon(theObject@lower.boundary,theObject@upper.boundary,normalizedTimes,int=n.int),ncol=2)
			colnames(under_null)<-c("Cumulative prob of futility under null hypothesis","Cumulative prob of efficacy under null hypothesis")
			under_alt<-matrix(seqmon(theObject@lower.boundary-theObject@noncentrality*sqrt(normalizedTimes),theObject@upper.boundary-theObject@noncentrality*sqrt(normalizedTimes),normalizedTimes,int=n.int),ncol=2)
			colnames(under_alt)<-c("Cumulative prob of futility under alternative hypothesis","Cumulative prob of efficacy under alternative hypothesis")
			pVal.upper<-matrix(1-pnorm(theObject@upper.boundary,0,1),ncol=1)
			colnames(pVal.upper)<-c("p-value for efficacy")
			pVal.lower<-matrix(1-pnorm(theObject@lower.boundary,0,1),ncol=1)
			colnames(pVal.lower)<-c("p-value for futility")
			lower<-matrix(theObject@lower.boundary,ncol=1)
			colnames(lower)<-c("Futility boundary for Z")
			upper<-matrix(theObject@upper.boundary,ncol=1)
			colnames(upper)<-c("Efficacy boundary for Z")

			print(t(format(round(cbind(lower,pVal.lower,upper,pVal.upper,under_null,under_alt),6), nsmall = 1)))
				
                      }
                      )


setGeneric(name="curtailDesign",
                       def=function(theObject,current0)
                       {
                               standardGeneric("curtailDesign")
                       }
                       )

setMethod(f="curtailDesign",
                      signature="sequential.design",
                      definition=function(theObject,current0)
                      {		
				normalizedTimes<-theObject@times/max(theObject@times)
        			prob<-curtail(theObject@lower.boundary,theObject@upper.boundary,theObject@current.look,normalizedTimes,theObject@noncentrality,current0)
				return(prob)
                      }
                      )



setGeneric(name="updateDesign",
                       def=function(theObject,futureTimes)
                       {
                               standardGeneric("updateDesign")
                       }
                       )

setMethod(f="updateDesign",
                      signature="sequential.design",
                      definition=function(theObject,futureTimes)
                      {
				if (!all(!is.na(futureTimes))) return()
				else if (min(futureTimes)<=max(theObject@times.history)) return()
				
				else if (theObject@current.look==0){
						theObject<-setTimes(theObject,futureTimes)
						theObject<-calcBoundaries(theObject)
						theObject@date.stamp=Sys.time()
						return(theObject)
						} 
						else 
						{old.alpha.spendf<-function(z)
							{if (theObject@current.look>0) return(theObject@base.alpha.spend(1)*(1-theObject@current.alpha.spend)+theObject@current.alpha.spend*theObject@base.alpha.spend(z))
					  		if (theObject@current.look==0) return(theObject@base.alpha.spend(z))						
							}
						old.beta.spendf<-function(z)
							{if (theObject@current.look>0) return(theObject@base.beta.spend(1)*(1-theObject@current.beta.spend)+theObject@current.beta.spend*theObject@base.beta.spend(z))
							if (theObject@current.look==0) return(theObject@base.beta.spend(z))
							}
				
						Tmax.old<-max(theObject@times)
						Tmax.new<-max(futureTimes)
				
						if (theObject@current.look>0) theObject@times<-c(theObject@times.history,futureTimes)
						if (theObject@current.look==0) theObject@times<-futureTimes
	
						new.Normalized.t<-theObject@times/Tmax.new
				
						upper.boundary<-c(theObject@upper.boundary[1:theObject@current.look],rep(0,length(futureTimes)))
						lower.boundary<-c(theObject@lower.boundary[1:theObject@current.look],rep(0,length(futureTimes)))
					
				
						if (Tmax.new<=Tmax.old)
							{new.alpha.spendf<-old.alpha.spendf
							levels.alpha<-new.alpha.spendf(new.Normalized.t)
					
					
							new.beta.spendf<-old.beta.spendf
							levels.beta<-new.beta.spendf(new.Normalized.t)
							
					
							for (i in (theObject@current.look+1):length(new.Normalized.t))
								{
								ff_a<-function(x){return(seqmon(rep(-80,i),
                                 				c(upper.boundary[1:(i-1)],x),new.Normalized.t[1:i],1000*rep(1,i))[2*i]-levels.alpha[i])}
    								bs_a=try(uniroot(ff_a,c(-80,80),tol=0.000001)$root)
								if (class(bs_a)=="try-error") 
									{upper.boundary[i]=80
									}
									else
									{upper.boundary[i]=bs_a
									}	
								ff_b=function(x) {return(seqmon(c(lower.boundary[1:(i-1)],x)-theObject@noncentrality*sqrt(new.Normalized.t[1:i]),
                                  					upper.boundary[1:i]-theObject@noncentrality*sqrt(new.Normalized.t[1:i]),new.Normalized.t[1:i],1000*rep(1,i))[i]-levels.beta[i])}
    								bs_b=try( uniroot(ff_b,c(-80,80),tol=0.000001)$root)
    								if (class(bs_b)=="try-error") 
									{lower.boundary[i]=-80 
									}
    									else 
									{lower.boundary[i]=bs_b
  									}
						
								} #for (i in (theObject@current.look+1):length(new.Normalized.t))
							new_a<-theObject@current.alpha.spend
							new_b<-theObject@current.beta.spend
							} #if (Tmax.new<=Tmax.old)
							else
							{
							if (theObject@current.look>0) 
								{b1<-theObject@beta.spent.history[theObject@current.look]
								b2<-old.beta.spendf(new.Normalized.t[theObject@current.look])
								a1<-theObject@alpha.spent.history[theObject@current.look]
								a2<-old.alpha.spendf(new.Normalized.t[theObject@current.look])
								}
							if (theObject@current.look==0) 
								{b1<-0
								b2<-0
								a1<-0
								a2<-0
								}
												
							alpha0<-old.alpha.spendf(1)
							beta0<-old.beta.spendf(1)
					
							new.alpha.spendf<-function(z)
								{
								return(a1+((alpha0-a1)/(alpha0-a2))*(old.alpha.spendf(z)-a2))
								}
							levels.alpha<-new.alpha.spendf(new.Normalized.t)
					
					
							new.beta.spendf<-function(z)
								{
								#print(paste("beta0=",beta0,";b1=",b1,";b2=",b2))
								return(b1+((beta0-b1)/(beta0-b2))*(old.beta.spendf(z)-b2))
								}
							levels.beta<-new.beta.spendf(new.Normalized.t)
							
							
							for (i in (theObject@current.look+1):length(new.Normalized.t))
								{
								ff_a<-function(x){return(seqmon(rep(-100,i),
                                 					c(upper.boundary[1:(i-1)],x),new.Normalized.t[1:i],1000*rep(1,i))[2*i]-levels.alpha[i])}
								bs_a=try(uniroot(ff_a,c(-80,80),tol=0.000001)$root)
								if (class(bs_a)=="try-error") 
									{upper.boundary[i]=80
									}
									else
									{upper.boundary[i]=bs_a
									}	
								
								ff_b=function(x) {
									return(seqmon(c(lower.boundary[1:(i-1)],x)-theObject@noncentrality*sqrt(new.Normalized.t[1:i]),
                                  						upper.boundary[1:i]-theObject@noncentrality*sqrt(new.Normalized.t[1:i]),new.Normalized.t[1:i],1000*rep(1,i))[i]-levels.beta[i])}
								bs_b=try( uniroot(ff_b,c(-80,80),tol=0.000001)$root)
    								if (class(bs_b)=="try-error") 
									{lower.boundary[i]=-80 
									}
    									else 
									{lower.boundary[i]=bs_b
  									}
						
								} #for (i in (theObject@current.look+1):length(new.Normalized.t))
					
							new_a<-(alpha0-a1)/(alpha0-a2)*theObject@current.alpha.spend
							new_b<-(beta0-b1)/(beta0-b2)*theObject@current.beta.spend
					
							} #if (Tmax.new>Tmax.old)
				
				
				
							theObject@upper.boundary<-upper.boundary
							theObject@lower.boundary<-lower.boundary
				
							theObject@current.alpha.spend<-new_a
							theObject@current.beta.spend<-new_b
						}#else if (theObject@current.look==0) , else if (min(futureTimes)<=max(theObject@times.history))
					theObject@date.stamp=Sys.time()
					return(theObject)
		} #definition
  	)



#modified version of that in seqmon.R, fix bug for length=1 for times

alphaspend<-function(levels,t,int=rep(500,length(t)),tol=0.005){
  
  dimLevels=length(levels)
  boundary=rep(0,dimLevels)
  boundary[1]=qnorm(1-levels[1])
  if (boundary[1]==Inf) boundary[1]<-30
  if (dimLevels>1)
	{for (i in 2:dimLevels)
		{ff=function(x){return(seqmon(rep(-20,i),
                                 c(boundary[1:(i-1)],x),t[1:i],int[1:i])[2*i]-levels[i])}
    		bs=try(uniroot(ff,c(-30,30),tol=tol)$root)
		if (class(bs)=="try-error") 
			{boundary[i]<-30 }
			else boundary[i]=bs  	
  		}#for (i in 2:dimLevels)
	} #if (dimLevels>1)
		
  return(boundary)
}

#modified version of that in seqmon.R, fix bug for length=1 for times

betaspend<-function(levels,upperboundary,t,int=rep(500,length(t)),noncent,tol=0.005){
  dimLevels=length(levels)
  boundary=rep(0,dimLevels)
  boundary[1]=qnorm(levels[1])+noncent*sqrt(t[1])
  if (boundary[1]==-Inf) boundary[1]<--30
  if (dimLevels>1)
	{for(i in 2:dimLevels)
		{ff=function(x) {return(seqmon(c(boundary[1:(i-1)],x)-noncent*sqrt(t[1:i]),
                                  upperboundary[1:i]-noncent*sqrt(t[1:i]),t[1:i],int[1:i])[i]-levels[i])}
    		bs=try( uniroot(ff,c(-30,30),tol=tol)$root)
    		if (class(bs)=="try-error") 
			{boundary[i]<--30 
			}
    			else boundary[i]=bs
  		}#for(i in 2:dimLevels)
 	} #if (dimLevels>1)
return(boundary)
}

#seqmon calculates the cumulative probabilities
seqmon<-function (a, b, t, int=rep(500,length(t))) 
{
  ones <- function(a, b) {
    array(rep(1, a * b), c(a, b))
  }
  normcdf <- function(xx) {
    pnorm(xx)
  }
  d <- (b - a)/int
  m <- length(a)
  pU = ones(m, 1)
  pL = ones(m, 1)
  sq2pi <- sqrt(2 * pi)
  H <- 1:int[1]
  E <- ones(1, int[1])
  xo <- a[1] + ((1:int[1]) - 0.5 * E) * d[1]
  pU[1] <- normcdf(-(sqrt(t[1]) * b[1])/sqrt(t[1]))
  M <- t((d[1]/sq2pi) * exp(-(sqrt(t[1]) * xo)^2/(2 * t[1])))
  pL[1] <- normcdf(sqrt(t[1]) * a[1]/sqrt(t[1]))
  if(m>1){for (k in 2:m) {
    VU <- normcdf(-(sqrt(t[k]) * b[k] * E - sqrt(t[k - 1]) * 
                      xo)/sqrt(t[k] - t[k - 1]))
    VL <- normcdf((sqrt(t[k]) * a[k] * E - sqrt(t[k - 1]) * 
                     xo)/sqrt(t[k] - t[k - 1]))
    pL[k] <- pL[k - 1] + VL %*% M
    pU[k] <- pU[k - 1] + VU %*% M
    x <- a[k] + ((1:int[k]) - 0.5 * ones(1, int[k])) * d[k]
    M <- (d[k] * sqrt(t[k])/(sq2pi * sqrt(t[k] - t[k - 1]))) * 
      exp(-(sqrt(t[k]) * (t(x) %*% ones(1, int[k - 1])) - 
              sqrt(t[k - 1]) * (ones(int[k], 1) %*% xo))^2/(2 * 
                                                              (t[k] - t[k - 1]))) %*% M
    xo <- x
  }}
  c(pL, pU)
}


#curtail calcualtes the probability for declaring efficacy given the statistics at the current look
curtail<-function(lower.boundary,upper.boundary,look,t,noncen,current=lower.boundary[look]){
  nlooks=length(lower.boundary)
  rng=(look+1):nlooks
  tn=function(x) ifelse(x==0,0,t[x])
  newt=(t[rng]-tn(look))/(t[nlooks]-tn(look))
  mult=sqrt((t[rng]-tn(look))/t[rng])
  start=ifelse(look==0,0,current)
  ez=sqrt(tn(look)/t[rng])*start+noncen*mult*sqrt((t[rng]-tn(look))/max(t))
  prob=seqmon((1/mult)*(lower.boundary[rng]-ez),(1/mult)*(upper.boundary[rng]-ez),newt)[2*(nlooks-look)]
  return(prob)}
