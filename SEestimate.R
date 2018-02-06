###################################################################
###################################################################
#######estimating standard error of GLM/NN models via resampling
###############
#######but now looping both GLM and NN... not using cv.glm
#######now use ALL of 'diamonds' to sample from, not just a subset
#########################################################
#########################################################

####require ggplot, neuralnet, caTools, boot, plyr else stop
##pkgs<-c("ggplot2","neuralnet","caTools","boot","plyr")
##if(!(require(pkgs,quietly=T))) {stop}


####define function to compare predictive analysis between GLM and NN:
se.estimate<-function(
	########what data to model? default=diamonds dataset
		dat=NULL,
	########what is the response variable?  default="price"
		pred.var=NULL,
	####number of sample groups to draw		
		resamples=10,
	#####pct of data used for test/validation set in each re-sample. 
		test.pct=0.10,		
	#####size of test/validation sets in each re-sample groups. will override test.pct if given
		test.size=NULL,
	###percentage of re-sample data to be used as 'train' data set -- default 70%
		##train.pct=0.70,
	###select method(s) used to calculate standard error -- "glm", "nn", "both"
		method="both",
	###select type of response variable
		distrib="continuous",
	###progress bar?  T or F
		prog.bar=T)
	{

##########if no data set is given, default is 2000 row sample of 'diamonds' data set
if(is.null(dat)){
	index<-sample(nrow(diamonds),2000)
	dat<-diamonds[index,]
	pred.var<-"price"
	}

#######check argument:  'data'
if(!is.data.frame(dat)) stop("Argument `dat' must be a valid data frame")

########a little more about our (now valid) data frame:
	########how big is the data set??
	N<-nrow(dat)
	###get variable names
	names(dat)->vars

#######check argument:  'pred.var'
if(!(pred.var %in% vars)) stop("Argument `pred.var' must be a column in the given data frame")
#######check argument:  'resamples'
if(!(resamples==round(resamples) && resamples>0)) stop("Argument `resamples' must be a positive integer")

#######check argument:  'test.size' does it exist?  is it valid?  then, define test.pct=NULL to override
if(!is.null(test.size)){
	if(!(test.size==round(test.size) && test.size>0)) stop("Argument `test.size' must be a positive integer")
	if(test.size>N) stop("Argument `test.size' exceeds rows in data set")
	test.pct=NULL
	}
#######check argument:  'test.pct' is it overridden?  is it valid? then, use it to define test.size
if(!is.null(test.pct)) {
	if(!(test.pct>0 && test.pct<=1)) stop("Argument `test.pct' must be a value greater than zero and less than or equal to 1")
	test.size<-round(N*test.pct)
	}
#######check argument:  'train.pct'
##if(!(train.pct>0 && train.pct<1)) stop("Argument `train.pct' must be a value between zero and one, exclusive")
#######possible methods; make sure 'method' arg is valid
method.options<-c("glm","nn","both")
if(!(method %in% method.options)) stop("Argument `method' must be one of `glm', `nn', or `both'")
#########check argument: 'distrib'
if(!(distrib %in% c("continuous","categorical"))) stop("Argument `distrib' must be either `continuous' or `categorical'")

#######check argument: 'prog.bar'
if(!is.logical(prog.bar)) stop("Argument `prog.bar' must be logical")


##########prep data
	###identify predictive column in data frame
	which(vars==pred.var)->resp.col
	
	#####create formula of from 'pred.var ~ .' for predictive model 
	model.formula<-paste(vars[-resp.col],collapse=" + ")
	model.formula<-paste(vars[resp.col],"~",model.formula)
	model.formula<-as.formula(model.formula)

	#######convert all data to numeric -- get rid of factors!!
	dat2<-as.data.frame(lapply(dat,as.numeric))

	#####unscale info for response
	min(dat2[,resp.col])->resp.min	
	max(dat2[,resp.col])->resp.max
	#####scale data
	apply(dat2,2,min)->mins
	apply(dat2,2,max)->maxs			
	as.data.frame(scale(dat2,mins,maxs-mins))->dat2.scaled

################################
###########GLM se.estimate
################################
	######check if glm is tested
	if(method=="glm" || method=="both"){
		####initialize output variable
		glm.se.estimate<-NULL
		#####if prog.bar=T, initialize progress bar
		if(prog.bar){
			print("Re-sampling data, creating and testing models, to calculate standard error for a GLM model")
			progress.bar<-create_progress_bar('text')
			progress.bar$init(resamples)
			}
		#######train/test/compute SE  "resamples" number of times
		for(i in 1:resamples){
			###find index for re-sampling TEST set
			test.index<-sample(N,test.size)
			###find its compliment -- the TRAIN set
			train.index<-seq(1,N)[-test.index]
			####define train/set from data
			train<-dat[train.index,]
			test<-dat[test.index,]
		
			####create glm!!
				####for continuous response (assumed normal distrib)
					if(distrib=="continuous") glm.model<-glm(model.formula,,train)
				####for categorical response 
					if(distrib=="categorical") glm.model<-glm(model.formula,family=binomial(link = 'logit'),data=train)
			####predict from model:
				###for continuous response
					if(distrib=="continuous") pred.glm<-predict(glm.model,test)
				###for categorical response
					if(distrib=="categorical") pred.glm<-predict(glm.model,test,type="response")
			####actual responses
			resp.glm<-test[,resp.col]
			####calculate standard error
			glm.se.estimate[i]<-sum((pred.glm-resp.glm)^2)/nrow(test)
			####if prog.bar=T, step up
			if(prog.bar) progress.bar$step()
			}
		##end glm estimate
		}


###############################
###########NN se.estimate
###############################
	######check if nn is tested
	if(method=="nn" || method=="both"){
		####initialize output variable
		nn.se.estimate<-NULL
		#####if prog.bar=T, initialize progress bar
		if(prog.bar){
			print("Re-sampling data, creating and testing models, to calculate standard error for a NN model")
			progress.bar<-create_progress_bar('text')
			progress.bar$init(resamples)
			}
		#######train/test/compute SE  "resamples" number of times
		for(i in 1:resamples){
			###find index for re-sampling TEST set
			test.index<-sample(N,test.size)
			###find its compliment -- the TRAIN set
			train.index<-seq(1,N)[-test.index]
			####define train/set from data... use dat2.scaled for NN!
			train<-dat2.scaled[train.index,]
			test<-dat2.scaled[test.index,]

			####create neuralnet!!
			nn<-neuralnet(model.formula,train,hidden=c(10,10,10),linear.output=F)
			####predict, and re-scale:
			pred.nn<-compute(nn,test[,-resp.col])
			pred.nn<-pred.nn$net.result*(resp.max-resp.min)+resp.min
			####actual responses
			resp.nn<-test[,resp.col]*(resp.max-resp.min)+resp.min
			##resp.nn<-dat[test.index,resp.col]  ###does it work too??
			####calculate standard error
			nn.se.estimate[i]<-sum((resp.nn-pred.nn)^2)/nrow(test)
			####if prog.bar=T, step up
			if(prog.bar) progress.bar$step()
			}
		###end nn estimates
		}


#####################
###output
#######################

######if method=glm
	if(method=="glm") se.results<-list(GLM.se.estimate=glm.se.estimate,GLM.summary=summary(glm.se.estimate)) 

######if method=glm
	if(method=="nn") se.results<-list(NN.se.estimate=nn.se.estimate,NN.summary=summary(nn.se.estimate)) 

######if method=glm
	if(method=="both") se.results<-list(GLM.se.estimate=glm.se.estimate,NN.se.estimate=nn.se.estimate,GLM.summary=summary(glm.se.estimate),NN.summary=summary(nn.se.estimate)) 

####output results
se.results}
