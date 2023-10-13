##Felipe
ReverseCoding_scale10<-function(data,VarName){
        output<-as.numeric()
        for(i in seq_along(data[[VarName]])){
                if(is.na(data[i,VarName])){
                        output[i]<-NA
                }else{
                        output[i] <- (-1) * data[i, VarName] + 10
                }
        }
        data<-cbind(data,output)
        return(data)
}

##Meijun
ReverseCoding_scale10<-function(data,VarName){
        output<-as.numeric()
        for(i in seq_along(data[[VarName]])){
                ifelse(is.na(data[i,VarName]), output[i]<-NA, 
                       output[i]<-(-1)*data[i,VarName]+10)
        }
        data<-cbind(data,output)
        return(data)
}

##Example to use the function above
##The variable must have quotation mark
ESS_immi<-ReverseCoding_scale10(data = ESS10_BE_immigrant,VarName = "imwbcnt")
