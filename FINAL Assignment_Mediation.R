##Read the data from SPSS in:
ESS7_DE<-haven::read_sav("./REAL analysis - ESS7_DE.sav")

##Define the Mediation Model
model_mediation<-'
Immi_EcoConcern_Mean~a*eduyrs
DEMAND_mean~c*eduyrs+b*Immi_EcoConcern_Mean
Indirect:=a*b
Total:=a*b+c
'

fit_mediation<-sem(model = model_mediation,
                    data = ESS7_DE)
summary(fit_mediation, standardized=TRUE)
