tickets <- read_csv("data/tickets_dat.csv")
tickets<-tickets |> rename(Acct_Dept=`Acct/Dept`, Resp_Group=`Resp Group` )
tickets |> mutate(Type=if_else(str_detect(Type, "VU Account"),"VU Account", Type),
                  Type = if_else(str_detect(Type, "Authen"),"Authenticator", Type),
                  Type = if_else(str_detect(Type, "Microsoft"),"Microsoft Service", Type),
                  Type = if_else(str_detect(Type, "Hardware"),"Hardware", Type),
                  Type = if_else(str_detect(Type, "Computer"),"Computer", Type),
                  Type = if_else(str_detect(Type, "Print"),"Printer", Type),
                  Type = if_else(str_detect(Type, "Wireless"),"Wireless Network", Type),
                  Type = if_else(str_detect(Type, "Adobe"),"Adobe", Type)) |> unique()


library(nnet)
library(tidyverse)
library(rpart)


fit1 <- multinom(data = train_ticket, Type ~ Resp_Group+Acct_Dept)
df1 <- test_ticket |> select(Acct_Dept,Resp_Group) |> slice(1:50) |> 
  filter_out(Resp_Group == "Law IT")
