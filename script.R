
setwd("C:/Users/kylep/Documents/Github/book-tax")
rm(list=ls())

options(scipen = 999)
assets<-read.csv("asset_data.csv", header = TRUE, fill = TRUE, sep = ",")

#Constant Parameters
  debt_finance_share <- .32
  interest_rate <- 0.068
  inflation <- 0.024
  return_on_equity<-.058
  share_lifo<-0.5
  inventory_holding_period<-1/3

#Cost recovery formulas

  straight_line<- function(discount_rate, asset_life, start_year, end_year, bonus){
    
    z_c <- bonus * max(min(1-start_year,1),0)
    
    z_a <- (1 - bonus)*((1-exp(-1*discount_rate*(min(end_year,asset_life)-start_year)))/(discount_rate*asset_life))*exp(-1*discount_rate*start_year)
    z<-z_c + z_a
    return(z)
    
  }
  
  declining_balance_sl<- function(discount_rate, asset_life, start_year, end_year, rate, bonus){
    
    beta <- rate/asset_life
    time_on_db <- asset_life*(1-(1/rate))
    
    z_c <- bonus * max(min(1-start_year,1),0)
    
    z_a<-(1-(beta/(beta))*(1-exp(-1*beta*start_year)))
    z_b<-(beta/(beta+discount_rate))*(1-exp(-1*(beta+discount_rate)*min(end_year,max(time_on_db-start_year,0))))*exp(-1*discount_rate*start_year)
    z_db<-z_a*z_b
    
    y<-min(max(end_year-time_on_db,0),1)
    z_sl<-y*((exp(-1*beta*time_on_db)/((asset_life-time_on_db)*discount_rate))*(exp(-1*discount_rate*max(time_on_db,start_year))-exp(-1*discount_rate*min(asset_life,end_year))))
    
    z<-z_c + (1-bonus)*z_db+(1-bonus)*z_sl
    
    return(z)
    
  }
  
  economic_depreciation<- function(discount_rate, start_year, end_year, rate, bonus){
    
    z_c<-bonus * max(min(1-start_year,1),0)
    
    z_a<-(1-(rate/(rate))*(1-exp(-1*rate*start_year)))
    z_b<-(rate/(rate+discount_rate-inflation))*(1-exp(-1*(rate+discount_rate-inflation)*min(end_year,max(end_year-start_year,0))))*exp(-1*discount_rate*start_year)
    
    z<-z_c + (1-bonus)*z_a*z_b
    
  }

#Service Price Formulas
  
  tax_on_profits<-function(discount_rate, start_year, end_year, corporate_tax_rate){
    
    tax <- ((1-exp(-1*discount_rate*(end_year-start_year)))/(discount_rate)) * corporate_tax_rate
    
    return(tax)
    
  }
  
  profits<-function(discount_rate, start_year, end_year){
    
    p <- ((1-exp(-1*discount_rate*(end_year-start_year)))/(discount_rate))
    
    return(p)
    
  }
  
  service_price<-function(z_present_value, discount_rate, economic_depreciation, corporate_tax_rate, r_d_credit, inflation){
    
    service_price<-(discount_rate - inflation + economic_depreciation)*(1 - corporate_tax_rate * z_present_value - r_d_credit)/ (1 - corporate_tax_rate)
    
  }
  
  amt_service_price<-function(uzp, discount_rate, economic_depreciation, profits, tax, r_d_credit, inflation, v_credit, w_credit){
    
    s<-(discount_rate - inflation + economic_depreciation)*(1 - uzp-v_credit-r_d_credit)/(1-((tax-w_credit)/profits))
    
    return(s)
    
  }
  
  inventories<-function(share_lifo, holding_period, discount_rate, inflation, corporate_tax_rate){
    
    s_fifo<- (((1/ holding_period)* log((exp(discount_rate * holding_period)- corporate_tax_rate)/ (1-corporate_tax_rate)))-inflation)
    s_lifo<- ((1/ holding_period) * log((exp((discount_rate - inflation) * holding_period)-corporate_tax_rate)/ (1-corporate_tax_rate)))
    s<- s_fifo * (1-share_lifo) + s_lifo * share_lifo
  
    return(s)
    
  }
  
  metr<-function(service_price, economic_depreciation, rate_of_return){
    
    metr<-((service_price - economic_depreciation) - (rate_of_return - inflation))/(service_price - economic_depreciation)
    
    return(metr)
    
  }

#Cost Recovery Systems
  
  cost_recovery_GDS<-function(gds_life, 
                              econ_depreciation,
                              system,
                              dep_method, 
                              discount_rate, 
                              start_year, 
                              end_year,
                              bonus){
          
          z<-0
      
          if(dep_method == "Expensing"){z <- 1*max(min(1-start_year,1),0)}
          if(dep_method == "Economic"){
            
            z<-economic_depreciation(discount_rate, start_year, end_year, econ_depreciation, bonus * (gds_life<=20))
            
          }
        
          if(dep_method =="DB 200%"){
            
            z<-declining_balance_sl(discount_rate, gds_life, start_year, end_year, 2, bonus * (gds_life<=20))
            
          }
          
          if(dep_method =="DB 150%"){
            
            z<-declining_balance_sl(discount_rate, gds_life, start_year, end_year, 1.5, bonus * (gds_life<=20))
            
          }
          
          if(dep_method =="SL"){
            
            z<-straight_line(discount_rate, gds_life, start_year, end_year, bonus  * (gds_life<=20))
            
          }
          
          if(dep_method == "Land"){z <- 0}
          
          
          return(z)
        
      }
    
  cost_recovery_ADS<-function(ads_life,
                              gds_life,
                              econ_depreciation,
                              system,
                              dep_method, 
                              discount_rate, 
                              start_year, 
                              end_year,
                              bonus){
    
    
    z<-straight_line(discount_rate, ads_life, start_year, end_year, bonus * (gds_life<=20))
    if(dep_method == "Expensing"){z <- 1*max(min(1-start_year,1),0)}
    if(dep_method == "Land"){z<-0}
    return(z)
    
  }
  
  cost_recovery_econ<-function(dep_method,
                               gds_life,
                               discount_rate, 
                               start_year, 
                               end_year, 
                               rate,
                               bonus){
    
    z<-economic_depreciation(discount_rate, start_year, end_year, rate, bonus * (gds_life<=20))
    if(dep_method == "Inventory"){z<-0}
    if(dep_method == "Land"){z<-0}
    
    return(z)
    
  }
  
  cost_recovery_book<-function(dep_method,
                               gds_life,
                               discount_rate,
                               start_year,
                               end_year,
                               rate,
                               bonus){
    
    z<-economic_depreciation(discount_rate, start_year, end_year, rate, bonus * (gds_life<=20))
    if(dep_method == "Expensing"){z <- 1*max(min(1-start_year,1),0)}
    if(dep_method == "Land"){z<-0}
    return(z)
    
  }

#Vectorize my functions
vads<-Vectorize(cost_recovery_ADS)
vecon<-Vectorize(cost_recovery_econ)
vgds<-Vectorize(cost_recovery_GDS)
vbook<-Vectorize(cost_recovery_book)
vtax_on_profits<-Vectorize(tax_on_profits)
vprofits<-Vectorize(profits)
vservice_price<-Vectorize(service_price)
vservice_price_inventories<-Vectorize(inventories)
vmetr<-Vectorize(metr)
vamt_service_price<-Vectorize(amt_service_price)

####################Permanent Tax System########################

permanent_tax<-function(corporate_tax_rate,
                        interest_limit,
                        r_d_credit,
                        depreciation_regime,
                        asset_data,
                        bonus){
  
  #discount rates
  after_tax_return<-interest_rate * debt_finance_share + (return_on_equity + inflation) * (1-debt_finance_share)
  discount_rate <- debt_finance_share * (interest_rate * (1 - (interest_limit)*corporate_tax_rate)) + 
    (1-debt_finance_share) * (return_on_equity+inflation)
  
  #Z values

    if(depreciation_regime =="ADS"){
      
      asset_data$z<-vads(ads_life = asset_data$ADS.Life,
                         gds_life = asset_data$GDS.Life,
                         econ_depreciation = asset_data$delta, 
                         system = asset_data$System, 
                         dep_method = asset_data$Method, 
                         discount_rate = discount_rate, 
                         start_year = 0, 
                         end_year = 999,
                         bonus = bonus)
      
    }
    
    if(depreciation_regime =="GDS"){
      
      asset_data$z<-vgds(gds_life = asset_data$GDS.Life, 
                         econ_depreciation = asset_data$delta, 
                         system = asset_data$System, 
                         dep_method = asset_data$Method, 
                         discount_rate = discount_rate, 
                         start_year = 0, 
                         end_year = Inf,
                         bonus = bonus)
      
    }
    
    if(depreciation_regime =="Economic"){
      
      asset_data$z<-vecon(dep_method = asset_data$Method,
                          gds_life = asset_data$GDS.Life,
                          discount_rate = discount_rate, 
                          start_year = 0, 
                          end_year = 999, 
                          rate = asset_data$delta,
                          bonus = bonus)

    }
  
    if(depreciation_regime =="Book"){
      
      asset_data$z<-vbook(dep_method = asset_data$Method,
                          gds_life = asset_data$GDS.Life,
                          discount_rate = discount_rate, 
                          start_year = 0, 
                          end_year = 999, 
                          rate = asset_data$delta,
                          bonus = bonus)
      
    }
  
  #Service Price of Capital

    asset_data$s<-vservice_price(z_present_value = asset_data$z, 
                                 discount_rate = discount_rate, 
                                 economic_depreciation = asset_data$delta, 
                                 corporate_tax_rate = corporate_tax_rate, 
                                 r_d_credit = (asset_data$major_asset_group == "Intellectual Property") * r_d_credit, 
                                 inflation = inflation)
  
    asset_data$s[asset_data$Method == "Inventory"]<-vservice_price_inventories(share_lifo = share_lifo, 
                                                                               holding_period = inventory_holding_period, 
                                                                               discount_rate = discount_rate, 
                                                                               inflation = inflation, 
                                                                               corporate_tax_rate = corporate_tax_rate)
  #METR
    
    asset_data$metr<-vmetr(service_price = asset_data$s,
                           economic_depreciation = asset_data$delta, 
                           rate_of_return = after_tax_return)
  
  return(asset_data)
  
}

##################Temporary Tax System, No AMT Credits###############

temporary_tax<-function(corporate_tax_rate_one,
                        corporate_tax_rate_two,
                        corporate_tax_rate_three,
                        interest_limit_one,
                        interest_limit_two,
                        interest_limit_three,
                        r_d_credit_one,
                        r_d_credit_two,
                        r_d_credit_three,
                        depreciation_regime_one,
                        depreciation_regime_two,
                        depreciation_regime_three,
                        asset_data,
                        bonus_one,
                        bonus_two,
                        bonus_three,
                        period_one,
                        period_two,
                        period_three){
  
  #Discount Rates
    after_tax_return<-interest_rate * debt_finance_share + (return_on_equity + inflation) * (1-debt_finance_share)
    
    discount_rate_one <- debt_finance_share * (interest_rate * (1 - (interest_limit_one)*corporate_tax_rate_one)) + 
      (1-debt_finance_share) * (return_on_equity+inflation)
    
    #Need to add the effect of generating an AMT tax credit
    discount_rate_two <- debt_finance_share * (interest_rate * (1 - (interest_limit_two)*corporate_tax_rate_two)) + 
      (1-debt_finance_share) * (return_on_equity+inflation)
    
    discount_rate_three <- debt_finance_share * (interest_rate * (1 - (interest_limit_three)*corporate_tax_rate_three)) + 
      (1-debt_finance_share) * (return_on_equity+inflation)
  
  #Z Values
    #Period 1
      if(depreciation_regime_one =="ADS"){
        
        asset_data$z_one<-vads(ads_life = asset_data$ADS.Life,
                           gds_life = asset_data$GDS.Life,
                           econ_depreciation = asset_data$delta, 
                           system = asset_data$System, 
                           dep_method = asset_data$Method, 
                           discount_rate = discount_rate_one, 
                           start_year = 0, 
                           end_year = period_one,
                           bonus = bonus_one)
        
      }
      
      if(depreciation_regime_one =="GDS"){
        
        asset_data$z_one<-vgds(gds_life = asset_data$GDS.Life, 
                           econ_depreciation = asset_data$delta, 
                           system = asset_data$System, 
                           dep_method = asset_data$Method, 
                           discount_rate = discount_rate_one, 
                           start_year = 0, 
                           end_year = period_one, 
                           bonus = bonus_one)
        
      }
      
      if(depreciation_regime_one =="Economic"){
        
        asset_data$z_one<-vecon(dep_method = asset_data$Method,
                            gds_life = asset_data$GDS.Life,
                            discount_rate = discount_rate_one, 
                            start_year = 0, 
                            end_year = period_one, 
                            rate = asset_data$delta,
                            bonus = bonus_one)
        
      }
      
      if(depreciation_regime_one =="Book"){
        
        asset_data$z_one<-vbook(dep_method = asset_data$Method,
                            gds_life = asset_data$GDS.Life,
                            discount_rate = discount_rate_one, 
                            start_year = 0, 
                            end_year = period_one, 
                            rate = asset_data$delta,
                            bonus = bonus_one)
        
      }
    
    #Period 2
      if(depreciation_regime_two =="ADS"){
        
        asset_data$z_two<-vads(ads_life = asset_data$ADS.Life,
                               gds_life = asset_data$GDS.Life,
                               econ_depreciation = asset_data$delta, 
                               system = asset_data$System, 
                               dep_method = asset_data$Method, 
                               discount_rate = discount_rate_two, 
                               start_year = period_one, 
                               end_year = period_two,
                               bonus = bonus_two)
        
      }
      
      if(depreciation_regime_two =="GDS"){
        
        asset_data$z_two<-vgds(gds_life = asset_data$GDS.Life, 
                               econ_depreciation = asset_data$delta, 
                               system = asset_data$System, 
                               dep_method = asset_data$Method, 
                               discount_rate = discount_rate_two, 
                               start_year = period_one, 
                               end_year = period_two, 
                               bonus = bonus_two)
        
      }
      
      if(depreciation_regime_two =="Economic"){
        
        asset_data$z_two<-vecon(dep_method = asset_data$Method,
                                gds_life = asset_data$GDS.Life,
                                discount_rate = discount_rate_two, 
                                start_year = period_one, 
                                end_year = period_two, 
                                rate = asset_data$delta,
                                bonus = bonus_two)
        
      }
      
      if(depreciation_regime_two =="Book"){
        
        asset_data$z_two<-vbook(dep_method = asset_data$Method,
                                gds_life = asset_data$GDS.Life,
                                discount_rate = discount_rate_two, 
                                start_year = period_one, 
                                end_year = period_two, 
                                rate = asset_data$delta,
                                bonus = bonus_two)
        
      }
  
    #Period 3
      if(depreciation_regime_one =="ADS"){
        
        asset_data$z_three<-vads(ads_life = asset_data$ADS.Life,
                               gds_life = asset_data$GDS.Life,
                               econ_depreciation = asset_data$delta, 
                               system = asset_data$System, 
                               dep_method = asset_data$Method, 
                               discount_rate = discount_rate_three, 
                               start_year = period_two, 
                               end_year = period_three,
                               bonus = bonus_three)
        
      }
      
      if(depreciation_regime_one =="GDS"){
        
        asset_data$z_three<-vgds(gds_life = asset_data$GDS.Life, 
                               econ_depreciation = asset_data$delta, 
                               system = asset_data$System, 
                               dep_method = asset_data$Method, 
                               discount_rate = discount_rate_three, 
                               start_year = period_two, 
                               end_year = period_three, 
                               bonus = bonus_three)
        
      }
      
      if(depreciation_regime_one =="Economic"){
        
        asset_data$z_three<-vecon(dep_method = asset_data$Method,
                                gds_life = asset_data$GDS.Life,
                                discount_rate = discount_rate_three, 
                                start_year = period_two, 
                                end_year = period_three, 
                                rate = asset_data$delta,
                                bonus = bonus_three)
        
      }
      
      if(depreciation_regime_one =="Book"){
        
        asset_data$z_three<-vbook(dep_method = asset_data$Method,
                                gds_life = asset_data$GDS.Life,
                                discount_rate = discount_rate_three, 
                                start_year = period_two, 
                                end_year = period_three, 
                                rate = asset_data$delta,
                                bonus = bonus_three)
        
      }
  
    #Total Z
  
      asset_data$z <- asset_data$z_one + asset_data$z_two + asset_data$z_three
  
    #The cost of capital formula expanded to:
      
      # c = (r+d)*(1 - uzp)/(1-(tax/profits))
        #where uzp is the sum of the present discounted value of depreciation deductions over the life of the asset
    
    #uzp
      
    asset_data$uzp <- asset_data$z_one*corporate_tax_rate_one + 
                      asset_data$z_two*corporate_tax_rate_two + 
                      asset_data$z_three*corporate_tax_rate_three

    #tax in each period
    
      asset_data$tax_one <- vtax_on_profits(discount_rate = discount_rate_one, 
                                            start_year = 0, 
                                            end_year = period_one, 
                                            corporate_tax_rate = corporate_tax_rate_one)
      asset_data$tax_two <- vtax_on_profits(discount_rate = discount_rate_two, 
                                            start_year = period_one, 
                                            end_year = period_two, 
                                            corporate_tax_rate = corporate_tax_rate_two)
      asset_data$tax_three <- vtax_on_profits(discount_rate = discount_rate_three, 
                                              start_year = period_two, 
                                              end_year = period_three, 
                                              corporate_tax_rate = corporate_tax_rate_three)
  
      asset_data$tax_on_profits <-  asset_data$tax_one + 
                                    asset_data$tax_two * exp(-1 * discount_rate_one * period_one) + 
                                    asset_data$tax_three * exp(-1 * discount_rate_one * period_one) * exp(-1 * discount_rate_two * (period_two-period_one))
      
    #profits in each period
      
      asset_data$profit_one<- vprofits(discount_rate = discount_rate_one, 
                                       start_year = 0, 
                                       end_year = period_one)
      asset_data$profit_two<- vprofits(discount_rate = discount_rate_two, 
                                       start_year = period_one, 
                                       end_year = period_two)
      asset_data$profit_three<-  vprofits(discount_rate = discount_rate_three, 
                                          start_year = period_two, 
                                          end_year = period_three)
      asset_data$profits <-  asset_data$profit_one + 
                             (asset_data$profit_two * exp(-1 * discount_rate_one * period_one)) + 
                             (asset_data$profit_three * exp(-1 * discount_rate_one * period_one) * exp(-1 * discount_rate_two * (period_two-period_one)))
      

    #Service Price of Capital
      
      #Weighted Average Discount Rate
      
        asset_data$discount<- 1/asset_data$profits
      
      asset_data$s<-(asset_data$discount - inflation + asset_data$delta)*(1 - asset_data$uzp - (asset_data$major_asset_group == "Intellectual Property") * r_d_credit_one)/(1-(asset_data$tax_on_profits/asset_data$profits))
    
      asset_data$s[asset_data$Method == "Inventory"]<-vservice_price_inventories(share_lifo = share_lifo, 
                                                                                 holding_period = inventory_holding_period, 
                                                                                 discount_rate = asset_data$discount[asset_data$Method == "Inventory"], 
                                                                                 inflation = inflation, 
                                                                                 corporate_tax_rate = asset_data$tax_on_profits[asset_data$Method == "Inventory"]/asset_data$profits[asset_data$Method == "Inventory"])
      
      
    #metr
      
      asset_data$metr<-vmetr(service_price = asset_data$s,
                             economic_depreciation = asset_data$delta, 
                             rate_of_return = after_tax_return)
      
  return(asset_data)
  
}

##################Ordinary, Switch to AMT#####################

ordinary_to_amt<-function(corporate_tax_rate_one,
                          corporate_tax_rate_two,
                          corporate_tax_rate_three,
                          interest_limit_one,
                          interest_limit_two,
                          interest_limit_three,
                          r_d_credit_one,
                          r_d_credit_two,
                          r_d_credit_three,
                          depreciation_regime_one,
                          depreciation_regime_two,
                          depreciation_regime_three,
                          asset_data,
                          bonus_one,
                          bonus_two,
                          bonus_three,
                          period_one,
                          period_two,
                          period_three){
  
  #Discount Rates
  after_tax_return<-interest_rate * debt_finance_share + (return_on_equity + inflation) * (1-debt_finance_share)
  
  discount_rate_one <- debt_finance_share * (interest_rate * (1 - (interest_limit_one)*corporate_tax_rate_one)) + 
    (1-debt_finance_share) * (return_on_equity+inflation)
  
  #Discount rate under the AMT, assuming the creation of credits
  
    interest_credit<-interest_rate*(corporate_tax_rate_one - corporate_tax_rate_two)
    #Debt discount rate needs to be solved iteratively for each period in AMT
    
      debt_discount_amt<-c(interest_rate*(1-corporate_tax_rate_two)-interest_credit/(1+interest_rate*(1-corporate_tax_rate_one)))
      
      for(n in seq(period_two-period_one-1)){
        
        x <- interest_rate*(1-corporate_tax_rate_two)-interest_credit/(prod(1+debt_discount_amt))*(1+interest_rate*(1-corporate_tax_rate_one))
        append(debt_discount_amt, x, after = length(debt_discount_amt))
        
      }
    
    discount_rate_two <- debt_finance_share * debt_discount_amt[length(debt_discount_amt)] + 
      (1-debt_finance_share) * (return_on_equity+inflation)
  
  discount_rate_three <- debt_finance_share * (interest_rate * (1 - (interest_limit_three)*corporate_tax_rate_three)) + 
    (1-debt_finance_share) * (return_on_equity+inflation)
  
  #Z Values
  #Period 1
  if(depreciation_regime_one =="ADS"){
    
    asset_data$z_one<-vads(ads_life = asset_data$ADS.Life,
                           gds_life = asset_data$GDS.Life,
                           econ_depreciation = asset_data$delta, 
                           system = asset_data$System, 
                           dep_method = asset_data$Method, 
                           discount_rate = discount_rate_one, 
                           start_year = 0, 
                           end_year = period_one,
                           bonus = bonus_one)
    
  }
  
  if(depreciation_regime_one =="GDS"){
    
    asset_data$z_one<-vgds(gds_life = asset_data$GDS.Life, 
                           econ_depreciation = asset_data$delta, 
                           system = asset_data$System, 
                           dep_method = asset_data$Method, 
                           discount_rate = discount_rate_one, 
                           start_year = 0, 
                           end_year = period_one, 
                           bonus = bonus_one)
    
  }
  
  if(depreciation_regime_one =="Economic"){
    
    asset_data$z_one<-vecon(dep_method = asset_data$Method,
                            gds_life = asset_data$GDS.Life,
                            discount_rate = discount_rate_one, 
                            start_year = 0, 
                            end_year = period_one, 
                            rate = asset_data$delta,
                            bonus = bonus_one)
    
  }
  
  if(depreciation_regime_one =="Book"){
    
    asset_data$z_one<-vbook(dep_method = asset_data$Method,
                            gds_life = asset_data$GDS.Life,
                            discount_rate = discount_rate_one, 
                            start_year = 0, 
                            end_year = period_one, 
                            rate = asset_data$delta,
                            bonus = bonus_one)
    
  }
  
  #Period 2
  if(depreciation_regime_two =="ADS"){
    
    asset_data$z_two<-vads(ads_life = asset_data$ADS.Life,
                           gds_life = asset_data$GDS.Life,
                           econ_depreciation = asset_data$delta, 
                           system = asset_data$System, 
                           dep_method = asset_data$Method, 
                           discount_rate = discount_rate_two, 
                           start_year = period_one, 
                           end_year = period_two,
                           bonus = bonus_two)
    
  }
  
  if(depreciation_regime_two =="GDS"){
    
    asset_data$z_two<-vgds(gds_life = asset_data$GDS.Life, 
                           econ_depreciation = asset_data$delta, 
                           system = asset_data$System, 
                           dep_method = asset_data$Method, 
                           discount_rate = discount_rate_two, 
                           start_year = period_one, 
                           end_year = period_two, 
                           bonus = bonus_two)
    
  }
  
  if(depreciation_regime_two =="Economic"){
    
    asset_data$z_two<-vecon(dep_method = asset_data$Method,
                            gds_life = asset_data$GDS.Life,
                            discount_rate = discount_rate_two, 
                            start_year = period_one, 
                            end_year = period_two, 
                            rate = asset_data$delta,
                            bonus = bonus_two)
    
  }
  
  if(depreciation_regime_two =="Book"){
    
    asset_data$z_two<-vbook(dep_method = asset_data$Method,
                            gds_life = asset_data$GDS.Life,
                            discount_rate = discount_rate_two, 
                            start_year = period_one, 
                            end_year = period_two, 
                            rate = asset_data$delta,
                            bonus = bonus_two)
    
  }
    
  #Period 3
  if(depreciation_regime_one =="ADS"){
    
    asset_data$z_three<-vads(ads_life = asset_data$ADS.Life,
                             gds_life = asset_data$GDS.Life,
                             econ_depreciation = asset_data$delta, 
                             system = asset_data$System, 
                             dep_method = asset_data$Method, 
                             discount_rate = discount_rate_three, 
                             start_year = period_two, 
                             end_year = period_three,
                             bonus = bonus_three)
    
  }
  
  if(depreciation_regime_one =="GDS"){
    
    asset_data$z_three<-vgds(gds_life = asset_data$GDS.Life, 
                             econ_depreciation = asset_data$delta, 
                             system = asset_data$System, 
                             dep_method = asset_data$Method, 
                             discount_rate = discount_rate_three, 
                             start_year = period_two, 
                             end_year = period_three, 
                             bonus = bonus_three)
    
  }
  
  if(depreciation_regime_one =="Economic"){
    
    asset_data$z_three<-vecon(dep_method = asset_data$Method,
                              gds_life = asset_data$GDS.Life,
                              discount_rate = discount_rate_three, 
                              start_year = period_two, 
                              end_year = period_three, 
                              rate = asset_data$delta,
                              bonus = bonus_three)
    
  }
  
  if(depreciation_regime_one =="Book"){
    
    asset_data$z_three<-vbook(dep_method = asset_data$Method,
                              gds_life = asset_data$GDS.Life,
                              discount_rate = discount_rate_three, 
                              start_year = period_two, 
                              end_year = period_three, 
                              rate = asset_data$delta,
                              bonus = bonus_three)
    
  }
  
  #Total Z
  
  asset_data$z <- asset_data$z_one + asset_data$z_two + asset_data$z_three
  
  #The cost of capital formula expanded to:
  
  # c = (r+d)*(1 - uzp)/(1-(tax/profits))
  #where uzp is the sum of the present discounted value of depreciation deductions over the life of the asset
  
  #uzp
  
  asset_data$uzp <- asset_data$z_one*corporate_tax_rate_one + 
    asset_data$z_two*corporate_tax_rate_two + 
    asset_data$z_three*corporate_tax_rate_three
  
  #tax in each period
  
  asset_data$tax_one <- vtax_on_profits(discount_rate = discount_rate_one, 
                                        start_year = 0, 
                                        end_year = period_one, 
                                        corporate_tax_rate = corporate_tax_rate_one)
  asset_data$tax_two <- vtax_on_profits(discount_rate = discount_rate_two, 
                                        start_year = period_one, 
                                        end_year = period_two, 
                                        corporate_tax_rate = corporate_tax_rate_two)
  asset_data$tax_three <- vtax_on_profits(discount_rate = discount_rate_three, 
                                          start_year = period_two, 
                                          end_year = period_three, 
                                          corporate_tax_rate = corporate_tax_rate_three)
  
  asset_data$tax_on_profits <-  asset_data$tax_one + 
    asset_data$tax_two * exp(-1 * discount_rate_one * period_one) + 
    asset_data$tax_three * exp(-1 * discount_rate_one * period_one) * exp(-1 * discount_rate_two * (period_two-period_one))
  
  #profits in each period
  
  asset_data$profit_one<- vprofits(discount_rate = discount_rate_one, 
                                   start_year = 0, 
                                   end_year = period_one)
  asset_data$profit_two<- vprofits(discount_rate = discount_rate_two, 
                                   start_year = period_one, 
                                   end_year = period_two)
  asset_data$profit_three<-  vprofits(discount_rate = discount_rate_three, 
                                      start_year = period_two, 
                                      end_year = period_three)
  asset_data$profits <-  asset_data$profit_one + 
    (asset_data$profit_two * exp(-1 * discount_rate_one * period_one)) + 
    (asset_data$profit_three * exp(-1 * discount_rate_one * period_one) * exp(-1 * discount_rate_two * (period_two-period_one)))
  
  
  #Calculating AMT Credits for Timing Differences
  
    #V (deductions)
    asset_data$v1<-vgds(gds_life = asset_data$GDS.Life, 
                           econ_depreciation = asset_data$delta, 
                           system = asset_data$System, 
                           dep_method = asset_data$Method, 
                           discount_rate = discount_rate_two, 
                           start_year = period_one, 
                           end_year = period_two, 
                           bonus = bonus_one)
  
    asset_data$v2<-vbook(dep_method = asset_data$Method,
                            gds_life = asset_data$GDS.Life,
                            discount_rate = discount_rate_two, 
                            start_year = period_one, 
                            end_year = period_two, 
                            rate = asset_data$delta,
                            bonus = bonus_two)
    
    
    asset_data$V<-(asset_data$v1*corporate_tax_rate_one - asset_data$v2*corporate_tax_rate_two) * exp(-1*discount_rate_one*period_one)
  
    #W (credit for taxation of earnings)
    
    asset_data$W <- asset_data$profit_two*(corporate_tax_rate_two - corporate_tax_rate_one) * exp(-1*discount_rate_one*period_one) * exp(-1*discount_rate_two*(period_two-period_one))
    
  #Service Price of Capital
  
  #Weighted Average Discount Rate
  
  asset_data$discount<- 1/asset_data$profits
  
  asset_data$s<-(asset_data$discount - inflation + asset_data$delta)*(1 - asset_data$uzp-asset_data$V)/(1-((asset_data$tax_on_profits-asset_data$W)/asset_data$profits))
  
  asset_data$s<-vamt_service_price(uzp = asset_data$uzp, 
                                   discount_rate = asset_data$discount, 
                                   economic_depreciation = asset_data$delta, 
                                   profits = asset_data$profits, 
                                   tax = asset_data$tax_on_profits, 
                                   r_d_credit = (asset_data$major_asset_group == "Intellectual Property") * r_d_credit_one, 
                                   inflation = inflation, 
                                   v_credit = asset_data$V, 
                                   w_credit = asset_data$W)
  
  asset_data$s[asset_data$Method == "Inventory"]<-vservice_price_inventories(share_lifo = share_lifo, 
                                                                             holding_period = inventory_holding_period, 
                                                                             discount_rate = asset_data$discount[asset_data$Method == "Inventory"], 
                                                                             inflation = inflation, 
                                                                             corporate_tax_rate = asset_data$tax_on_profits[asset_data$Method == "Inventory"]/asset_data$profits[asset_data$Method == "Inventory"])
  
  #metr
  
  asset_data$metr<-vmetr(service_price = asset_data$s,
                         economic_depreciation = asset_data$delta, 
                         rate_of_return = after_tax_return)
  
  return(asset_data)
  
}

###############AMT, Switch to Ordinary######################

amt_to_ordinary<-function(corporate_tax_rate_one,
                          corporate_tax_rate_two,
                          corporate_tax_rate_three,
                          interest_limit_one,
                          interest_limit_two,
                          interest_limit_three,
                          r_d_credit_one,
                          r_d_credit_two,
                          r_d_credit_three,
                          depreciation_regime_one,
                          depreciation_regime_two,
                          depreciation_regime_three,
                          asset_data,
                          bonus_one,
                          bonus_two,
                          bonus_three,
                          period_one,
                          period_two,
                          period_three){
  
  #Discount Rates
    after_tax_return<-interest_rate * debt_finance_share + (return_on_equity + inflation) * (1-debt_finance_share)
    
  #Discount rate under the AMT, creation of tax credits (rate one is AMT, rate two is ordinary)
    
    interest_credit<-interest_rate*(corporate_tax_rate_two - corporate_tax_rate_one)
    
    debt_discount_amt<-c(interest_rate*(1-corporate_tax_rate_one)-interest_credit/(1+interest_rate*(1-corporate_tax_rate_two)))
    
    for(n in seq(period_one-1)){
      
      x <- interest_rate*(1-corporate_tax_rate_one)-interest_credit/(prod(1+debt_discount_amt))*(1+interest_rate*(1-corporate_tax_rate_two))
      append(debt_discount_amt, x, after = length(debt_discount_amt))
      
    }
  
    discount_rate_one <- debt_finance_share * debt_discount_amt[length(debt_discount_amt)] + 
      (1-debt_finance_share) * (return_on_equity+inflation)
  
    discount_rate_two <- debt_finance_share * (interest_rate * (1 - (interest_limit_two)*corporate_tax_rate_two)) + 
      (1-debt_finance_share) * (return_on_equity+inflation)
  
    discount_rate_three <- debt_finance_share * (interest_rate * (1 - (interest_limit_three)*corporate_tax_rate_three)) + 
      (1-debt_finance_share) * (return_on_equity+inflation)
  
  #Z Values
  #Period 1
    if(depreciation_regime_one =="ADS"){
      
      asset_data$z_one<-vads(ads_life = asset_data$ADS.Life,
                             gds_life = asset_data$GDS.Life,
                             econ_depreciation = asset_data$delta, 
                             system = asset_data$System, 
                             dep_method = asset_data$Method, 
                             discount_rate = discount_rate_one, 
                             start_year = 0, 
                             end_year = period_one,
                             bonus = bonus_one)
      
    }
    
    if(depreciation_regime_one =="GDS"){
      
      asset_data$z_one<-vgds(gds_life = asset_data$GDS.Life, 
                             econ_depreciation = asset_data$delta, 
                             system = asset_data$System, 
                             dep_method = asset_data$Method, 
                             discount_rate = discount_rate_one, 
                             start_year = 0, 
                             end_year = period_one, 
                             bonus = bonus_one)
      
    }
    
    if(depreciation_regime_one =="Economic"){
      
      asset_data$z_one<-vecon(dep_method = asset_data$Method,
                              gds_life = asset_data$GDS.Life,
                              discount_rate = discount_rate_one, 
                              start_year = 0, 
                              end_year = period_one, 
                              rate = asset_data$delta,
                              bonus = bonus_one)
      
    }
    
    if(depreciation_regime_one =="Book"){
      
      asset_data$z_one<-vbook(dep_method = asset_data$Method,
                              gds_life = asset_data$GDS.Life,
                              discount_rate = discount_rate_one, 
                              start_year = 0, 
                              end_year = period_one, 
                              rate = asset_data$delta,
                              bonus = bonus_one)
      
    }
  
  #Period 2
    if(depreciation_regime_two =="ADS"){
      
      asset_data$z_two<-vads(ads_life = asset_data$ADS.Life,
                             gds_life = asset_data$GDS.Life,
                             econ_depreciation = asset_data$delta, 
                             system = asset_data$System, 
                             dep_method = asset_data$Method, 
                             discount_rate = discount_rate_two, 
                             start_year = period_one, 
                             end_year = period_two,
                             bonus = bonus_two)
      
    }
    
    if(depreciation_regime_two =="GDS"){
      
      asset_data$z_two<-vgds(gds_life = asset_data$GDS.Life, 
                             econ_depreciation = asset_data$delta, 
                             system = asset_data$System, 
                             dep_method = asset_data$Method, 
                             discount_rate = discount_rate_two, 
                             start_year = period_one, 
                             end_year = period_two, 
                             bonus = bonus_two)
      
    }
    
    if(depreciation_regime_two =="Economic"){
      
      asset_data$z_two<-vecon(dep_method = asset_data$Method,
                              gds_life = asset_data$GDS.Life,
                              discount_rate = discount_rate_two, 
                              start_year = period_one, 
                              end_year = period_two, 
                              rate = asset_data$delta,
                              bonus = bonus_two)
      
    }
    
    if(depreciation_regime_two =="Book"){
      
      asset_data$z_two<-vbook(dep_method = asset_data$Method,
                              gds_life = asset_data$GDS.Life,
                              discount_rate = discount_rate_two, 
                              start_year = period_one, 
                              end_year = period_two, 
                              rate = asset_data$delta,
                              bonus = bonus_two)
      
    }
  
  #Period 3
    if(depreciation_regime_one =="ADS"){
      
      asset_data$z_three<-vads(ads_life = asset_data$ADS.Life,
                               gds_life = asset_data$GDS.Life,
                               econ_depreciation = asset_data$delta, 
                               system = asset_data$System, 
                               dep_method = asset_data$Method, 
                               discount_rate = discount_rate_three, 
                               start_year = period_two, 
                               end_year = period_three,
                               bonus = bonus_three)
      
    }
    
    if(depreciation_regime_one =="GDS"){
      
      asset_data$z_three<-vgds(gds_life = asset_data$GDS.Life, 
                               econ_depreciation = asset_data$delta, 
                               system = asset_data$System, 
                               dep_method = asset_data$Method, 
                               discount_rate = discount_rate_three, 
                               start_year = period_two, 
                               end_year = period_three, 
                               bonus = bonus_three)
      
    }
    
    if(depreciation_regime_one =="Economic"){
      
      asset_data$z_three<-vecon(dep_method = asset_data$Method,
                                gds_life = asset_data$GDS.Life,
                                discount_rate = discount_rate_three, 
                                start_year = period_two, 
                                end_year = period_three, 
                                rate = asset_data$delta,
                                bonus = bonus_three)
      
    }
    
    if(depreciation_regime_one =="Book"){
      
      asset_data$z_three<-vbook(dep_method = asset_data$Method,
                                gds_life = asset_data$GDS.Life,
                                discount_rate = discount_rate_three, 
                                start_year = period_two, 
                                end_year = period_three, 
                                rate = asset_data$delta,
                                bonus = bonus_three)
      
    }
  
  #Total Z
  
    asset_data$z <- asset_data$z_one + asset_data$z_two + asset_data$z_three
  
  #The cost of capital formula expanded to:
  
    # c = (r+d)*(1 - uzp)/(1-(tax/profits))
    #where uzp is the sum of the present discounted value of depreciation deductions over the life of the asset
    
  #uzp
  
    asset_data$uzp <- asset_data$z_one*corporate_tax_rate_one + 
    asset_data$z_two*corporate_tax_rate_two + 
    asset_data$z_three*corporate_tax_rate_three
  
  #tax in each period
  
    asset_data$tax_one <- vtax_on_profits(discount_rate = discount_rate_one, 
                                          start_year = 0, 
                                          end_year = period_one, 
                                          corporate_tax_rate = corporate_tax_rate_one)
    asset_data$tax_two <- vtax_on_profits(discount_rate = discount_rate_two, 
                                          start_year = period_one, 
                                          end_year = period_two, 
                                          corporate_tax_rate = corporate_tax_rate_two)
    asset_data$tax_three <- vtax_on_profits(discount_rate = discount_rate_three, 
                                            start_year = period_two, 
                                            end_year = period_three, 
                                            corporate_tax_rate = corporate_tax_rate_three)
  
    asset_data$tax_on_profits <-  asset_data$tax_one + 
                                  asset_data$tax_two * exp(-1 * discount_rate_one * period_one) + 
                                  asset_data$tax_three * exp(-1 * discount_rate_one * period_one) * exp(-1 * discount_rate_two * (period_two-period_one))
  
  #profits in each period
  
    asset_data$profit_one<- vprofits(discount_rate = discount_rate_one, 
                                     start_year = 0, 
                                     end_year = period_one)
    asset_data$profit_two<- vprofits(discount_rate = discount_rate_two, 
                                     start_year = period_one, 
                                     end_year = period_two)
    asset_data$profit_three<-  vprofits(discount_rate = discount_rate_three, 
                                        start_year = period_two, 
                                        end_year = period_three)
    asset_data$profits <-  asset_data$profit_one + 
                          (asset_data$profit_two * exp(-1 * discount_rate_one * period_one)) + 
                          (asset_data$profit_three * exp(-1 * discount_rate_one * period_one) * exp(-1 * discount_rate_two * (period_two-period_one)))
    
  
  #Calculating AMT Credits for Timing Differences
  
  #V (deductions)
    asset_data$v1<-vgds(gds_life = asset_data$GDS.Life, 
                        econ_depreciation = asset_data$delta, 
                        system = asset_data$System, 
                        dep_method = asset_data$Method, 
                        discount_rate = discount_rate_one, 
                        start_year = 0, 
                        end_year = period_one, 
                        bonus = bonus_two)
    
    asset_data$v2<-vbook(dep_method = asset_data$Method,
                         gds_life = asset_data$GDS.Life,
                         discount_rate = discount_rate_one, 
                         start_year = 0, 
                         end_year = period_one, 
                         rate = asset_data$delta,
                         bonus = bonus_one)
  
  
    asset_data$V<-(asset_data$v1*corporate_tax_rate_two - asset_data$v2*corporate_tax_rate_one) * exp(-1*discount_rate_one*period_one)
  
  #W (credit for taxation of earnings)
  
    asset_data$W <- asset_data$profit_one*(corporate_tax_rate_one - corporate_tax_rate_two) * exp(-1*discount_rate_one*period_one)
  
  #Service Price of Capital
  
  #Weighted Average Discount Rate
  
    asset_data$discount<- 1/asset_data$profits
    
    asset_data$s<-vamt_service_price(uzp = asset_data$uzp, 
                                     discount_rate = asset_data$discount, 
                                     economic_depreciation = asset_data$delta, 
                                     profits = asset_data$profits, 
                                     tax = asset_data$tax_on_profits, 
                                     r_d_credit = (asset_data$major_asset_group == "Intellectual Property") * r_d_credit_two * exp(-1*discount_rate_one*period_one), 
                                     inflation = inflation, 
                                     v_credit = asset_data$V, 
                                     w_credit = asset_data$W)
    
    asset_data$s[asset_data$Method == "Inventory"]<-vservice_price_inventories(share_lifo = share_lifo, 
                                                                               holding_period = inventory_holding_period, 
                                                                               discount_rate = asset_data$discount[asset_data$Method == "Inventory"], 
                                                                               inflation = inflation, 
                                                                               corporate_tax_rate = asset_data$tax_on_profits[asset_data$Method == "Inventory"]/asset_data$profits[asset_data$Method == "Inventory"])
    
  #metr
  
    asset_data$metr<-vmetr(service_price = asset_data$s,
                           economic_depreciation = asset_data$delta, 
                           rate_of_return = after_tax_return)
    
    return(asset_data)
  
}


#Output Function

output<-function(asset_data){
  
  equipment<-weighted.mean(asset_data$metr[asset_data$major_asset_group == "Equipment"], asset_data$assets[asset_data$major_asset_group == "Equipment"])
  structures<-weighted.mean(asset_data$metr[asset_data$major_asset_group == "Structures"], asset_data$assets[asset_data$major_asset_group == "Structures"])
  intellectual_property<-weighted.mean(asset_data$metr[asset_data$major_asset_group == "Intellectual Property"], asset_data$assets[asset_data$major_asset_group == "Intellectual Property"])
  land<-weighted.mean(asset_data$metr[asset_data$major_asset_group == "Land"], asset_data$assets[asset_data$major_asset_group == "Land"])
  inventory<-weighted.mean(asset_data$metr[asset_data$major_asset_group == "Inventories"], asset_data$assets[asset_data$major_asset_group == "Inventories"])
  overall<-weighted.mean(asset_data$metr, asset_data$assets)
  standard_deviation <- sqrt(sum((asset_data$metr - overall)^2 * asset_data$assets/sum(asset_data$assets)))
  
  results<-c(equipment, structures, intellectual_property,land,inventory,overall,standard_deviation)
  names(results)<-c("Equipment","Structures","Intellectual Property","Land","Inventory","Overall","Standard Deviation")
  
  return(results)
  
}


baseline<-permanent_tax(corporate_tax_rate = 0.28,
                        interest_limit = (1-0.16),
                        r_d_credit = 0.0217,
                        depreciation_regime = "GDS",
                        asset_data = assets,
                        bonus = 1)

book_tax<-permanent_tax(corporate_tax_rate = 0.15,
                        interest_limit = (1),
                        r_d_credit = 0,
                        depreciation_regime = "Book",
                        asset_data = assets,
                        bonus = (0.0))

temporary_test<-temporary_tax(corporate_tax_rate_one = .28,
                              corporate_tax_rate_two = .28,
                              corporate_tax_rate_three = .28,
                              interest_limit_one = (1-.16),
                              interest_limit_two = (1-.16),
                              interest_limit_three = (1-.16),
                              r_d_credit_one = 0.0217,
                              r_d_credit_two = 0.0217,
                              r_d_credit_three = 0.0217,
                              depreciation_regime_one = "GDS",
                              depreciation_regime_two = "GDS",
                              depreciation_regime_three = "GDS",
                              asset_data = assets,
                              bonus_one = 1.0,
                              bonus_two = 1.0,
                              bonus_three = 1.0,
                              period_one = 2,
                              period_two = 4,
                              period_three = Inf)

ordinary_to_amt_test<-ordinary_to_amt(corporate_tax_rate_one = .28,
                                    corporate_tax_rate_two = .15,
                                    corporate_tax_rate_three = .28,
                                    interest_limit_one = (1-.16),
                                    interest_limit_two = (1),
                                    interest_limit_three = (1-.16),
                                    r_d_credit_one = 0.0217,
                                    r_d_credit_two = 0.0217,
                                    r_d_credit_three = 0.0217,
                                    depreciation_regime_one = "GDS",
                                    depreciation_regime_two = "Book",
                                    depreciation_regime_three = "GDS",
                                    asset_data = assets,
                                    bonus_one = 1.0,
                                    bonus_two = 0,
                                    bonus_three = 1.0,
                                    period_one = 3,
                                    period_two = 8,
                                    period_three = Inf)

amt_to_ordinary_test<-amt_to_ordinary(corporate_tax_rate_one = .15,
                                      corporate_tax_rate_two = .28,
                                      corporate_tax_rate_three = .28,
                                      interest_limit_one = (1),
                                      interest_limit_two = (1-.16),
                                      interest_limit_three = (1-.16),
                                      r_d_credit_one = 0,
                                      r_d_credit_two = 0.0217,
                                      r_d_credit_three = 0.0217,
                                      depreciation_regime_one = "Book",
                                      depreciation_regime_two = "GDS",
                                      depreciation_regime_three = "GDS",
                                      asset_data = assets,
                                      bonus_one = 0,
                                      bonus_two = 1.0,
                                      bonus_three = 1.0,
                                      period_one = 5,
                                      period_two = 10,
                                      period_three = Inf)


output(baseline)
output(book_tax)
#output(temporary_test)
output(ordinary_to_amt_test)
output(amt_to_ordinary_test)

#AMT Tax Credits DONE
#Debt financing discount rate for AMT DONE
#RND Tax Credits for AMT DONE
#Fixing the discounting of Z
#Fix inventory service price DONE
#Fix discount rates DONE
#Check the math of the V credit and discounting. Doesn't feel right
