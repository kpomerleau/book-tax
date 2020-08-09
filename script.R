
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

  straight_line<- function(discount_rate, asset_life, start_year, years_on, bonus){
    
    z_c <- bonus * max(min(1-start_year,1),0)
    
    z_a <- (1 - bonus)*((1-exp(-1*discount_rate*years_on))/(discount_rate*asset_life))*exp(-1*discount_rate*start_year)
    z<-z_c + z_a
    return(z)
    
  }
  
  declining_balance_sl<- function(discount_rate, asset_life, start_year, years_on, rate, bonus){
    
    beta <- rate/asset_life
    time_on_db <- asset_life*(1-(1/rate))
    
    z_c <- bonus * max(min(1-start_year,1),0)
    
    z_a<-(1-(beta/(beta))*(1-exp(-1*beta*start_year)))
    z_b<-(beta/(beta+discount_rate))*(1-exp(-1*(beta+discount_rate)*min(years_on,max(time_on_db-start_year,0))))*exp(-1*discount_rate*start_year)
    z_db<-z_a*z_b
    ###FIX THIS###
    z_sl<-((exp(-1*beta*time_on_db)/((asset_life-time_on_db)*discount_rate))*(exp(-1*discount_rate*max(time_on_db,start_year))-exp(-1*discount_rate*min(asset_life,years_on))))
    ###FIX THIS###
    
    z<-z_c + (1-bonus)*z_db+(1-bonus)*z_sl
    
    return(z_sl)
    
  }
  
  economic_depreciation<- function(discount_rate, start_year, years_on, rate, bonus){
    
    z_c<-bonus * max(min(1-start_year,1),0)
    
    z_a<-(1-(rate/(rate))*(1-exp(-1*rate*start_year)))
    z_b<-(rate/(rate+discount_rate-inflation))*(1-exp(-1*(rate+discount_rate-inflation)*min(years_on,max(years_on-start_year,0))))*exp(-1*discount_rate*start_year)
    
    z<-z_c + (1-bonus)*z_a*z_b
    
  }

#Need to add AMT credits

#Service Price Formula
  
  service_price<-function(z_present_value, discount_rate, economic_depreciation, corporate_tax_rate, r_d_credit, inflation){
    
    service_price<-(discount_rate - inflation + economic_depreciation)*(1 - corporate_tax_rate * z_present_value - r_d_credit)/ (1 - corporate_tax_rate)
    
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
                              years_on,
                              years_on_econ,
                              bonus){
          
          z<-0
      
          if(dep_method == "Expensing"){z <- 1*max(min(1-start_year,1),0)}
          if(dep_method == "Economic"){
            
            z<-economic_depreciation(discount_rate, start_year, years_on_econ, econ_depreciation, bonus * (gds_life<=20))
            
          }
        
          if(dep_method =="DB 200%"){
            
            z<-declining_balance_sl(discount_rate, gds_life, start_year, years_on, 2, bonus * (gds_life<=20))
            
          }
          
          if(dep_method =="DB 150%"){
            
            z<-declining_balance_sl(discount_rate, gds_life, start_year, years_on, 1.5, bonus * (gds_life<=20))
            
          }
          
          if(dep_method =="SL"){
            
            z<-straight_line(discount_rate, gds_life, start_year, years_on, bonus  * (gds_life<=20))
            
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
                              years_on,
                              bonus){
    
    
    z<-straight_line(discount_rate, ads_life, start_year, years_on, bonus * (gds_life<=20))
    if(dep_method == "Expensing"){z <- 1*max(min(1-start_year,1),0)}
    if(dep_method == "Land"){z<-0}
    return(z)
    
  }
  
  cost_recovery_econ<-function(dep_method,
                               gds_life,
                               discount_rate, 
                               start_year, 
                               years_on, 
                               rate,
                               bonus){
    
    z<-economic_depreciation(discount_rate, start_year, years_on, rate, bonus * (gds_life<=20))
    if(dep_method == "Inventory"){z<-0}
    if(dep_method == "Land"){z<-0}
    
    return(z)
    
  }
  
  cost_recovery_book<-function(dep_method,
                               gds_life,
                               discount_rate,
                               start_year,
                               years_on,
                               rate,
                               bonus){
    
    z<-economic_depreciation(discount_rate, start_year, years_on, rate, bonus * (gds_life<=20))
    if(dep_method == "Expensing"){z <- 1*max(min(1-start_year,1),0)}
    if(dep_method == "Land"){z<-1}
    return(z)
    
  }

#Vectorize my functions
vads<-Vectorize(cost_recovery_ADS)
vecon<-Vectorize(cost_recovery_econ)
vgds<-Vectorize(cost_recovery_GDS)
vbook<-Vectorize(cost_recovery_book)
vservice_price<-Vectorize(service_price)
vservice_price_inventories<-Vectorize(inventories)
vmetr<-Vectorize(metr)

#Permanent Tax System

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
                         years_on = asset_data$ADS.Life,
                         bonus = bonus)
      
    }
    
    if(depreciation_regime =="GDS"){
      
      asset_data$z<-vgds(gds_life = asset_data$GDS.Life, 
                         econ_depreciation = asset_data$delta, 
                         system = asset_data$System, 
                         dep_method = asset_data$Method, 
                         discount_rate = discount_rate, 
                         start_year = 0, 
                         years_on = asset_data$GDS.Life, 
                         years_on_econ = Inf,
                         bonus = bonus)
      
    }
    
    if(depreciation_regime =="Economic"){
      
      asset_data$z<-vecon(dep_method = asset_data$Method,
                          gds_life = asset_data$GDS.Life,
                          discount_rate = discount_rate, 
                          start_year = 0, 
                          years_on = Inf, 
                          rate = asset_data$delta,
                          bonus = bonus)

    }
  
    if(depreciation_regime =="Book"){
      
      asset_data$z<-vbook(dep_method = asset_data$Method,
                          gds_life = asset_data$GDS.Life,
                          discount_rate = discount_rate, 
                          start_year = 0, 
                          years_on = Inf, 
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

#Temporary Tax System

temporary_tax<-function(corporate_tax_rate_one,
                        corporate_tax_rate_two,
                        interest_limit_one,
                        interest_limit_two,
                        r_d_credit_one,
                        r_d_credit_two,
                        depreciation_regime_one,
                        depreciation_regime_two,
                        asset_data,
                        bonus_one,
                        bonus_two,
                        period_one,
                        period_two,
                        period_three){
  
  #Discount Rates
  after_tax_return<-interest_rate * debt_finance_share + (return_on_equity + inflation) * (1-debt_finance_share)
  discount_rate_one <- debt_finance_share * (interest_rate * (1 - (interest_limit_one)*corporate_tax_rate_one)) + 
    (1-debt_finance_share) * (return_on_equity+inflation)
  discount_rate_two <- debt_finance_share * (interest_rate * (1 - (interest_limit_two)*corporate_tax_rate_two)) + 
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
                       years_on = pmin(asset_data$ADS.Life, period_one),
                       bonus = bonus_one)
    
  }
  
  if(depreciation_regime_one =="GDS"){
    
    asset_data$z_one<-vgds(gds_life = asset_data$GDS.Life, 
                       econ_depreciation = asset_data$delta, 
                       system = asset_data$System, 
                       dep_method = asset_data$Method, 
                       discount_rate = discount_rate_one, 
                       start_year = 0, 
                       years_on = pmin(asset_data$GDS.Life, period_one), 
                       years_on_econ = period_one,
                       bonus = bonus_one)
    
  }
  
  if(depreciation_regime_one =="Economic"){
    
    asset_data$z_one<-vecon(dep_method = asset_data$Method,
                        gds_life = asset_data$GDS.Life,
                        discount_rate = discount_rate_one, 
                        start_year = 0, 
                        years_on = period_one, 
                        rate = asset_data$delta,
                        bonus = bonus_one)
    
  }
  
  if(depreciation_regime_one =="Book"){
    
    asset_data$z_one<-vbook(dep_method = asset_data$Method,
                        gds_life = asset_data$GDS.Life,
                        discount_rate = discount_rate, 
                        start_year = 0, 
                        years_on = period_one, 
                        rate = asset_data$delta,
                        bonus = bonus_one)
    
  }
  
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

test<-temporary_tax(corporate_tax_rate_one = .28,
                    corporate_tax_rate_two = .15,
                    interest_limit_one = (1-.16),
                    interest_limit_two = (0),
                    r_d_credit_one = 0.0217,
                    r_d_credit_two = 0,
                    depreciation_regime_one = "GDS",
                    depreciation_regime_two = "Book",
                    asset_data = assets,
                    bonus_one = 0,
                    bonus_two = 0,
                    period_one = 2,
                    period_two = 2,
                    period_three = Inf)

output(baseline)
output(book_tax)

