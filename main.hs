



-- Monthly save rate -> dividend per year in percent -> dividend growth rate per year in percent -> amount of months youre saving money

divi::Float->Float->Float->Int->Float
divi _ _ _ 0 = 0.00
divi savings_rate divi_percent divi_growth months = fromIntegral (round (ergebnis * 100)) / 100
                                                    where ergebnis = (savings_rate + prev_savings_ratesXgrowth + previous_dividends) * (divi_percent/100/12)
                                                          prev_savings_ratesXgrowth = sum $ takeListToPwr (replicate (months-1) savings_rate) (1+divi_growth/12/100) (months-1)
                                                          previous_dividends =  divi savings_rate divi_percent divi_growth (months-1)





-- Ressources (used by divi)
takeListToPwr :: [Float] -> Float -> Int -> [Float]
takeListToPwr [] _ _ = []
takeListToPwr (x:xs) factor counter = x*factor^counter : takeListToPwr xs factor (counter+1)
