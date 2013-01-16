## Generate rolling functions

rolling_mean <- rollit( final_trans="x/N", name="roll_mean" )
rolling_prod <- rollit( combine="*", name="roll_prod" )
rolling_max <- rollit_raw("double out = __DBL_MIN__;
                          for( int i=0; i < n; i++ ) {
                            if( x[i+ind] > out && weights[i] != 0 ) {
                              out = x[i+ind];
                            }
                          }
                          return out;
                          ", name="roll_max")
rolling_min <- rollit_raw("double out = __DBL_MAX__;
                          for( int i=0; i < n; i++ ) {
                            if( x[i+ind] < out && weights[i] != 0 ) {
                              out = x[i+ind];
                            }
                          }
                          return out;
                          ", name="roll_min")
rolling_median <- rollit_raw()
rolling_var <- rollit("((x-m)*(x-m))", const_vars = list(m="mean(x)"), final_trans="x/(N-1)", name="roll_var")
rolling_sd <- rollit("((x-m)*(x-m))", const_vars = list(m="mean(x)"), final_trans="sqrt( x/(N-1) )", name="roll_sd")
rolling_sum <- rollit(name="roll_sum")
rolling_kurtosis <- rollit_raw("double numerator = 0;
                               double denominator = 0;
                               const double m = mean( x[seq( ind, ind+n-1 )] );
                               for( int i=0; i < n; i++ ) {
                                 if( weights[i] != 0 ) {
                                   numerator += weights[i] * (x[i+ind]-m) * (x[i+ind]-m) * (x[i+ind]-m) * (x[i+ind]-m);
                                   denominator += weights[i] * (x[i+ind]-m) * (x[i+ind]-m);
                                 }
                               }
                               return N * numerator / (denominator * denominator) - 3;
                               ")