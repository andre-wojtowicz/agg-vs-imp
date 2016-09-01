source("aggregation-operators-generator.R")

get.grouped.aggregation.operators = function()
{
    ret = list()

    ret[["Mean Numeric"]] =
        apply(cbind(expand.grid(AGGR.WEIGHTS, AGGR.SELECTORS),
                    expand.grid(AGGR.WEIGHTS.NAME, AGGR.SELECTORS.NAME)),
              1, function(row){
              apply(cbind(expand.grid(AGGR.CUTOFFS.NUMERIC,AGGR.RMEANS),
                          expand.grid(AGGR.CUTOFFS.NUMERIC.NAME,AGGR.RMEANS.NAME)),
                    1, function(row2){
                    list(AGG.GEN.MEAN.WEIGHTED(row[[1]],row[[2]],row2[[1]],r = row2[[2]]),
                         paste('mean',row[[3]],row[[4]],row2[[3]],row2[[4]],sep = '_'),
                         'Mean', 'Numeric')
              })
        })

    ret[["Mean Interval"]] =
        apply(cbind(AGGR.WEIGHTS, AGGR.WEIGHTS.NAME),
              1, function(row){
              apply(cbind(expand.grid(AGGR.CUTOFFS, AGGR.RMEANS),
                          expand.grid(AGGR.CUTOFFS.NAME, AGGR.RMEANS.NAME)),
                    1, function(row2){
                    list(AGG.GEN.INTERVAL.MEAN(row[[1]],row2[[1]],r = row2[[2]]),
                         paste('iMean',row[[2]],row2[[3]],row2[[4]],sep = '_'),
                         'Mean', 'Interval')
              })
        })

    ret[["OWA Numeric"]] =
        apply(cbind(expand.grid(AGGR.SELECTORS, AGGR.OWA.ORDERS),
                    expand.grid(AGGR.SELECTORS.NAME, AGGR.OWA.ORDERS.NAME)),
              1, function(row){
                  apply(cbind(expand.grid(AGGR.OWA.WEIGHTS, AGGR.CUTOFFS.NUMERIC),
                              expand.grid(AGGR.OWA.WEIGHTS.NAME, AGGR.CUTOFFS.NUMERIC.NAME)),
                        1, function(row2){
                        wg = GEN.WEIGHT.OWA(row2[[1]],row[[2]])
                        wgn = paste('(',row2[[3]],'_',row[[4]],')',sep = '')
                        list(AGG.GEN.MEAN.WEIGHTED(wg,row[[1]],row2[[2]]),
                             paste('mean',wgn,row[[3]],row2[[4]],sep = '_'),
                             'OWA', 'Numeric')
                  })
        })

    ret[["OWA Interval"]] =
        apply(cbind(AGGR.OWA.ORDERS, AGGR.OWA.ORDERS.NAME),
              1, function(row){
              apply(cbind(expand.grid(AGGR.OWA.WEIGHTS, AGGR.CUTOFFS),
                          expand.grid(AGGR.OWA.WEIGHTS.NAME, AGGR.CUTOFFS.NAME)),
                     1, function(row2){
                            wg = GEN.WEIGHT.OWA(row2[[1]],row[[1]])
                            wgn = paste('(',row2[[3]],'_',row[[2]],')',sep = '')
                            list(AGG.GEN.INTERVAL.MEAN(wg,row2[[2]], wg),
                                 paste('iMean',wgn,row2[[4]],sep = '_'),
                                 'OWA', 'Interval')
              })
        })

    ret[["OWA Interval"]] = append(ret[["OWA Interval"]], list( # OWA.INTERSECTION
       apply(cbind(AGGR.CUTOFFS,
                   AGGR.CUTOFFS.NAME),
             1, function(row){
                 list(AGG.GEN.INTERVAL.INTERSECTION(row[[1]]),
                      paste('inter',row[[2]],sep = '_'),
                      'OWA', 'Interval')
        })
    ))


    ret[["OWA Interval"]] = append(ret[["OWA Interval"]], list( # OWA.SUM
        apply(cbind(AGGR.CUTOFFS,
                    AGGR.CUTOFFS.NAME),
              1, function(row){
                  list(AGG.GEN.INTERVAL.SUM(row[[1]]),
                       paste('sum',row[[2]],sep = '_'),
                       'OWA', 'Interval')
        })
    ))

    ret[["Integral Numeric"]] =
        apply(cbind(expand.grid(AGGR.INTEGRALS, AGGR.MEASURES, AGGR.SELECTORS),
                    expand.grid(AGGR.INTEGRALS.NAME, AGGR.MEASURES.NAME, AGGR.SELECTORS.NAME)),
              1, function(row){
                    apply(cbind(AGGR.CUTOFFS.NUMERIC, AGGR.CUTOFFS.NUMERIC.NAME),
                          1, function(row2){
                              list(AGG.GEN.INTEGRAL(row[[2]],row[[3]],row2[[1]],row[[1]]),
                                   paste(row[[4]],row[[5]],row[[6]],row2[[2]],sep = '_'),
                                   'Integral', 'Numeric')
                  })
        })

    ret[["Integral Interval"]] =
        apply(cbind(expand.grid(AGGR.INTEGRALS, AGGR.MEASURES),
                    expand.grid(AGGR.INTEGRALS.NAME, AGGR.MEASURES.NAME)),
              1, function(row){
                apply(cbind(AGGR.CUTOFFS,AGGR.CUTOFFS.NAME),
                      1, function(row2){
                          list(AGG.GEN.INTERVAL.INTEGRAL(row[[2]],row2[[1]],row[[1]]),
                               paste('i',row[[3]],row[[4]],row2[[2]],sep = '_'),
                               'Integral', 'Interval')
                })
        })


    ret[["t-operation Numeric"]] =
        apply(cbind(expand.grid(AGGR.NORMS, AGGR.SELECTORS),
                    expand.grid(AGGR.NORMS.NAME, AGGR.SELECTORS.NAME)),
              1, function(row){
                  apply(cbind(AGGR.CUTOFFS.NUMERIC, AGGR.CUTOFFS.NUMERIC.NAME),
                       1, function(row2){
                           list(AGG.GEN.T.OPERATION(row[[1]],row[[2]],row2[[1]]),
                                paste(row[[3]],row[[4]],row2[[2]],sep = '_'),
                                't-operation', 'Numeric')
                  })
        })

    ret[["t-operation Numeric"]] = append(ret[["t-operation Numeric"]],
        apply(cbind(expand.grid(AGGR.NORMS, AGGR.SELECTORS),
                    expand.grid(AGGR.NORMS.NAME, AGGR.SELECTORS.NAME)),
              1, function(row){
                  apply(cbind(expand.grid(AGGR.CUTOFFS.NUMERIC,AGGR.SOFT.LAMBDAS),
                              expand.grid(AGGR.CUTOFFS.NUMERIC.NAME, AGGR.SOFT.LAMBDAS.NAME)),
                       1, function(row2){
                           soft = GEN.SOFT(row[[1]],row2[[2]])
                           softn = paste('(soft_',row[[3]],'_',row2[[4]],')',sep = '')

                           list(AGG.GEN.T.OPERATION(soft,row[[2]],row2[[1]]),
                                paste(softn,row[[4]],row2[[3]],sep = '_'),
                                't-operation', 'Numeric')
                     })
             })
    )

    ret[["t-operation Interval"]] =
        apply(cbind(AGGR.NORMS, AGGR.NORMS.NAME),
              1, function(row){
                  apply(cbind(AGGR.CUTOFFS, AGGR.CUTOFFS.NAME),
                          1, function(row2){
                              list(AGG.GEN.INTERVAL.T.OPERATION(row[[1]],row2[[1]]),
                                   paste('i',row[[2]],row2[[2]],sep = '_'),
                                   't-operation', 'Interval')
                  })
        })

    ret[["t-operation Interval"]] = append(ret[["t-operation Interval"]],
        apply(cbind(AGGR.NORMS, AGGR.NORMS.NAME),
              1, function(row){
                  apply(cbind(expand.grid(AGGR.CUTOFFS,AGGR.SOFT.LAMBDAS),
                              expand.grid(AGGR.CUTOFFS.NAME, AGGR.SOFT.LAMBDAS.NAME)),
                        1, function(row2){
                            soft = GEN.SOFT(row[[1]],row2[[2]])
                            softn = paste('(soft_',row[[2]],'_',row2[[4]],')',sep = '')

                            list(AGG.GEN.INTERVAL.T.OPERATION(soft,row2[[1]]),
                                 paste('i',softn,row2[[3]],sep = '_'),
                                 't-operation', 'Interval')
                  })
        })
    )

    ret
}
