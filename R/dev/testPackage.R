library(pgfKeys)
value <- c('key 1=value 1',
       'key 2 = {value 2, masked}',
       'path/key 3/.handler',
       'long/path/key=value',
       'only a path/={  white space}')

x <- pgfKeyValue()

pgfKey(x, expand = TRUE) <- value

pgfValue(x) <- c('new value 1', '{ masked value   }', NA,
                  '{family/.cd, sub key 2 = sub value}', '   white space')

y <- pgfKeyValue()

value2 <- list('key 1' = 'value 1', 'key 2/.with handler' = NA) 

pgfKey(y) <- value2
