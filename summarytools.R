
tab = function(arr) {
  
  cs = function(c, n = 30) {
    s = c; for (i in 1:n) s = paste(s, c, sep = '')
    return(s)
  }
  
  ws = function(n = 30) {
    s = ''; for (i in 1:n) s = paste(s, ' ', sep = '')
    return(s)
  }
  
  padl = function(s, n) {
    ss = paste(ws(n), s, sep = '')
    ss = substr(ss, nchar(ss) - n, nchar(ss))
    return(ss)
  }
  
  padr = function(s, n) {
    ss = paste(s, ws(n), sep = '')
    ss = substr(ss, 1, n)
    return(ss)
  }
  
  addc = function(val) {
    if (typeof(val) != 'character') val = as.character(val)
    val = unlist(strsplit(val, ''))
    s = ''
    j = 0
    while (length(val) > 0) {
      s = paste(val[length(val)], s, sep = '')
      val = val[-length(val)]
      j = j + 1
      if (j == 3 & length(val) > 0) {
        s = paste(',', s, sep = '')
        j = 0
      }
    }
    return(s)
  }

  tb = table(arr, useNA = 'ifany')
  
  nams = names(tb)
  nams = paste('  ', nams, sep = '')
  
  vals = as.numeric(tb)
  valstot = sum(vals)
  
  pers = round(vals / sum(vals) * 100, 1)
  perstot = sum(pers)
  cpers = cumsum(pers)
  
  vals = unlist(lapply(vals, function(x) addc(x)))
  valstot = addc(valstot) 
  
  ow = floor((options()$width) / 2)
  if (max(nchar(nams)) < ow) ow = max(nchar(nams))
  
  maxd = max(nchar(as.character(vals))) + 2
  
  nams = padr(nams, ow)
  vals = padl(vals, maxd)
  pers = padl(pers, maxd)
  cpers = padl(cpers, maxd)
  
  cat('\n',
      padr('Category', ow),
      padl('Freq.', maxd),
      padl('%', maxd),
      padl('Cum.%', maxd),
      '\n', cs('=', ow+maxd*3+5)
  )
  i = 0
  while (i < length(nams)) {
    i = i + 1
    cat('\n', nams[i], vals[i], pers[i], cpers[i])
  }
  cat('\n', cs('-', ow+maxd*3+5))
  
  cat('\n',
      padr('TOTAL', ow),
      padl(addc(valstot), maxd), padl(perstot, maxd), '\n\n')
  
}
