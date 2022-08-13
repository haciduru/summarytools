
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

tab = function(arr) {
  
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

crosstab = function(var1, var2) {
  
  nam1 = deparse(substitute(var1))
  nam2 = deparse(substitute(var2))
  
  tb = table(var2, var1, useNA = 'ifany')
  
  rnams = row.names(tb)
  rnams[is.na(rnams)] = '<NA>'
  rnams = paste('  ', rnams, sep = '')
  
  cnams = names(tb[1,])
  cnams[is.na(cnams)] = '<NA>'
  
  vals = matrix(as.numeric(tb), ncol = ncol(tb))
  totvals = apply(vals, 2, FUN='sum')
  vals = matrix(unlist(lapply(vals, function(x) addc(x))), ncol = ncol(tb))
  totvals = unlist(lapply(totvals, function(x) addc(x)))
  
  ow = floor((options()$width) / (ncol(tb)+4))
  vw = max(c(nchar(rnams),
             nchar(nam1),
             nchar(as.character(as.numeric(tb))),
             nchar(names(tb[1,]))), na.rm = T) + 3
  if (vw > ow) vw = ow
  
  cnams = padl(trimws(padr(cnams, vw)), vw)
  ttl = paste(padr(nam2, vw),
              paste(cnams, collapse = ' '),
              collapse = '')
  
  cat('\n', padr('', vw), padr(nam1, vw))
  cat('\n', ttl,
      '\n', cs('=', nchar(ttl)))
  
  i = 0
  while (i < nrow(tb)) {
    i = i + 1
    cat('\n', padr(rnams[i], vw), padl(vals[i,], vw))
  }
  
  cat('\n', cs('-', nchar(ttl)))
  cat('\n',
      padr('TOTAL', vw),
      padl(totvals, vw), '\n\n')
  
}
