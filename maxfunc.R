maxfunc <- function(A) {
    ##assume that A is a n x 3 matrix
    ##output a matrix G such that common ids are maxed
    A=t(A)
    shortenA=0*(A[1:2,])
    shortenA[1,]=(10*A[1,]+A[2,])
    shortenA[2,]=A[3,]
    a=sort(shortenA[1,],index.return=T)
    shortenA=shortenA[,a$ix]
    A=A[,a$ix]
    
    G=shortenA
    curind=G[1,1]
    curstart=1
    curend=1
    for (j in 2:ncol(G)) {
        if (G[1,j]==curind)
            curend=j
        else {
            G[2,curstart:curend]=max(G[2,curstart:curend], na.rm=TRUE)
            curstart=j
            curend=j
            curind=G[1,j]
        }
    }
    G[2,curstart:curend]=max(G[2,curstart:curend], na.rm=TRUE)
    bigG=A
    bigG[3,]=G[2,]
    t(bigG)
}

maxfunc <- function(A) {
  ##assume that A is a n x 3 matrix
  ##output a matrix G such that common ids are maxed
  A=t(A)
  shortenA=0*(A[1:2,])
  shortenA[1,]=(10*A[1,]+A[2,])
  shortenA[2,]=A[3,]
  a=sort(shortenA[1,],index.return=T)
  shortenA=shortenA[,a$ix]
  A=A[,a$ix]
  
  G=shortenA
  curind=G[1,1]
  curstart=1
  curend=1
  for (j in 2:ncol(G)) {
    if (G[1,j]==curind)
      curend=j
    else {
      G[2,curstart:curend]=max(G[2,curstart:curend], na.rm=TRUE)
      curstart=j
      curend=j
      curind=G[1,j]
    }
  }
  G[2,curstart:curend]=max(G[2,curstart:curend], na.rm=TRUE)
  bigG=A
  bigG[3,]=G[2,]
  t(bigG)
}

maxfunc_alt <- function(A) {
  ##A is n x 2
  ##output a matrix that satisfies conditions
  a=sort(A[,1],index.return=TRUE)
  G=t(A[a$ix,])
  curind=G[1,1]
  curstart=1
  curend=1
  for (j in 2:ncol(G)) {
    if (G[1,j]==curind)
      curend=j
    else {
      G[2,curstart:curend]=max(G[2,curstart:curend], na.rm=TRUE)
      curstart=j
      curend=j
      curind=G[1,j]
    }
  }
  G[2,curstart:curend]=max(G[2,curstart:curend], na.rm=TRUE)
  t(G)
}

maxfunc_sum <- function(A) {
  ##A is n x 2
  ##output a matrix that satisfies conditions
  a=sort(A[,1],index.return=TRUE)
  G=t(A[a$ix,])
  curind=G[1,1]
  curstart=1
  curend=1
  for (j in 2:ncol(G)) {
    if (G[1,j]==curind)
      curend=j
    else {
      G[2,curstart:curend]=sum(G[2,curstart:curend], na.rm=TRUE)
      curstart=j
      curend=j
      curind=G[1,j]
    }
  }
  G[2,curstart:curend]=sum(G[2,curstart:curend], na.rm=TRUE)
  t(G)
}

newmaxfunc <- function(A) {
  ##A is n x 6
  ##output a matrix that satisfies conditions
  a=sort(A[,1],index.return=TRUE)
  G=t(A[a$ix,])
  curind=G[1,1]
  curstart=1
  curend=1
  for (j in 2:ncol(G)) {
    if (G[1,j]==curind)
      curend=j
    else {
      G[2,curstart:curend]=max(G[2,curstart:curend], na.rm=TRUE)
      G[3,curstart:curend]=max(G[3,curstart:curend], na.rm=TRUE)
      G[5,curstart:curend]=max(G[5,curstart:curend], na.rm=TRUE)
      G[6,curstart:curend]=min(G[6,curstart:curend], na.rm=TRUE)
      G[4,curstart:curend]=sum(G[4,curstart:curend], na.rm=TRUE)
      curstart=j
      curend=j
      curind=G[1,j]
    }
  }
  G[2,curstart:curend]=max(G[2,curstart:curend], na.rm=TRUE)
  G[3,curstart:curend]=max(G[3,curstart:curend], na.rm=TRUE)
  G[5,curstart:curend]=max(G[5,curstart:curend], na.rm=TRUE)
  G[6,curstart:curend]=min(G[6,curstart:curend], na.rm=TRUE)
  G[4,curstart:curend]=sum(G[4,curstart:curend], na.rm=TRUE)
  t(G)
}
