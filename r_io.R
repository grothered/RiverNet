## R routines to read output

get_flow<-function(flowfile='output.txt', timefile='time.txt', numvars=5){
    dd=read.table(flowfile)
    dd=as.matrix(dd)
    v=seq(1,dim(dd)[1],by=numvars)

    time=read.table(timefile)
    time=as.matrix(time)
    return(list(t=time, w=dd[v,], A=dd[v+1,], d=dd[v+2,], Q=dd[v+3,], Qc=dd[v+4,]))
}
