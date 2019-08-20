context("altrep class settings")

length_func <- function(x) {
    length(x)
}

get_ptr_func <- function(x, writeable) {
    return(x)
}
ptr_or_null_func <- function(x) {
    return(x)
}

environment(length_func)=globalenv()
environment(get_ptr_func)=globalenv()
environment(ptr_or_null_func)=globalenv()

deleteClass(className = "test",warning=FALSE)

setAltClass("test", "real")
setAltMethod("test", getLength = length_func)
setAltMethod("test", getDataptr = get_ptr_func)
setAltMethod("test", getDataptrOrNull = ptr_or_null_func)

a=runif(10)
b = makeAltrep("test", a)


# autoExportClassDef = TRUE,
# autoDuplicate = TRUE,
# autoSerialize = TRUE

test_that("Auto duplication",{
    setAltClassSetting(className = "test",autoDuplicate=TRUE)
    d=b
    d[1]=10
    expect_true(is.altWrapper(d))
    expect_true(d[1]!=b[1])
    expect_equal(d[-1],b[-1])
})



test_that("Auto serialize", {
    # browser()
    #auto serialize on
    setAltClassSetting(className = "test",autoExportClassDef=TRUE,autoSerialize=TRUE)
    b_serilized=serialize(b,NULL)
    b1=unserialize(b_serilized)
    expect_equal(b1,b)
    expect_true(is.altWrapper(b1))

    #auto serialize off
    setAltClassSetting(className = "test",autoSerialize=FALSE)
    b_serilized=serialize(b,NULL)
    b1=unserialize(b_serilized)
    expect_false(is.altWrapper(b1))
})


test_that("Auto export class def", {
    ## auto export on
    setAltClassSetting(className = "test",autoExportClassDef=TRUE,autoSerialize=TRUE)
    b_serilized_auto=serialize(b,NULL)
    b1=unserialize(b_serilized_auto)
    expect_equal(b1,b)
    expect_true(is.altWrapper(b1))

    ## auto serialize off
    setAltClassSetting(className = "test",autoExportClassDef=FALSE)
    b_serilized_noAuto=serialize(b,NULL)
    b1=unserialize(b_serilized_noAuto)
    expect_equal(b1,b)
    expect_true(is.altWrapper(b1))

    ## Check the size of the serialized data
    expect_true(length(b_serilized_auto)>length(b_serilized_noAuto))
})


test_that("cluster export", {
    ## auto export and serialize on
    setAltClassSetting(className = "test",autoExportClassDef=TRUE,autoSerialize=TRUE)
    library(parallel)
    cl=makeCluster(1)
    clusterExport(cl,"b",envir = environment())
    expect_equal(clusterEvalQ(cl,b),list(b))
    stopCluster(cl)
})

test_that("cluster export, auto serialize, no auto export class def", {
    ## auto export off and serialize on
    ## expect error
    setAltClassSetting(className = "test",autoExportClassDef=FALSE,autoSerialize=TRUE)
    
    con1=showConnections(all = FALSE)
    library(parallel)
    #browser()
    cl=makeCluster(1)
    expect_error(clusterExport(cl,"b",envir = environment()))
    expect_error(stopCluster(cl))

    ## Close the error connection
    con2=showConnections(all = FALSE)
    errorIndex=as.integer(rownames(con2)[!rownames(con2)%in%rownames(con1)])
    close(getConnection(errorIndex))
    
    ## manually export functions
    ## should be no error
    # browser()
    cl=makeCluster(1)
    clusterExport(cl=cl,c("length_func","get_ptr_func","ptr_or_null_func"),envir = environment())
    clusterEvalQ(cl=cl,{
        library(AltWrapper)
        setAltClass("test", "real")
        setAltMethod("test", getLength = length_func)
        setAltMethod("test", getDataptr = get_ptr_func)
        setAltMethod("test", getDataptrOrNull = ptr_or_null_func)
    })
    clusterExport(cl,"b",envir = environment())
    expect_equal(clusterEvalQ(cl,b),list(b))
    stopCluster(cl)
})

test_that("cluster export, overwrite auto serialize", {
    ## auto export and serialize on
    setAltClassSetting(className = "test",autoExportClassDef=TRUE,autoSerialize=TRUE)

    serialize_func <- function(x) {
        as.numeric(seq_along(x))
    }
    unserialize_func <- function(myclass,x) {
        x
    }
    environment(serialize_func)=globalenv()
    environment(unserialize_func)=globalenv()
    
    setAltMethod(className="test",serialize = serialize_func)
    setAltMethod(className="test",unserialize = unserialize_func)

    library(parallel)
    cl=makeCluster(1)
    clusterExport(cl,"b",envir = environment())
    expect_equal(clusterEvalQ(cl,b),list(seq_along(b)))
    stopCluster(cl)
})
