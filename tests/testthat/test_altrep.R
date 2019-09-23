context("Test altrep")
length_func <- function(x) {
    length(x)
}

inspect_func <- function(x) {
    cat("Altrep object")
}
get_element_func <- function(x, i) {
    #message("element")
    return(x[i])
}
get_subset_func <- function(x, ind) {
    #message("subset")
    return(x[ind])
}
get_ptr_func <- function(x, writeable) {
    #message("pointer")
    return(x)
}
ptr_or_null_func <- function(x) {
    #message("pointer or null")
    return(x)
}

duplicate_func <- function(x, deep) {
    #message("duplicated")
    C_create_altrep("test", C_duplicate_object(x, !deep))
}
region_func <- function(x, start, size, output) {
    #message("region")
    n = length(x) - start + 1
    if (n > size)
        n = size
    for (i in seq_len(n)) {
        output[i] = x[start + i - 1]
    }
    n
}
coerce_func <- function(x, type) {
    #message("type:",type)
    x
}
serialize_func <- function(x) {
    #message("I'm serializing")
    x
}
unserialize_func <- function(x) {
    #message("I'm unserializing")
    x
}
classTypeList = list(integer = as.integer, real = as.numeric)
deleteClass(className = "test",warning=FALSE)

for (i in seq_along(classTypeList)) {
    classType = names(classTypeList)[i]
    
    ## Define the class
    setAltClass("test", classType)
    setAltMethod("test", getLength = length_func)
    setAltMethod("test", getDataptr = get_ptr_func)
    setAltMethod("test", getDataptrOrNull = ptr_or_null_func)
    
    a = classTypeList[[i]](runif(10) * 100)
    b = makeAltrep("test", a)
    
    test_that("creation", {
        expect_equal(a, b)
    })
    
    test_that("subset", {
        expect_equal(a[1], b[1])
        expect_equal(a[1:5], b[1:5])
    })
    
    test_that("sum", {
        expect_equal(sum(a), sum(b))
    })
    
    test_that("get class name", {
        expect_equal(getAltClassName(x = b), "test")
    })
    
    ################################
    ## Function argument check
    ################################
    test_that("get class name", {
        expect_error(setAltMethod("test", getDataptrOrNull = print))
    })
    
    ################################
    ## Get settings from data
    ################################
    test_that("get class type from data", {
        expect_equal(getClassType(x = b), classType)
    })
    test_that("get class method from data", {
        expect_null(getAltMethod(x = b, methodName = "inspect"))
        expect_equal(
            getAltMethod(x = b, methodName = c("getLength","getDataptr")),
            c(length_func,get_ptr_func)
            )
    })
    
    test_that("Inspect class status from data", {
        expect_output(altClassStatus(x = b), NULL)
    })
    
    ################################
    ## Get settings from class name
    ################################
    
    test_that("get class type from class name", {
        expect_equal(getClassType(className = "test"), classType)
        expect_error(getClassType(className = "test1"))
    })
    
    test_that("get class method from class name", {
        expect_null(getAltMethod(className = "test", methodName = "inspect"))
        expect_null(getAltMethod(className = "test1", methodName = "inspect"))
        
        expect_equal(
            getAltMethod(className = "test", methodName = c("getLength","getDataptr")),
            c(length_func,get_ptr_func)
        )
        
    })
    
    test_that("Inspect class status from class name", {
        expect_output(altClassStatus(className = "test"), NULL)
        expect_error(altClassStatus(className = "test1"))
    })
    
    
    
    
    ################################
    ## AltWrapper set self data
    ################################
    test_that("set self data", {
        getSum<-function(x,na.rm){
            x[1]=as(10,typeof(x))
            setAltSelfData(x)
            sum(x)
        }
        setAltMethod(className="test",sum = getSum)
        sum(b)
        expect_true(b[1]==10)
    })
    
    ################################
    ## AltWrapper tools
    ################################
    test_that("check method existance", {
        expect_true(isAltMethodExist(className="test",methodName="getDataptr"))
        expect_false(isAltMethodExist(className="test1",methodName="getDataptr"))
        expect_false(isAltMethodExist(className="test",methodName="noNA"))
    })
    
    test_that("get altWrapper data", {
        expect_false(is.altrep(getAltWrapperData(b)))
        b = setAltWrapperData(x=b,value=a)
        expect_equal(b,a)
    })
    
   
    ################################
    ## class deletion
    ################################
    test_that("remove class", {
        expect_warning(deleteClass(className = "test1"))
        expect_error(deleteClass(className = "test"), NA)
        expect_error(b, NA)
    })
}



