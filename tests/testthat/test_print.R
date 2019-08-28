context("Test altrep print function")
length_func <- function(x) {
    length(x)
}

ptr_or_null_func <- function(x) {
    #message("pointer or null")
    return(x)
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
get_element_func <- function(x, i) {
    #message("element")
    return(x[i])
}
get_subset_func <- function(x, ind) {
    #message("subset")
    return(x[ind])
}

deleteClass(className = "test",warning=FALSE)
setAltClass("test", "real")
setAltMethod("test", getLength = length_func)
a = runif(10)
b_s3=makeAltrep("test",a,S3Class=TRUE)
b_s4=makeAltrep("test",a,S4Class=TRUE)


################################
## Test print function
################################

test_that("No print method defined",{
    expect_error(print(b_s3))
    expect_error(print(b_s4))
})
test_that("print object from pointer", {
    setAltMethod(className="test",getDataptrOrNull = ptr_or_null_func)
    expect_output(print(b_s3))
    expect_output(print(b_s4))
})
test_that("print object from region", {
    setAltMethod(className="test",getDataptrOrNull = NULL)
    setAltMethod(className="test", getRegion = region_func)
    expect_output(print(b_s3))
    expect_output(print(b_s4))
})
test_that("print object from subset", {
    setAltMethod(className="test",getRegion = NULL)
    setAltMethod(className="test", getSubset  = get_subset_func)
    expect_output(print(b_s3))
    expect_output(print(b_s4))
})

test_that("print object from element", {
    setAltMethod(className="test",getSubset = NULL)
    setAltMethod(className="test", getElement  = get_element_func)
    expect_output(print(b_s3))
    expect_output(print(b_s4))
})
