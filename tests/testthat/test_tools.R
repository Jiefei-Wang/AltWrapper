context("Tools")
length_func <- function(x) {
    length(x)
}

get_ptr_func <- function(x, writeable) {
    return(x)
}
ptr_or_null_func <- function(x) {
    return(x)
}

deleteAltClass(className = "test",warning=FALSE)
setAltClass("test", "double")
setAltMethod("test", getLength = length_func)
setAltMethod("test", getDataptr = get_ptr_func)
setAltMethod("test", getDataptrOrNull = ptr_or_null_func)

a=runif(10)
b = newAltrep("test", a)

test_that("is altrep",{
    expect_false(is.altrep(a))
    expect_true(is.altrep(b))
    expect_true(is.altrep(1:10))
})

test_that("is altWrapper",{
    expect_false(is.altWrapper(a))
    expect_true(is.altWrapper(b))
    expect_false(is.altWrapper(1:10))
})

test_that("getAltData1&2",{
    expect_true(is.null(.getAltData1(a)))
    expect_true(!is.null(.getAltData1(b)))
    
    
    expect_true(is.null(.getAltData2(a)))
    expect_true(!is.null(.getAltData2(b)))
})


test_that("setAltData1&2",{
    expect_false(.setAltData1(a,a))
    expect_true(.setAltData1(b,a))
    
    
    expect_false(.setAltData2(a,a))
    expect_true(.setAltData2(b,.getAltData2(b)))
})


test_that("remove wrapper",{
    expect_error(removeWrapper(a))
})

