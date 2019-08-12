context("altrep")

length_func<-function(x){
    return(length(x))
}
setAltClass(className = "test", classType = "real")
setAltMethod(className = "test", getLength = length_func)

testData = runif(10)
myAltrep = makeAltrep("test", testData)

test_that("Auto serialize", {
    browser()
    A = 10
    A_serialized=serialize(A,NULL)
    B = new.env()
    B_serialized=serialize(B,NULL)
})


# test_that("Auto serialize", {
#     #auto serialize on
#     setAltClassSetting(className = "test",autoExportClassDef=TRUE,autoSerialize=TRUE)
#     expect_false(is.null(getAltClassSetting(x=b)))
#     b_serilized=serialize(b,NULL)
#     #b = makeAltrep("test", a)
#     stop()
#     b1=unserialize(b_serilized)
#     expect_equal(b1,b)
#     expect_true(is.altWrapper(b1))
# })
