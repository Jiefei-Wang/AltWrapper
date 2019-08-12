context("package settings")

test_that("get and set options",{
    opt=getAltWrapperOptions()
    setAltWrapperOptions(redefineWarning=FALSE)
    expect_true(!identical(opt,getAltWrapperOptions()))
})

test_that("redefine warnings",{
    setAltClass(className = "pkgOptions",classType = "integer")
    setAltWrapperOptions(redefineWarning=FALSE)
    expect_equal(setAltClass(className = "pkgOptions",classType = "integer"),NULL)
    setAltWrapperOptions(redefineWarning=TRUE)
    expect_warning(setAltClass(className = "pkgOptions",classType = "integer"))
})