module Main

Fable.Core.JS.console.log("Started tests: Main")

#if !HEADLESS

HelloWorldTest.init()
DOMTest.init()
BindingTest.init()
ObservableTest.init()
StoreTest.init()

BrowserFramework.runAll( 
    "*", "*"
    )

#endif
