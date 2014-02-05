
* Download these scripts from https://github.com/ghc-ios/ghc-ios-scripts. Place them in your path.
* Download http://llvm.org/releases/3.0/clang+llvm-3.0-x86_64-apple-darwin11.tar.gz and place them somewhere easy to remember (e.g. /usr/local/clang-3.0/ — not in your path, since you don't want to override Xcode's clang)
* Download and unpack https://github.com/ghc-ios/ghc-ios-scripts/releases/download/7.8-rc1-device/ghc-7.8.20140129-arm-apple-ios.tar.bz2
* After running 
```
./configure
```

* Edit the "settings" file.
    * (if you've already run "make install", you can edit the file in /usr/local/lib/arm-apple-darwin10-ghc-7.8.20140129/settings)
    * Ensure "C compiler command" is "arm-apple-darwin10-clang"
    * Ensure "ld command" is "arm-apple-darwin10-ld"
    * Ensure "LLVM llc command" is the full path to llc from Clang+LLVM 3.0 (as downloaded above, e.g. /usr/local/clang-3.0/bin/llc)
    * Ditto for "LLVM opt command" (e.g. "/usr/local/clang-3.0/bin/opt")
    
* Install the compiler
```
make install
```

* Create a file named Counter.hs
```haskell
{-# LANGUAGE ForeignFunctionInterface #-}
module Counter where
import Control.Concurrent
import Control.Monad
foreign export ccall startCounter :: Int -> IO ()
startCounter :: Int -> IO ()
startCounter = void . forkIO . void . loop
    where loop i = do
            putStrLn (replicate i 'o')
            threadDelay (10^6)
            loop (i + 1)
```
* Compile this like so to get Counter.a and Counter_stub.h
```
arm-apple-darwin10-ghc -staticlib -threaded Counter.hs -o Counter
```

* Create or open an Xcode project (the Single View Application template is simple for testing)

* Drag Counter.a and Counter_stub.h to the project's sidebar. Make sure "Add to Targets:" has a check next to your app.

* Click on your project at the top of the Xcode sidebar. 
    * In the "Build Phases" tab, under "Link Binary with Libraries", click the + and choose libiconv.dylib
    * In the "Build Settings" tab, set "Dead Code Stripping" to No, and add /usr/local/lib/arm-apple-darwin10-ghc-7.8.20140129/include/ to "Header Search Paths" (ensure this matches the date of the GHC-iOS binary you downloaded)

* In your app's AppDelegate.m, 
```
#import "Counter_stub.h"
```
* and add
```
    hs_init(NULL, NULL);
    startCounter(3);
```
* to the top of application:didFinishLaunchingWithOptions: .

* Run your app! You should see a growing triangle of 'o's.
