import System.Environment
import System.Process

-- | This script wraps clang to pass it essential arguments that keep it from dying when
-- GHC uses it as a C pre-processor during the build process.
-- To use it, choose the version of clang you'd like to use, 
-- (GHC-iOS needs to use Xcode5's clang, which is already set here)
-- then compile it (ghc clang-xcode5-wrapper.hs).
-- Then, find your GHC settings file (usually something like /usr/local/lib/ghc-7.6.3/settings)
-- and change the "C compiler command" to point to the resultant wrapper executable.

-- N.B. you can also just put it in your PATH, which lets you refer to it simply 
-- as "clang-xcode5-wrapper" in GHC settings and "./configure --with-gcc=clang-xcode5-wrapper"

clang = "/usr/bin/clang" -- system clang

flags = 
	[ "-Wno-invalid-pp-token" 
	, "-Wno-unicode"
	, "-Wno-trigraphs"
	]

-- See if we're in preprocessor mode
check args@("-E":"-undef":"-traditional":_) = replace args
check other = other

-- make sure we use assembler-with-cpp
replace ("-x":"c":xs) = "-x":"assembler-with-cpp":replace xs
replace (x:xs) = x:replace xs
replace [] = []

main = do
	args <- getArgs
	rawSystem clang $ flags ++ (check args)

