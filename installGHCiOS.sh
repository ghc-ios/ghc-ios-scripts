#!/bin/sh

cd /tmp

if [[ ! -f /usr/local/clang-3.0/bin/llc ]]; then
    echo "Downloading LLVM 3.0..."
    curl -O http://llvm.org/releases/3.0/clang+llvm-3.0-x86_64-apple-darwin11.tar.gz
    tar xvf clang+llvm-3.0-x86_64-apple-darwin11.tar.gz
    mv clang+llvm-3.0-x86_64-apple-darwin11 /usr/local/clang-3.0
    rm clang+llvm-3.0-x86_64-apple-darwin11.tar.gz
fi


echo "Downloading GHC for iOS devices..."

curl -L -O https://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-arm-apple-ios.tar.xz
tar xvf ghc-7.8.3-arm-apple-ios.tar.xz && mv ghc-7.8.3 ghc-7.8.3-arm
rm ghc-7.8.3-arm-apple-ios.tar.xz
cd ghc-7.8.3-arm

# Remove befuddling inclusion of my local paths
LC_CTYPE=C 
LANG=C
find . -type f -not -name .DS_Store -not -name "*.a" -print0 | xargs -0 sed -i '' 's|/Users/lukexi/Code/ghc-ios-scripts/||g'

./configure
# Fix the settings file to point to the right scripts and clang versions
sed -i '' 's|/usr/bin/gcc|arm-apple-darwin10-clang|g' settings
sed -i '' 's|/usr/bin/ld|arm-apple-darwin10-ld|g' settings
sed -i '' 's|"opt"|"/usr/local/clang-3.0/bin/opt"|g' settings
sed -i '' 's|"llc"|"/usr/local/clang-3.0/bin/llc"|g' settings
make install
cd ..
rm -r ghc-7.8.3-arm

echo "Downloading GHC for the iOS simulator..."
cd /tmp
curl -L -O https://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-i386-apple-ios.tar.xz
tar xvf ghc-7.8.3-i386-apple-ios.tar.xz && mv ghc-7.8.3 ghc-7.8.3-i386
rm ghc-7.8.3-i386-apple-ios.tar.xz
cd ghc-7.8.3-i386

# ditto above
LC_CTYPE=C 
LANG=C
find . -type f -not -name .DS_Store -not -name "*.a" -print0 | xargs -0 sed -i '' 's|/Users/lukexi/Code/ghc-ios-scripts/||g'

./configure
# Fix the settings file to point to the right scripts and clang versions
sed -i '' 's|/usr/bin/gcc|i386-apple-darwin11-clang|g' settings
sed -i '' 's|/usr/bin/ld|i386-apple-darwin11-ld|g' settings
sed -i '' 's|"opt"|"/usr/local/clang-3.0/bin/opt"|g' settings
sed -i '' 's|"llc"|"/usr/local/clang-3.0/bin/llc"|g' settings
make install
cd ..
rm -r ghc-7.8.3-i386
