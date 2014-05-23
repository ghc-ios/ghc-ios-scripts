#!/bin/sh

cd /tmp

echo "Downloading LLVM 3.0..."
curl -O http://llvm.org/releases/3.0/clang+llvm-3.0-x86_64-apple-darwin11.tar.gz
tar xvf clang+llvm-3.0-x86_64-apple-darwin11.tar.gz
mv clang+llvm-3.0-x86_64-apple-darwin11 /usr/local/clang-3.0
rm clang+llvm-3.0-x86_64-apple-darwin11.tar.gz

echo "Downloading GHC for iOS devices..."

curl -O https://www.haskell.org/ghc/dist/7.8.2/ghc-7.8.2-arm-apple-ios.tar.xz
tar xvf ghc-7.8.2-arm-apple-ios.tar.xz && mv ghc-7.8.2 ghc-7.8.2-arm
rm ghc-7.8.2-arm-apple-ios.tar.xz
cd ghc-7.8.2-arm
./configure
# Fix the settings file to point to the right scripts and clang versions
sed -i '' 's|/usr/bin/gcc|arm-apple-darwin10-clang|g' settings
sed -i '' 's|/usr/bin/ld|arm-apple-darwin10-ld|g' settings
sed -i '' 's|opt|/usr/local/clang-3.0/bin/opt|g' settings
sed -i '' 's|llc|/usr/local/clang-3.0/bin/llc|g' settings
# Fix a screwy 7.8.2 arm bindist. Can be removed for 7.8.3.
LC_ALL=C find ./ -type f -exec sed -i -e 's/cfgOnAppThread//g' {} \;
make install
cd ..
rm -r ghc-7.8.2-arm

echo "Downloading GHC for the iOS simulator..."
cd /tmp
curl -O https://www.haskell.org/ghc/dist/7.8.2/ghc-7.8.2-i386-apple-ios.tar.xz
tar xvf ghc-7.8.2-i386-apple-ios.tar.xz && mv ghc-7.8.2 ghc-7.8.2-i386
rm ghc-7.8.2-i386-apple-ios.tar.xz
cd ghc-7.8.2-i386
./configure
# Fix the settings file to point to the right scripts and clang versions
sed -i '' 's|/usr/bin/gcc|i386-apple-darwin11-clang|g' settings
sed -i '' 's|/usr/bin/ld|i386-apple-darwin11-ld|g' settings
sed -i '' 's|opt|/usr/local/clang-3.0/bin/opt|g' settings
sed -i '' 's|llc|/usr/local/clang-3.0/bin/llc|g' settings
make install
cd ..
rm -r ghc-7.8.2-i386
