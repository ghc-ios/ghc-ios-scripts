#!/bin/sh

echo "Downloading GHC for iOS devices..."
cd /tmp
wget https://www.haskell.org/ghc/dist/7.8.2/ghc-7.8.2-arm-apple-ios.tar.xz
tar xvf ghc-7.8.2-arm-apple-ios.tar.bz2 && mv ghc-7.8.2 ghc-7.8.2-arm
cd ghc-7.8.2-arm
./configure
# Fix the settings file to point to the right scripts
sed -i '' 's|/usr/bin/gcc|arm-apple-darwin10-clang|g' settings
sed -i '' 's|/usr/bin/ld|arm-apple-darwin10-ld|g' settings
sudo make install


echo "Downloading GHC for the iOS simulator..."
cd /tmp
wget https://www.haskell.org/ghc/dist/7.8.2/ghc-7.8.2-i386-apple-ios.tar.xz
tar xvf ghc-7.8.2-i386-apple-ios.tar.bz2 && mv ghc-7.8.2 ghc-7.8.2-i386
cd ghc-7.8.2-i386
./configure
# Fix the settings file to point to the right scripts
sed -i '' 's|/usr/bin/gcc|i386-apple-darwin11-clang|g' settings
sed -i '' 's|/usr/bin/ld|i386-apple-darwin11-ld|g' settings
sudo make install