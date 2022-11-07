#!/bin/sh
set -e
# set -x

# helpers
beginswith() { case $2 in "$1"*) true;; *) false;; esac; }

# parameterize
if [ -z "$1" ]; then
	echo "Must specify a release version!"
	exit 1
else
	VERSION="$1"
fi
if [ -z "$2" ]; then
	TARGET='linux-x64'
else
	TARGET="$2"
fi

# derive
if [ "$TARGET" = 'linux-x64' ]; then
	FRAMEWORK='net7.0'
else
	FRAMEWORK='net6.0'
fi

# prepare
docker build -t dotnet:7.0-bionic .

# compile
docker run -it --rm \
	-v "$(pwd)":/source -w /source \
	dotnet:7.0-bionic \
	/bin/bash -c "/opt/dotnet publish -o build/ -c Release -r $TARGET -f $FRAMEWORK && chown -R $(id -u):$(id -g) ."

# package
cd build
if [ "$TARGET" = 'linux-x64' ]; then
	tar -czf graf-$VERSION-$TARGET.tar.gz graf
	sha256sum graf-$VERSION-$TARGET.tar.gz | tee graf-$VERSION-$TARGET.tar.gz.sha256
elif beginswith win $TARGET; then
	VERSION=$(echo "$VERSION" | sed 's/\./_/g')
	zip -9 -k graf-$VERSION-$TARGET.zip graf.exe
	sha256sum graf-$VERSION-$TARGET.zip | tee graf-$VERSION-"$TARGET"_zip_sha256.txt
fi
cd ..
