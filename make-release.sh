#!/bin/sh
set -e

# parameterize
TARGET='linux-x64'
if [ -z "$1" ]; then
	echo "Must specify a version"; exit 1
else
	VERSION="$1";
fi

# prepare
docker build -t dotnet:7.0-bionic .

# compile
docker run -it --rm \
	-v "$(pwd)":/source -w /source \
	dotnet:7.0-bionic \
	/bin/bash -c "/opt/dotnet publish -c Release -o build/ && chown -R $(id -u):$(id -g) ."

# package
cd build
tar -czf graf-$VERSION-$TARGET.tar.gz graf
sha256sum graf-$VERSION-$TARGET.tar.gz | tee graf-$VERSION-$TARGET.tar.gz.sha256
cd ..
