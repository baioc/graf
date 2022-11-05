#!/bin/sh

docker build -t dotnet:7.0-bionic .

docker run -it --rm \
	-v "$(pwd)":/source -w /source \
	dotnet:7.0-bionic \
	/bin/bash -c "/opt/dotnet publish -c Release -o build/ && chown -R $(id -u):$(id -g) ."
