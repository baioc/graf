#!/bin/sh

docker run -it --rm \
	-v "$(pwd)":/source -w /source dotnet /bin/bash \
	-c "/opt/dotnet publish -c release -o build/ && chown -R $(id -u):$(id -g) ."
