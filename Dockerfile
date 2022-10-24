FROM ubuntu:18.04

# Install dependencies
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libc6 libgcc1 libgssapi-krb5-2 libicu60 libssl1.1 libstdc++6 zlib1g \
        libgdiplus \
        clang zlib1g-dev \
        curl ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install .NET
RUN curl -sSL -k -o dotnet-install.sh https://dot.net/v1/dotnet-install.sh \
    && chmod +x dotnet-install.sh \
    && ./dotnet-install.sh --channel 7.0 --install-dir /opt \
    && opt/dotnet help \
    && rm dotnet-install.sh
