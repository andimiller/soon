FROM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.9.9_3.4.0 as build
WORKDIR /src
RUN apt update -y
RUN apt install -y clang mold libstdc++-12-dev libgc-dev libutf8proc-dev libssl-dev
COPY . /src
RUN SBT_OPTS="-Xmx4G" sbt "test;nativeLink;stageBinary"

FROM scratch
COPY --from=build /src/soon /soon
ENTRYPOINT ["/soon"]

