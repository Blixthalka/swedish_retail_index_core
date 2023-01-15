FROM erlang:24-alpine

RUN apk update
# fetching deps
RUN apk add git
# c compiler
RUN apk add build-base

RUN mkdir /buildroot
WORKDIR /buildroot

COPY . /buildroot

RUN rebar3 release

FROM erlang:24-alpine

RUN apk add --no-cache libstdc++
COPY --from=0 /buildroot/_build/default/rel/swedish_retail_index /swedish_retail_index

RUN mkdir -p /swedish_retail_index/mnesia
EXPOSE 8082

CMD ["/swedish_retail_index/bin/swedish_retail_index", "foreground"]
