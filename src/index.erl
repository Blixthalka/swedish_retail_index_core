-module(index).

-record(index, {
    values :: [{binary(), decimal:decimal()}]
}).