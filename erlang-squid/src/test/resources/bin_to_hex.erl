-module(bin_to_hex).

-define(H(X), (hex(X)):16).

hex(X) -> X.
