-module(agb_variable).

-callback table()->atom() | {atom(),term()}.
-callback put(any(),term())->boolean().
-callback geto(any())->term().
-callback getv(any())->term().

