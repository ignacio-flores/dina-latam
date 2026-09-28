* Retired configuration entry point. It is intentionally not a fallback.
di as error "_config.do is retired and cannot configure a DINA run."
di as error "Use `dina run`, or generate an explicit runtime file with `dina config stata --output PATH`."
exit 198
