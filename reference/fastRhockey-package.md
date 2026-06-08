# fastRhockey: Functions to Access Professional Women's Hockey League and National Hockey League Play by Play Data

A utility to scrape and load play-by-play data and statistics from the
Professional Women's Hockey League <https://www.thepwhl.com/>, formerly
known as the Premier Hockey Federation (PHF) or National Women's Hockey
League (NWHL). Additionally, allows access to the National Hockey
League's stats API <https://www.nhl.com/>.

## Proxy support

All HTTP requests flow through an internal httr2 helper that resolves a
proxy in this order:

1.  an explicit `proxy =` argument (only on wrappers that pass `...`);

2.  `getOption("fastRhockey.proxy")` – set once per session with
    `options(fastRhockey.proxy = "http://host:port")` and every call
    picks it up;

3.  the `http_proxy` / `https_proxy` / `no_proxy` environment variables,
    read by libcurl when no explicit proxy is supplied.

The proxy value may be a URL string (`"http://host:port"`) or a named
list spread into
[`httr2::req_proxy()`](https://httr2.r-lib.org/reference/req_proxy.html)
for authenticated proxies, e.g.
`list(url = "http://host", port = 8080, username = "u", password = "p", auth = "basic")`.
The session-[`options()`](https://rdrr.io/r/base/options.html) form is
recommended when one proxy covers many calls.

## See also

Useful links:

- <https://fastRhockey.sportsdataverse.org/>

- <https://github.com/sportsdataverse/fastRhockey>

- Report bugs at <https://github.com/sportsdataverse/fastRhockey/issues>

## Author

**Maintainer**: Saiem Gilani <saiem.gilani@gmail.com>
([ORCID](https://orcid.org/0000-0002-7194-9067))

Authors:

- Ben Howell <benhowell71@gmail.com>

Other contributors:

- Alyssa Longmuir <aklongmuir@gmail.com> \[contributor\]
