# fastRhockey: Access Hockey Play-by-Play Data and Statistics

Scrape and load hockey play-by-play data and statistics. Sources include
the National Hockey League (NHL) <https://www.nhl.com/> web, stats, 'NHL
Edge' tracking and records application programming interfaces (APIs),
the Professional Women's Hockey League <https://www.thepwhl.com/>, and
the American Hockey League, Ontario Hockey League, Western Hockey League
and Quebec Maritimes Junior Hockey League through their 'HockeyTech'
feeds. It also reads NHL data from 'ESPN' and 'Fox Sports', National
Collegiate Athletic Association (NCAA) men's and women's hockey from
'ESPN', and 13 European leagues: Germany's Deutsche Eishockey Liga
(DEL), Austria's International Central European (ICE) Hockey League, the
United Kingdom's Elite Ice Hockey League (EIHL), Denmark's Metal Ligaen,
the Kontinental Hockey League (KHL), Russia's Supreme Hockey League
(VHL), the Swedish Hockey League (SHL) and HockeyAllsvenskan, Finland's
Liiga, Switzerland's National League and Swiss Ice Hockey Federation
competitions, Czechia's Extraliga and France's Ligue Magnus. Loaders
read pre-scraped play-by-play, box score, schedule and expected-goals
datasets from the 'SportsDataverse' data releases.

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

- Saiem Gilani <saiem.gilani@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-7194-9067))

- Ben Howell <benhowell71@gmail.com>

Other contributors:

- Alyssa Longmuir <aklongmuir@gmail.com> \[contributor\]

- fastRhockey authors \[copyright holder\]
