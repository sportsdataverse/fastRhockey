"""Generate OpenAPI 3.0 specs for the three NHL APIs.

Reads `_endpoints_catalog.json` (sourced from
https://github.com/dfleis/nhl-api-docs) and emits one OpenAPI spec per
base URL in both JSON and YAML form. Re-runnable.
"""
import json
import re
from collections import OrderedDict, defaultdict
import yaml

CATALOG = "_endpoints_catalog.json"
PARAM_RE = re.compile(r"\{([A-Za-z_][A-Za-z0-9_]*)\}")


def humanize(name: str) -> str:
    return name.replace("_", " ").replace("-", " ").title()


def infer_param_schema(pname: str) -> dict:
    p = pname.lower()
    if p == "season":
        return {"type": "string", "description": "8-digit season e.g. 20242025"}
    if p in ("date", "month"):
        return {"type": "string", "description": "YYYY-MM-DD or YYYY-MM"}
    if p in ("gametype", "game_type"):
        return {
            "type": "integer",
            "enum": [1, 2, 3],
            "description": "1=preseason, 2=regular, 3=playoffs",
        }
    if p == "lang":
        return {"type": "string", "default": "en"}
    if p in ("team", "team_abbr", "tricode", "franchisetricode"):
        return {
            "type": "string",
            "description": "Three-letter team abbreviation (e.g. TOR)",
        }
    if p == "round":
        return {"type": "string", "description": "Round number or 'all'"}
    if p in (
        "playerid",
        "player_id",
        "teamid",
        "team_id",
        "franchiseid",
        "gameid",
        "game_id",
        "year",
        "seasonid",
        "trophyid",
        "officeid",
        "id",
        "eventnumber",
    ):
        return {"type": "integer", "format": "int64"}
    if p == "seriesletter":
        return {"type": "string", "description": "Playoff series letter (A-P)"}
    if p in ("position", "positions", "category", "sortby", "strength", "attribute", "report", "rankingcategory"):
        return {"type": "string"}
    if p == "templatekey":
        return {"type": "string"}
    if p == "countrycode":
        return {"type": "string", "description": "ISO country code"}
    if p == "postalcode":
        return {"type": "string"}
    if p == "path":
        return {"type": "string"}
    if p == "type":
        return {"type": "string"}
    return {"type": "string"}


def build_path_item(label: str, entry: dict) -> dict:
    path_template = entry["path_template"]
    params = []
    seen = set()
    for pname in PARAM_RE.findall(path_template):
        if pname in seen:
            continue
        seen.add(pname)
        params.append(
            {
                "name": pname,
                "in": "path",
                "required": True,
                "description": f"Path parameter `{pname}`",
                "schema": infer_param_schema(pname),
            }
        )

    qp = entry.get("query_params")
    qp_list = []
    if isinstance(qp, list):
        qp_list = qp
    elif isinstance(qp, dict):
        names = qp.get("name")
        types = qp.get("type")
        if isinstance(names, list):
            tlist = types if isinstance(types, list) else [None] * len(names)
            qp_list = [{"name": n, "type": t} for n, t in zip(names, tlist)]
        elif isinstance(names, str) and names:
            qp_list = [{"name": names, "type": types if isinstance(types, str) else None}]
    for q in qp_list:
        if not isinstance(q, dict):
            continue
        qname = q.get("name")
        if not qname:
            continue
        qtype = (q.get("type") or "string").lower()
        if qtype in ("int", "integer", "long"):
            qschema = {"type": "integer"}
        elif qtype in ("bool", "boolean"):
            qschema = {"type": "boolean"}
        elif qtype in ("float", "number", "double"):
            qschema = {"type": "number"}
        else:
            qschema = {"type": "string"}
        params.append(
            {
                "name": qname,
                "in": "query",
                "required": False,
                "description": f"Query parameter `{qname}`",
                "schema": qschema,
            }
        )

    op = OrderedDict()
    op["summary"] = humanize(label)
    op["operationId"] = label
    # Strip a common `db_` prefix used by the Stats REST catalog so the
    # second token becomes the meaningful tag
    base = label[3:] if label.startswith("db_") else label
    tag = base.split("_")[0] if "_" in base else base.split("-")[0]
    op["tags"] = [tag]
    if params:
        op["parameters"] = params
    op["responses"] = {
        "200": {
            "description": "Successful response",
            "content": {"application/json": {"schema": {"type": "object"}}},
        }
    }
    return {"get": op}


def make_spec(base_url: str, entries: list, title: str, description: str) -> OrderedDict:
    server_url = base_url.rstrip("/")
    paths = OrderedDict()
    for label, entry in sorted(entries, key=lambda x: x[1]["path_template"]):
        path = "/" + entry["path_template"].lstrip("/")
        if path in paths:
            continue
        paths[path] = build_path_item(label, entry)

    tag_set = set()
    for item in paths.values():
        for op in item.values():
            for t in op.get("tags", []):
                tag_set.add(t)
    tags = [{"name": t} for t in sorted(tag_set)]

    spec = OrderedDict()
    spec["openapi"] = "3.0.3"
    spec["info"] = {
        "title": title,
        "version": "1.0.0",
        "description": description,
        "contact": {
            "name": "fastRhockey maintainers",
            "url": "https://github.com/sportsdataverse/fastRhockey",
        },
        "license": {"name": "MIT"},
    }
    spec["servers"] = [{"url": server_url}]
    spec["tags"] = tags
    spec["paths"] = paths
    return spec


SPECS = [
    {
        "base": "https://api-web.nhle.com/",
        "title": "NHL Web API (api-web.nhle.com)",
        "description": (
            "Public NHL Web API used by the official NHL.com clients. "
            "No authentication. Endpoint catalog sourced from "
            "https://github.com/dfleis/nhl-api-docs and cross-referenced "
            "with https://github.com/RentoSaijo/nhlscraper/wiki and "
            "https://github.com/coreyjs/nhl-api-py."
        ),
        "fname": "nhl_api_web_openapi",
    },
    {
        "base": "https://api.nhle.com/stats/rest/",
        "title": "NHL Stats REST API (api.nhle.com/stats/rest)",
        "description": (
            "NHL Stats REST API. Supports Cayenne filter expressions via "
            "the `cayenneExp` query parameter. No authentication."
        ),
        "fname": "nhl_stats_rest_openapi",
    },
    {
        "base": "https://records.nhl.com/site/api/",
        "title": "NHL Records API (records.nhl.com)",
        "description": (
            "NHL records and historical statistics API hosting franchise, "
            "player, coach, draft, milestone, international and Stanley Cup "
            "records. Supports `cayenneExp` query filters. No authentication."
        ),
        "fname": "nhl_records_openapi",
    },
]


class OrderedDumper(yaml.SafeDumper):
    pass


def _dict_representer(dumper, data):
    return dumper.represent_mapping(
        yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, data.items()
    )


OrderedDumper.add_representer(OrderedDict, _dict_representer)


def main() -> None:
    catalog = json.load(open(CATALOG))
    by_base = defaultdict(list)
    for k, v in catalog.items():
        by_base[v["base_url"]].append((k, v))

    for cfg in SPECS:
        entries = by_base.get(cfg["base"], [])
        spec = make_spec(cfg["base"], entries, cfg["title"], cfg["description"])
        json_path = cfg["fname"] + ".json"
        yaml_path = cfg["fname"] + ".yaml"
        with open(json_path, "w", encoding="utf-8") as f:
            json.dump(spec, f, indent=2)
        with open(yaml_path, "w", encoding="utf-8") as f:
            yaml.dump(
                spec,
                f,
                Dumper=OrderedDumper,
                default_flow_style=False,
                sort_keys=False,
                allow_unicode=True,
            )
        print(f"{cfg['fname']}: {len(entries)} endpoints -> {json_path}, {yaml_path}")


if __name__ == "__main__":
    main()
