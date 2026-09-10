#!/usr/bin/env python3
"""Seed dgiot role/rules for a headless hub (idempotent).

Why: a freshly built dgiot hub has no _Role/Permission objects (the
CentOS installer seeds them, the openEuler source build does not), so
every business route answers {"code":119} Forbidden because the login
session carries rules=[]. This tool rebuilds the dgiot-native chain:

    _User --(users)--> _Role --(rules)--> Permission(name=OPERATION_ID)

which dgiot_parse_auth:get_rules/3 walks at login time
($relatedTo query on the Permission class).

Credentials come from the environment only - never commit them:

    HUB_PARSE_URL     default http://127.0.0.1:1337
    HUB_PARSE_APPID   parse application id
    HUB_PARSE_MASTER  parse master key
    HUB_ADMIN_USER    existing admin user to bind (default admin)

Usage:
    python3 seed_roles.py --probe          # report only, change nothing
    python3 seed_roles.py                  # seed (idempotent)
    python3 seed_roles.py --extra GET_THING,GET_TDENGINE
"""
import argparse
import json
import os
import sys
import urllib.parse
import urllib.request

DEFAULT_RULES = [
    # business reads
    "GET_PRODUCT", "GET_DEVICE", "GET_PRODUCTTREE", "GET_THING",
    "GET_TDENGINE", "GET_CHANNEL", "GET_NOTIFY",
    # business writes (downlink)
    "POST_DEVICE_DEBUG", "POST_PRODUCT", "POST_DEVICE",
    # parse class reads / writes (route operation ids)
    "GET_CLASSES_PRODUCT", "GET_CLASSES_DEVICE", "GET_CLASSES_USER",
    "GET_CLASSES_ROLE", "GET_CLASSES_THING", "GET_CLASSES_CHANNEL",
    "POST_CLASSES_PRODUCT", "PUT_CLASSES_PRODUCT",
    "POST_CLASSES_DEVICE", "PUT_CLASSES_DEVICE",
    "POST_CLASSES_THING", "PUT_CLASSES_THING",
]
ROLE_NAME = "admin"
ROLE_FIELDS = {"org_type": "SW", "level": 1, "tag": {}, "parent": {}}


class Hub:
    def __init__(self, base, appid, master):
        self.base = base.rstrip("/") + "/parse"
        self.headers = {
            "X-Parse-Application-Id": appid,
            "X-Parse-Master-Key": master,
            "Content-Type": "application/json",
        }

    def _call(self, method, path, payload=None, params=None):
        url = f"{self.base}{path}"
        if params:
            url += "?" + urllib.parse.urlencode(params)
        data = json.dumps(payload).encode() if payload is not None else None
        req = urllib.request.Request(url, data=data, headers=self.headers,
                                     method=method)
        with urllib.request.urlopen(req, timeout=15) as r:
            return json.loads(r.read().decode())

    def find(self, cls, where, keys=None):
        params = {"where": json.dumps(where), "limit": 1}
        if keys:
            params["keys"] = ",".join(keys)
        return self._call("GET", f"/classes/{cls}", params=params)

    def create(self, cls, payload):
        return self._call("POST", f"/classes/{cls}",
                          {"ACL": {"*": {"read": True}}, **payload})

    def update(self, cls, oid, payload):
        return self._call("PUT", f"/classes/{cls}/{oid}", payload)


def seed(hub, admin_user, rules, probe_only=False):
    """Returns a report dict; idempotent on re-run."""
    report = {"role": None, "created_role": False,
              "permissions_created": [], "user": admin_user,
              "rules": len(rules)}
    user = hub.find("_User", {"username": admin_user},
                    keys=["objectId"])["results"]
    if not user:
        raise SystemExit(f"admin user '{admin_user}' not found on the hub")
    user_id = user[0]["objectId"]
    report["user_id"] = user_id

    role = hub.find("_Role", {"name": ROLE_NAME})["results"]
    if role:
        role_id = role[0]["objectId"]
        report["role"] = role_id
    elif probe_only:
        report["role"] = "MISSING"
        return report
    else:
        role_id = hub.create("_Role", {"name": ROLE_NAME,
                                       "alias": ROLE_NAME})["objectId"]
        report["role"] = role_id
        report["created_role"] = True

    if probe_only:
        return report

    # dgiot_parse_auth:get_role/3 pattern-matches these fields
    hub.update("_Role", role_id, ROLE_FIELDS)

    perm_ids = []
    for op in rules:
        found = hub.find("Permission", {"name": op}, keys=["objectId"])["results"]
        if found:
            perm_ids.append(found[0]["objectId"])
        else:
            perm_ids.append(hub.create("Permission", {"name": op})["objectId"])
            report["permissions_created"].append(op)

    ptr = lambda c, o: {"__type": "Pointer", "className": c, "objectId": o}
    hub.update("_Role", role_id, {"users": {"__op": "AddRelation",
                                            "objects": [ptr("_User", user_id)]}})
    hub.update("_Role", role_id, {"rules": {"__op": "AddRelation",
                                            "objects": [ptr("Permission", p)
                                                        for p in perm_ids]}})
    return report


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--probe", action="store_true",
                    help="report current state without writing")
    ap.add_argument("--extra", default="",
                    help="comma separated extra operation ids")
    ap.add_argument("--admin", default=os.environ.get("HUB_ADMIN_USER", "admin"))
    args = ap.parse_args()

    appid = os.environ.get("HUB_PARSE_APPID")
    master = os.environ.get("HUB_PARSE_MASTER")
    if not appid or not master:
        sys.exit("set HUB_PARSE_APPID and HUB_PARSE_MASTER (no defaults: "
                 "credentials must never live in the repo)")
    hub = Hub(os.environ.get("HUB_PARSE_URL", "http://127.0.0.1:1337"),
              appid, master)
    rules = DEFAULT_RULES + [r.strip() for r in args.extra.split(",") if r.strip()]
    report = seed(hub, args.admin, rules, probe_only=args.probe)
    print(json.dumps(report, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
