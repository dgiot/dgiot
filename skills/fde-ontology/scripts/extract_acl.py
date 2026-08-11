#!/usr/bin/env python3
"""Extract ACL rules from dgiot source + Parse schema"""
import json, sys

def extract_acl():
    return {
        "objectid_encoding": {
            "algorithm": "MD5(ClassName + keyFields) -> first 10 hex chars",
            "properties": {
                "deterministic": True,
                "collision_free": True,  # ClassName prefix prevents cross-class collision
                "addressable": True,     # No DB lookup needed
                "length": 10,            # hex chars (16^10 = 1 trillion)
            },
            "examples": {
                "Device":     'md5("Device" + ProductId + DevAddr)',
                "Product":    'md5("Product" + CategoryId + DevType + Name)',
                "Channel":    'md5("Channel" + Type + CType + Name)',
                "_Role":      'md5("_Role" + Name)',
                "_User":      'md5("_User" + Username)',
                "Permission": 'md5("Permission" + Name)',
                "Menu":       'md5("Menu" + Name)',
            }
        },
        "tdengine_addressing": {
            "database":  "_{ChannelId}",
            "table":     "_{ProductId}",
            "format":    "{DB}.{Table}",
            "devaddr_tag": "NCHAR(50) mandatory",
        },
        "mqtt_acl": {
            "layers": [
                {
                    "name": "Device",
                    "clientId": "{ProductID}_{DevAddr}",
                    "username": "{ProductID}",
                    "password": "ProductSecret | DeviceSecret",
                    "topics": ["$dg/thing/{PID}/...", "$dg/device/{PID}/{DevAddr}/..."]
                },
                {
                    "name": "User",
                    "clientId": "{SessionToken}{Type}",
                    "auth": "Token -> Session -> Roles -> check_device_acl",
                    "topics": ["$dg/user/{DevID}/...", "$dg/device/{PID}/..."]
                },
                {
                    "name": "Superuser",
                    "clientId": "dgiot (127.0.0.1 bypass)",
                    "topics": ["*"]
                }
            ],
            "check_device_acl": "Token -> get_session -> UserId + Roles -> childrole(recursive) -> intersect(DeviceRoleIds) -> allow/deny"
        },
        "parse_acl": {
            "clp": {
                "location": "_SCHEMA table",
                "format": "{find:{role:perm}, create:{role:perm}, ...}",
                "example": "{find:{\"*\":true}, create:{\"role:root\":true}}"
            },
            "object_acl": {
                "location": "ACL JSONB field on each object",
                "format": "{\"*\":{\"read\":true}, \"role:X\":{\"write\":true}}"
            },
            "role_hierarchy": {
                "tables": ["_Role", "_Join:users:_Role", "_Join:roles:_Role"],
                "function": "dgiot_role:childrole(RoleIds, [])",
                "counts": {"users_roles": 399, "roles_roles": 298, "rules_roles": 85497}
            }
        }
    }

if __name__ == '__main__':
    result = extract_acl()
    print(json.dumps(result, indent=2, ensure_ascii=False))
