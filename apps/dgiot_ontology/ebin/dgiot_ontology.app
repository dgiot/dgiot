{application, dgiot_ontology,
    [{description, "DGIOT Ontology — OWL本体语义引擎 (物模型→本体→规则→执行)"},
        {vsn, "0.1.0"},
        {modules, []},
        {registered, [dgiot_ontology_registry, dgiot_ontology_sup]},
        {applications, [kernel, stdlib, dgiot, dgiot_device]},
        {mod, {dgiot_ontology_app, []}},
        {licenses, ["Apache-2.0"]},
        {maintainers, ["DGIOT Team"]},
        {links, [{"Homepage", "https://www.dgiotcloud.cn/"},
            {"Github", "https://github.com/dgiot/dgiot"}
        ]}
    ]
}.
