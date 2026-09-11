%% @doc 同名承接：emqx_tls_lib（TLS 版本/套件）→ 返回默认安全列表。
%% 本项目不自建 TLS 监听（TLS 由部署层/反向代理承担），这里提供 API 兼容。
-module(emqx_tls_lib).

-export([integral_versions/1, integral_ciphers/2, default_versions/0,
         default_ciphers/0, versions/1]).

default_versions() -> ['tlsv1.3', 'tlsv1.2'].
default_ciphers() ->
    ["ECDHE-ECDSA-AES256-GCM-SHA384", "ECDHE-RSA-AES256-GCM-SHA384",
     "ECDHE-ECDSA-AES128-GCM-SHA256", "ECDHE-RSA-AES128-GCM-SHA256"].

integral_versions(undefined) -> default_versions();
integral_versions(Vs) when is_list(Vs) -> Vs;
integral_versions(_) -> default_versions().

integral_ciphers(_Version, undefined) -> default_ciphers();
integral_ciphers(_Version, Cs) when is_list(Cs) -> Cs;
integral_ciphers(_Version, _) -> default_ciphers().

versions(_Opts) -> default_versions().
