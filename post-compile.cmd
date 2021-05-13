@echo off
:: cd %REBAR_BUILD_DIR%

:: set REBAR_BUILD_DIR=c:/Users/Gilbe/emqx/emqx_rel/_build/emqx (for debug)

rmdir /s/q "%REBAR_BUILD_DIR%\conf"
mkdir "%REBAR_BUILD_DIR%\conf\plugins"
mkdir "%REBAR_BUILD_DIR%\conf\schema"

pushd "%REBAR_BUILD_DIR%"


for /d %%i in ("lib\emqx*") do call :conf %%i

for /d %%i in ("lib\emqx*") do call :schema %%i

exit 0

:conf
pushd %1
for %%f in ("etc\*.conf") do (
    :: echo %%f
    if "emqx" == "%%~nf" (
        copy %%f "%REBAR_BUILD_DIR%\conf\"
    ) else (
        if "acl" == "%%~nf" (
            copy %%f "%REBAR_BUILD_DIR%\conf\"
        ) else ( 
            if "ssl_dist" == "%%~nf" (
                copy %%f "%REBAR_BUILD_DIR%\conf\"
            ) else copy %%f "%REBAR_BUILD_DIR%\conf\plugins\"
        )
    )
)
popd
:end

:schema
pushd %1
copy priv\emqx.schema "%REBAR_BUILD_DIR%\conf\schema\"
popd
:end

