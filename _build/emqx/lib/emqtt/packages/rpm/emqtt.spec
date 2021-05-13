%define debug_package %{nil}
%define _user %{_name}
%define _group %{_name}
%define _lib_home /usr/lib/%{_name}
%define _build_name_fmt %{_arch}/%{_name}%{?_ostype}-v%{_version}-%{_release}.%{_arch}.rpm

Name: %{_package_name}
Version: %{_version}
Release: %{_release}%{?dist}
Summary: emqtt
Group: System Environment/Daemons
License: Apache License Version 2.0
URL: https://www.emqx.io
BuildRoot: %{_tmppath}/%{_name}-%{_version}-root
Provides: %{_name}

%description
Erlang MQTT v5.0 Client compatible with MQTT v3.0.

%prep

%build

%install
if [-d %{buildroot}]; then
   rm -rf %{buildroot}
fi
mkdir -p %{buildroot}%{_lib_home}
cp -r %{_builddir}/%{_name}/* %{buildroot}%{_lib_home}/

%pre

%post
if [ $1 = 1 ]; then
    ln -s %{_lib_home}/bin/emqtt %{_bindir}/emqtt
    ln -s %{_lib_home}/bin/emqtt_cli %{_bindir}/emqtt_cli
fi

%preun
if [ $1 = 0 ]; then
    rm -f %{_bindir}/emqtt
    rm -f %{_bindir}/emqtt_cli
fi
exit 0

%files
%defattr(-,root,root)
%{_lib_home}

%clean
rm -rf %{buildroot}

%changelog

