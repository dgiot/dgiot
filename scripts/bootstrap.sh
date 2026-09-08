#!/usr/bin/env bash
# bootstrap.sh -- one-click setup of build prerequisites for dgiot/dgiot
#
# Installs Erlang/OTP 24.x plus basic build tools so that a plain `make`
# works afterwards. rebar3 itself is fetched automatically by the Makefile
# (scripts/ensure-rebar3.sh), so it is not handled here.
#
# Supported systems:
#   Ubuntu 20.04/22.04/24.04 (exact OTP 24.3.4.17 prebuilt from hex.pm builds)
#   Debian (distribution packages, version may differ)
#   CentOS / RHEL / Rocky / AlmaLinux / openEuler / Fedora (dnf/yum)
#   macOS (Homebrew)
#
# NOTE: keep this file ASCII-only. Non-ASCII comments have been corrupted
# by codepage-mismatched tooling before -- do not reintroduce them.

set -euo pipefail

OTP_MAJOR_REQUIRED=24
HEX_OTP_VERSION="24.3.4.17"   # exact prebuilt OTP fetched from builds.hex.pm

log()  { printf '\033[1;32m==>\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m[warn]\033[0m %s\n' "$*"; }
die()  { printf '\033[1;31m[error]\033[0m %s\n' "$*"; exit 1; }

# run a command as root when not already root
sudo_sh() {
  if [ "$(id -u)" -eq 0 ]; then "$@"; else sudo "$@"; fi
}

# print the installed OTP major version, or 0 when erl is unavailable
otp_major() {
  if ! command -v erl >/dev/null 2>&1; then
    echo 0
    return 0
  fi
  local v
  v=$(erl -noshell -eval 'io:format("~s~n",[erlang:system_info(otp_release)]),halt().' 2>/dev/null || echo 0)
  echo "${v:-0}" | cut -d. -f1
}

# try to fetch the hex.pm prebuilt OTP build; returns 1 when unavailable
try_hex_build() {
  local distro="$1" arch url
  arch=$(uname -m)
  case "$arch" in
    x86_64)  arch=amd64 ;;
    aarch64) arch=arm64 ;;
    *) return 1 ;;
  esac
  url="https://builds.hex.pm/builds/otp/${arch}/${distro}/OTP-${HEX_OTP_VERSION}.tar.gz"
  command -v curl >/dev/null 2>&1 || return 1
  curl -fsSLI "$url" >/dev/null 2>&1 || return 1
  log "Downloading prebuilt Erlang/OTP ${HEX_OTP_VERSION} (${distro}/${arch}) from builds.hex.pm"
  sudo_sh mkdir -p /usr/local/lib/erlang
  curl -fsSL "$url" | sudo_sh tar -xz -C /usr/local/lib/erlang --strip-components=1
  sudo_sh ln -sf /usr/local/lib/erlang/bin/erl /usr/local/bin/erl
  sudo_sh ln -sf /usr/local/lib/erlang/bin/escript /usr/local/bin/escript
  return 0
}

install_ubuntu_like() {
  if try_hex_build "ubuntu-20.04" || try_hex_build "ubuntu-22.04" || try_hex_build "ubuntu-24.04"; then
    return 0
  fi
  warn "No exact prebuilt OTP matched -- falling back to distribution packages"
  sudo_sh apt-get update -y
  sudo_sh apt-get install -y erlang-nox build-essential git curl
}

install_debian_like() {
  sudo_sh apt-get update -y
  sudo_sh apt-get install -y erlang-nox build-essential git curl
}

install_rhel_like() {
  local pkg
  if command -v dnf >/dev/null 2>&1; then pkg=dnf; else pkg=yum; fi
  sudo_sh "$pkg" install -y erlang git gcc gcc-c++ make curl || {
    sudo_sh "$pkg" install -y epel-release || true
    sudo_sh "$pkg" install -y erlang git gcc gcc-c++ make curl
  }
}

install_macos() {
  command -v brew >/dev/null 2>&1 || die "Homebrew not found -- install it from https://brew.sh first"
  brew install erlang@24 2>/dev/null || brew install erlang
}

# ---------------------------------------------------------------- main

major=$(otp_major)
if [ "$major" = "$OTP_MAJOR_REQUIRED" ]; then
  log "Erlang/OTP $(erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().') already installed -- nothing to do"
  exit 0
fi

os_id=""
if [ "$(uname)" = "Darwin" ]; then
  os_id="macos"
elif [ -r /etc/os-release ]; then
  # shellcheck disable=SC1091
  . /etc/os-release
  os_id="${ID:-}"
fi

case "$os_id" in
  ubuntu|pop|neon)        install_ubuntu_like ;;
  debian|deepin|kylin)    install_debian_like ;;
  centos|rhel|rocky|almalinux|fedora|openeuler|anolis)
                          install_rhel_like ;;
  macos)                  install_macos ;;
  "")
    die "Unsupported system -- install Erlang/OTP 24.3 manually (https://www.erlang.org/downloads) and ensure 'erl' is on PATH" ;;
  *)
    warn "Unrecognized distro '${os_id}' -- trying RHEL-style packages"
    install_rhel_like ;;
esac

major=$(otp_major)
case "$major" in
  "$OTP_MAJOR_REQUIRED")
    log "Erlang/OTP $(erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().') is ready" ;;
  0)
    die "erl is still not on PATH -- install Erlang/OTP 24.3 manually (https://www.erlang.org/downloads)" ;;
  *)
    warn "Erlang/OTP ${major} installed, but the project targets 24.x -- the build may still work; otherwise install 24.3 from https://www.erlang.org/downloads" ;;
esac

log "Bootstrap done. Next step: make"
