#!/bin/sh -e
#
# Load early-init.el and init.el in batch mode to check the configuration
# starts cleanly.

usage () {
    cat <<EOF
Usage: $0 [-k|--insecure-tls]

  -k, --insecure-tls  Don't verify TLS certificates when contacting package
                      archives.  Needed behind a TLS-intercepting proxy whose
                      CA GnuTLS doesn't know about.  Equivalent to setting
                      INSECURE_TLS=1 in the environment.
  -h, --help          Show this message.

Set EMACS to test a binary other than the one on PATH.
EOF
}

insecure_tls=${INSECURE_TLS:-0}

while [ $# -gt 0 ]; do
    case "$1" in
        -k|--insecure-tls) insecure_tls=1 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "$0: unknown option: $1" >&2; usage >&2; exit 2 ;;
    esac
    shift
done

if [ "$insecure_tls" = 1 ]; then
    tls_setup="(setq network-security-level 'low gnutls-verify-error nil)"
    echo "TLS certificate verification disabled for this run."
else
    tls_setup=""
fi

# Unquoted heredoc: $tls_setup is interpolated, and the elisp has no other
# shell metacharacters in it.
eval_form=$(cat <<EOF
(progn
  (defvar url-show-status)
  (let ((debug-on-error t)
        (url-show-status nil)
        (user-emacs-directory default-directory)
        (user-init-file (expand-file-name "init.el"))
        (early-init-file (expand-file-name "early-init.el"))
        (load-path (delq default-directory load-path)))
     (setq package-check-signature nil)
     $tls_setup
     (load-file early-init-file)
     (load-file user-init-file)
     (run-hooks (quote after-init-hook))))
EOF
)

log=$(mktemp "${TMPDIR:-/tmp}/emacs-startup-log.XXXXXX")
rc=$(mktemp "${TMPDIR:-/tmp}/emacs-startup-rc.XXXXXX")
trap 'rm -f "$log" "$rc"' EXIT INT TERM

echo "Attempting startup..."
# Keep the output live while still capturing it.  The status goes via a file
# because a pipeline reports tee's status, not Emacs's; and -e is off for the
# duration so that a failing Emacs doesn't kill the subshell before we record
# what it exited with.
set +e
{ "${EMACS:-emacs}" -nw --batch --eval "$eval_form" 2>&1; echo $? > "$rc"; } | tee "$log"
set -e
status=$(cat "$rc" 2>/dev/null || true)
[ -n "$status" ] || status=1

if grep -q "Could not create connection to" "$log"; then
    cat >&2 <<EOF

Emacs could not open a TLS connection to a package archive.

In batch mode Emacs can't ask whether to trust an unrecognised certificate,
so it refuses the connection instead of prompting.  That is what happens
behind a TLS-intercepting proxy: the proxy re-signs traffic with its own CA,
which macOS trusts but GnuTLS does not, because GnuTLS reads PEM bundles
rather than the system keychain.

To accept the certificate for this run:

    $0 --insecure-tls

To accept it permanently, either answer the prompt once in an interactive
Emacs (M-x package-refresh-contents), which records the decision in
network-security.eld, or add the proxy CA to gnutls-trustfiles.
EOF
fi

[ "$status" -eq 0 ] || exit "$status"

echo "Startup successful"
