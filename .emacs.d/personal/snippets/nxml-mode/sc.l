# -*- mode: snippet; require-final-newline: nil -*-
# name: <schemalist>...
# key: sc.l
# --
<schemalist gettext-domain="${1:$$(replace-regexp-in-string " " "-" yas-text)}">
$0
</schemalist>