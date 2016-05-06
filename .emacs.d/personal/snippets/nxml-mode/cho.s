# -*- mode: snippet; require-final-newline: nil -*-
# name: <choices> <choice...>
# key: cho.s
# --
<choices>
<choice value="${1:$$(replace-regexp-in-string " " "-" yas-text)}"/>
<choice value="${2:$$(replace-regexp-in-string " " "-" yas-text)}"/>$0
</choices>