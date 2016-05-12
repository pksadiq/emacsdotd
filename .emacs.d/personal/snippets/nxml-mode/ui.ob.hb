# -*- mode: snippet; require-final-newline: nil -*-
# name: <object> GtkHeaderBar
# key: ob.hb
# --
<object class="GtkHeaderBar" id="${1:header$(replace-regexp-in-string " " "_" yas-text)}">
<property name="visible">${2:true}</property>
<property name="show-close-button">${3:true}</property>${0:$$(read-only-mode)}
</object>