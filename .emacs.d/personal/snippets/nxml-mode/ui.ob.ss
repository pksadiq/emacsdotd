# -*- mode: snippet; require-final-newline: nil -*-
# name: <object> GtkStackSwitcher
# key: ob.ss
# --
<object class="GtkStackSwitcher" id="${1:stacks$(replace-regexp-in-string " " "_" yas-text)}">
<property name="visible">${2:true}</property>
<property name="stack">${4:stack$(replace-regexp-in-string " " "_" yas-text)}</property>${0:$$(read-only-mode)}
</object>